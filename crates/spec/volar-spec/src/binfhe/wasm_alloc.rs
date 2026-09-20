// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! Bounded static allocator for a no-std `wasm32v1-none` BinFHE module.
//!
//! `binfhe::plan::PlanWorkspace` and `CiphertextInputBuffer` make the plan
//! executor's allocations explicit. A wasm provider may install this allocator
//! as its one `#[global_allocator]`, allocate all keys/workspaces at module
//! initialization, then run repeated plan calls without allocation growth.
//!
//! ```ignore
//! #[global_allocator]
//! static ALLOC: StaticBumpAllocator<262_144> = StaticBumpAllocator::new();
//! ```
//!
//! The allocator deliberately never deallocates and has no reset operation.
//! Resetting a global bump pointer while `Vec`/keys/workspaces still borrow its
//! bytes would be unsound. Instead, the plan boundary resets *logical* wire,
//! RGSW, cell, LUT, and ciphertext-input arenas between calls while retaining
//! their one-time bounded allocation. A module that needs a fresh heap must be
//! re-instantiated by its host after all allocations are dead.
//!
//! # Safety
//!
//! It is a minimal `GlobalAlloc` implementation for single-threaded no-std
//! WASM. `alloc` uses atomic reservation so reentrant allocation cannot
//! overlap, but it makes no general multithreaded allocator performance claim.
//! The `wasm32v1-none` target has no host allocator fallback: an exhausted
//! buffer returns null and Rust's allocation failure behavior applies.
//!
//! See `docs/fhe/future-provider-integration-ledger.md` before attaching this
//! allocator to a concrete provider artifact.

use core::alloc::{GlobalAlloc, Layout};
use core::cell::UnsafeCell;
use core::ptr::null_mut;
use core::sync::atomic::{AtomicUsize, Ordering};

/// A statically reserved, monotonic allocation region.
///
/// `BYTES` is part of the artifact's public resource profile and must cover
/// all one-time key/workspace/input-buffer setup. Repeated BinFHE execution
/// then uses the fixed-capacity arenas rather than asking this allocator for
/// more memory.
pub struct StaticBumpAllocator<const BYTES: usize> {
    storage: UnsafeCell<[u8; BYTES]>,
    next: AtomicUsize,
}

unsafe impl<const BYTES: usize> Sync for StaticBumpAllocator<BYTES> {}

impl<const BYTES: usize> StaticBumpAllocator<BYTES> {
    /// Construct an empty static allocation region.
    pub const fn new() -> Self {
        Self {
            storage: UnsafeCell::new([0; BYTES]),
            next: AtomicUsize::new(0),
        }
    }

    /// Number of bytes permanently reserved since module initialization.
    pub fn used(&self) -> usize {
        self.next.load(Ordering::Relaxed)
    }

    /// Static heap capacity.
    pub const fn capacity(&self) -> usize {
        BYTES
    }
}

unsafe impl<const BYTES: usize> GlobalAlloc for StaticBumpAllocator<BYTES> {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let align = layout.align();
        let size = layout.size();
        let mut current = self.next.load(Ordering::Relaxed);
        loop {
            let Some(aligned) = current.checked_add(align - 1).map(|v| v & !(align - 1)) else {
                return null_mut();
            };
            let Some(end) = aligned.checked_add(size) else {
                return null_mut();
            };
            if end > BYTES {
                return null_mut();
            }
            match self.next.compare_exchange_weak(
                current,
                end,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => {
                    // SAFETY: the successful monotonic reservation assigns the
                    // non-overlapping [aligned, end) range inside `storage`.
                    return unsafe { (*self.storage.get()).as_mut_ptr().add(aligned) };
                }
                Err(observed) => current = observed,
            }
        }
    }

    unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {
        // Monotonic by design: plan/key buffers retain their capacity for the
        // lifetime of the no-std module so later calls perform no reallocation.
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reserves_aligned_non_overlapping_ranges_and_fails_closed() {
        let heap = StaticBumpAllocator::<16>::new();
        // SAFETY: raw allocations are never dereferenced; the test observes
        // only allocator range reservation.
        let first = unsafe { heap.alloc(Layout::from_size_align(3, 1).unwrap()) };
        // SAFETY: same as above.
        let second = unsafe { heap.alloc(Layout::from_size_align(4, 8).unwrap()) };
        // SAFETY: same as above; capacity is intentionally exhausted.
        let exhausted = unsafe { heap.alloc(Layout::from_size_align(9, 1).unwrap()) };
        assert!(!first.is_null());
        assert!(!second.is_null());
        assert_eq!((second as usize) % 8, 0);
        assert!(exhausted.is_null());
        assert_eq!(heap.used(), 12);
    }
}
