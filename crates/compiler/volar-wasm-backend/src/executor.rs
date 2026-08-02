//! Pluggable background-work abstraction for encoding finished functions.
//!
//! `WasmBackend` hands each function's recorded call log to
//! [`WasmExecutor::spawn`] as soon as its `end_function` fires, so the actual
//! `wasm_encoder` byte-level encoding of a large function can run on a worker
//! thread while the driver keeps lowering the next one. Nothing else in this
//! workspace has an executor/thread-pool abstraction to reuse, so this is a
//! new, minimal, dependency-free trait local to this crate.

/// Runs a boxed job, synchronously or on some other thread.
///
/// Implementations decide how/where `job` runs; `WasmBackend` only relies on
/// `spawn` eventually calling `job()` exactly once.
pub trait WasmExecutor: Send + Sync {
    fn spawn(&self, job: Box<dyn FnOnce() + Send + 'static>);
}

/// Default executor: runs the job immediately, inline, on the calling
/// thread. Zero setup -- background encoding is strictly opt-in via
/// [`crate::WasmBackend::with_executor`].
#[derive(Debug, Default, Clone, Copy)]
pub struct InlineExecutor;

impl WasmExecutor for InlineExecutor {
    fn spawn(&self, job: Box<dyn FnOnce() + Send + 'static>) {
        job();
    }
}

/// Runs each job on its own freshly spawned `std::thread`.
///
/// Simple, unbounded concurrency -- fine for modules with a moderate number
/// of functions. For very large modules, plug in a real thread pool (e.g.
/// `rayon::ThreadPool`) by implementing [`WasmExecutor`] directly.
#[derive(Debug, Default, Clone, Copy)]
pub struct ThreadPerJobExecutor;

impl WasmExecutor for ThreadPerJobExecutor {
    fn spawn(&self, job: Box<dyn FnOnce() + Send + 'static>) {
        std::thread::spawn(job);
    }
}
