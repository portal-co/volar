use super::*;

// Dynamic equivalents for the field_rotate utilities in the spec.
// Provide bit-rotation helpers for QDyn and DeltaDyn and expose a `vope`
// submodule for Vope-specific rotations.

pub mod vope;

impl QDyn<u8> {
    pub fn rotate_left_bits(&self, n: usize) -> QDyn<u8> {
        let q = &self.q;
        let len = q.len();
        let mut out = Vec::with_capacity(len);
        for i in 0..len {
            let b = q[i];
            let next = q[(i + 1) % len];
            out.push(((b << (n as u8)) | (next >> (8 - n as u8))));
        }
        QDyn { q: out }
    }
    pub fn rotate_right_bits(&self, n: usize) -> QDyn<u8> {
        let q = &self.q;
        let len = q.len();
        let mut out = Vec::with_capacity(len);
        for i in 0..len {
            let prev = q[(i + len - 1) % len];
            let b = q[i];
            out.push(((prev << (8 - n as u8)) | (b >> (n as u8))));
        }
        QDyn { q: out }
    }
    pub fn bit(&self, n: u8) -> QDyn<bool> {
        let q = &self.q;
        let len = q.len();
        let mut out = Vec::with_capacity(len);
        for i in 0..len {
            out.push(((q[i] >> n) & 1) != 0);
        }
        QDyn { q: out }
    }
}

impl DeltaDyn<u8> {
    pub fn rotate_left_bits(&self, n: usize) -> DeltaDyn<u8> {
        let d = &self.delta;
        let len = d.len();
        let mut out = Vec::with_capacity(len);
        for i in 0..len {
            let b = d[i];
            let next = d[(i + 1) % len];
            out.push(((b << (n as u8)) | (next >> (8 - n as u8))));
        }
        DeltaDyn { delta: out }
    }
    pub fn rotate_right_bits(&self, n: usize) -> DeltaDyn<u8> {
        let d = &self.delta;
        let len = d.len();
        let mut out = Vec::with_capacity(len);
        for i in 0..len {
            let prev = d[(i + len - 1) % len];
            let b = d[i];
            out.push(((prev << (8 - n as u8)) | (b >> (n as u8))));
        }
        DeltaDyn { delta: out }
    }
    pub fn bit(&self, n: u8) -> DeltaDyn<bool> {
        let d = &self.delta;
        let len = d.len();
        let mut out = Vec::with_capacity(len);
        for i in 0..len {
            out.push(((d[i] >> n) & 1) != 0);
        }
        DeltaDyn { delta: out }
    }
}
