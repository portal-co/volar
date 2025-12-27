use super::*;

use alloc::vec::Vec;

impl VopeDyn<u8> {
    pub fn rotate_left_bits(&self, n: usize) -> Self {
        let nlen = self.v.len();
        let mut u_out: Vec<Vec<u8>> = Vec::with_capacity(self.u.len());
        for l in 0..self.u.len() {
            let mut row = Vec::with_capacity(nlen);
            for i in 0..nlen {
                let b = self.u[l][i];
                let next = self.u[l][(i + 1) % nlen];
                row.push(((b << (n as u8)) | (next >> (8 - n as u8))));
            }
            u_out.push(row);
        }
        let mut v_out = Vec::with_capacity(nlen);
        for i in 0..nlen {
            let b = self.v[i];
            let next = self.v[(i + 1) % nlen];
            v_out.push(((b << (n as u8)) | (next >> (8 - n as u8))));
        }
        VopeDyn { u: u_out, v: v_out }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self {
        let nlen = self.v.len();
        let mut u_out: Vec<Vec<u8>> = Vec::with_capacity(self.u.len());
        for l in 0..self.u.len() {
            let mut row = Vec::with_capacity(nlen);
            for i in 0..nlen {
                let prev = self.u[l][(i + nlen - 1) % nlen];
                let b = self.u[l][i];
                row.push(((prev << (8 - n as u8)) | (b >> (n as u8))));
            }
            u_out.push(row);
        }
        let mut v_out = Vec::with_capacity(nlen);
        for i in 0..nlen {
            let prev = self.v[(i + nlen - 1) % nlen];
            let b = self.v[i];
            v_out.push(((prev << (8 - n as u8)) | (b >> (n as u8))));
        }
        VopeDyn { u: u_out, v: v_out }
    }
    pub fn bit(&self, n: u8) -> VopeDyn<bool> {
        let nlen = self.v.len();
        let mut u_out: Vec<Vec<bool>> = Vec::with_capacity(self.u.len());
        for l in 0..self.u.len() {
            let mut row = Vec::with_capacity(nlen);
            for i in 0..nlen {
                row.push(((self.u[l][i] >> n) & 1) != 0);
            }
            u_out.push(row);
        }
        let mut v_out = Vec::with_capacity(nlen);
        for i in 0..nlen {
            v_out.push(((self.v[i] >> n) & 1) != 0);
        }
        VopeDyn { u: u_out, v: v_out }
    }
}
