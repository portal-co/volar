use super::*;

impl<T: Clone> Clone for VopeDyn<T> {
    fn clone(&self) -> Self {
        VopeDyn {
            u: self.u.iter().map(|r| r.iter().cloned().collect()).collect(),
            v: self.v.iter().cloned().collect(),
        }
    }
}
impl<T: Clone> Clone for QDyn<T> {
    fn clone(&self) -> Self {
        QDyn { q: self.q.iter().cloned().collect() }
    }
}
impl<T: Clone> Clone for DeltaDyn<T> {
    fn clone(&self) -> Self {
        DeltaDyn { delta: self.delta.iter().cloned().collect() }
    }
}

impl<T: PartialEq> PartialEq for VopeDyn<T> {
    fn eq(&self, other: &Self) -> bool {
        if self.u.len() != other.u.len() { return false; }
        if self.v.len() != other.v.len() { return false; }
        for l in 0..self.u.len() {
            for i in 0..self.u[l].len() {
                if self.u[l][i] != other.u[l][i] { return false; }
            }
        }
        for i in 0..self.v.len() {
            if self.v[i] != other.v[i] { return false; }
        }
        true
    }
}
impl<T: PartialEq> PartialEq for QDyn<T> {
    fn eq(&self, other: &Self) -> bool {
        if self.q.len() != other.q.len() { return false; }
        for i in 0..self.q.len() { if self.q[i] != other.q[i] { return false; } }
        true
    }
}
impl<T: PartialEq> PartialEq for DeltaDyn<T> {
    fn eq(&self, other: &Self) -> bool {
        if self.delta.len() != other.delta.len() { return false; }
        for i in 0..self.delta.len() { if self.delta[i] != other.delta[i] { return false; } }
        true
    }
}

impl<T: Eq> Eq for VopeDyn<T> {}
impl<T: Eq> Eq for QDyn<T> {}
impl<T: Eq> Eq for DeltaDyn<T> {}
