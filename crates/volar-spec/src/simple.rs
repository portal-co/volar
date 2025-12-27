use super::*;
pub struct CommitmentCore<D: Digest>(GenericArray<u8, D::OutputSize>);
impl<D: Digest> AsRef<[u8]> for CommitmentCore<D> {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}
impl<D: Digest> Default for CommitmentCore<D> {
    fn default() -> Self {
        CommitmentCore(GenericArray::default())
    }
}
impl<D: Digest> Clone for CommitmentCore<D> {
    fn clone(&self) -> Self {
        CommitmentCore(self.0.clone())
    }
}
impl<D: Digest> PartialEq for CommitmentCore<D> {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_slice() == other.0.as_slice()
    }
}
impl<D: Digest> Eq for CommitmentCore<D> {}
impl<D: Digest> CommitmentCore<D> {
    pub fn commit(message: &impl AsRef<[u8]>, rand: &impl AsRef<[u8]>) -> Self {
        let mut hasher = D::new();
        hasher.update(message.as_ref());
        hasher.update(rand.as_ref());
        CommitmentCore(hasher.finalize())
    }
}
impl<D: Digest> CommitmentCore<D> {
    pub fn validate(&self, opened_message: &impl AsRef<[u8]>, opened_rand: &impl AsRef<[u8]>) -> bool {
        let recomputed: CommitmentCore<D> = CommitmentCore::commit(opened_message, opened_rand);
        &recomputed.0 == &self.0
    }
}