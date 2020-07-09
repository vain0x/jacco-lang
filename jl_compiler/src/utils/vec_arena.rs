use super::RawId;
use std::{
    fmt::{self, Debug, Formatter},
    ops::{Index, IndexMut},
};

#[derive(Clone)]
pub(crate) struct VecArena<T> {
    inner: Vec<T>,
}

impl<T> VecArena<T> {
    pub(crate) const fn new() -> Self {
        Self { inner: vec![] }
    }

    pub(crate) fn len(&self) -> usize {
        self.inner.len()
    }

    pub(crate) fn has_raw(&self, id: RawId) -> bool {
        id.to_index() < self.inner.len()
    }

    pub(crate) fn alloc_raw(&mut self, value: T) -> RawId {
        let id = RawId::from_index(self.inner.len());
        self.inner.push(value);
        id
    }

    pub(crate) fn keys_raw(&self) -> impl Iterator<Item = RawId> {
        (1..=self.inner.len() as u32).map(|id| unsafe { RawId::new_unchecked(id) })
    }

    pub(crate) fn enumerate_raw(&self) -> impl Iterator<Item = (RawId, &T)> {
        self.keys_raw().zip(&self.inner)
    }
}

impl<T: Debug> Debug for VecArena<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.enumerate_raw()).finish()
    }
}

impl<T> Default for VecArena<T> {
    fn default() -> Self {
        VecArena::new()
    }
}

impl<T> Index<RawId> for VecArena<T> {
    type Output = T;

    fn index(&self, index: RawId) -> &T {
        &self.inner[index.to_index()]
    }
}

impl<T> IndexMut<RawId> for VecArena<T> {
    fn index_mut(&mut self, index: RawId) -> &mut T {
        &mut self.inner[index.to_index()]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_debug() {
        let mut arena = VecArena::new();
        arena.alloc_raw("Alice");
        arena.alloc_raw("Bob");
        arena.alloc_raw("Catherine");
        arena.alloc_raw("Dave");

        assert_eq!(
            format!("{:?}", arena),
            r#"{0: "Alice", 1: "Bob", 2: "Catherine", 3: "Dave"}"#
        );
    }
}
