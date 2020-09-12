pub(crate) mod map_stack {
    use std::collections::HashMap;

    pub(crate) struct MapStack<T> {
        inner: Vec<HashMap<String, T>>,
    }

    impl<T> MapStack<T> {
        pub(crate) const fn new() -> Self {
            Self { inner: Vec::new() }
        }

        pub(crate) fn get<'a>(&'a self, name: &str) -> Option<&'a T> {
            self.inner.iter().rev().find_map(|map| map.get(name))
        }

        pub(crate) fn push(&mut self) {
            self.inner.push(HashMap::new());
        }

        pub(crate) fn pop(&mut self) {
            self.inner.pop();
        }

        pub(crate) fn insert(&mut self, name: String, value: T) -> Option<T> {
            self.inner.last_mut().unwrap().insert(name, value)
        }
    }

    impl<T> Default for MapStack<T> {
        fn default() -> Self {
            Self::new()
        }
    }
}
