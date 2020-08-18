use self::map_stack::MapStack;
use crate::cps::{KLocalValue, KTy};

pub(crate) struct Env {
    ty_env: MapStack<KTy>,
    value_env: MapStack<KLocalValue>,
}

impl Env {
    pub(crate) const fn new() -> Self {
        Env {
            ty_env: MapStack::new(),
            value_env: MapStack::new(),
        }
    }

    pub(crate) fn enter_scope(&mut self) {
        self.ty_env.push();
        self.value_env.push();
    }

    pub(crate) fn leave_scope(&mut self) {
        self.ty_env.pop();
        self.value_env.pop();
    }

    pub(crate) fn find_ty<'a>(&'a self, name: &str) -> Option<&'a KTy> {
        self.ty_env.get(name)
    }

    pub(crate) fn find_value(&self, name: &str) -> Option<KLocalValue> {
        self.value_env.get(name).copied()
    }

    pub(crate) fn insert_ty(&mut self, name: String, ty: KTy) {
        self.ty_env.insert(name, ty);
    }

    pub(crate) fn insert_value(&mut self, name: String, value: KLocalValue) {
        self.value_env.insert(name, value);
    }
}

mod map_stack {
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
}
