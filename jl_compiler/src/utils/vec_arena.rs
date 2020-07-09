use super::RawId;
use std::{
    fmt::{self, Debug, Formatter},
    ops::{Index, IndexMut},
};

/// `VecArena` の要素に使える型を表すトレイト。
pub(crate) trait VecArenaItem {
    type Id: Clone + Copy + From<RawId> + Into<RawId>;
}

/// `VecArena` の ID に使う型を表すトレイト。
/// `VecArenaItem` の実装から自動で生成されるので、これの実装を手で書く必要はない。
pub(crate) trait VecArenaId<Item>: Clone + Copy + From<RawId> + Into<RawId> + Sized
where
    Item: VecArenaItem<Id = Self>,
{
    fn of(self, arena: &VecArena<Item>) -> &Item {
        &arena[self]
    }

    fn of_mut(self, arena: &mut VecArena<Item>) -> &mut Item {
        &mut arena[self]
    }
}

impl<T: VecArenaItem> VecArenaId<T> for T::Id {}

/// `VecArena` の ID の型に以下のトレイトを実装させる。
///
/// - `From<RawId>`
/// - `Into<RawId>`
/// - `VecArenaId`
///
/// 使用例: `impl_vec_arena_id { User, UserData }`
#[macro_export]
macro_rules! impl_vec_arena_id {
    ($id_ty:ty, $data_ty:ty) => {
        impl From<$crate::utils::RawId> for $id_ty {
            fn from(id: $crate::utils::RawId) -> Self {
                Self(id)
            }
        }

        impl From<$id_ty> for $crate::utils::RawId {
            fn from(id: $id_ty) -> Self {
                id.0
            }
        }

        impl $crate::utils::vec_arena::VecArenaItem for $data_ty {
            type Id = $id_ty;
        }
    };
}

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

    pub(crate) fn iter(&self) -> impl Iterator<Item = &T> {
        self.inner.iter()
    }

    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.inner.iter_mut()
    }

    fn has_raw(&self, id: RawId) -> bool {
        id.to_index() < self.inner.len()
    }

    fn alloc_raw(&mut self, value: T) -> RawId {
        let id = RawId::from_index(self.inner.len());
        self.inner.push(value);
        id
    }

    fn get_raw(&self, id: RawId) -> &T {
        &self.inner[id.to_index()]
    }

    fn get_raw_mut(&mut self, id: RawId) -> &mut T {
        &mut self.inner[id.to_index()]
    }

    fn keys_raw(&self) -> impl Iterator<Item = RawId> {
        (1..=self.inner.len() as u32).map(|id| unsafe { RawId::new_unchecked(id) })
    }

    fn enumerate_raw(&self) -> impl Iterator<Item = (RawId, &T)> {
        self.keys_raw().zip(&self.inner)
    }
}

impl<T: VecArenaItem> VecArena<T> {
    pub(crate) fn has(&self, id: T::Id) -> bool {
        self.has_raw(id.into())
    }

    pub(crate) fn alloc(&mut self, value: T) -> T::Id {
        self.alloc_raw(value).into()
    }

    pub(crate) fn keys(&self) -> impl Iterator<Item = T::Id> {
        self.keys_raw().map(Into::into)
    }

    pub(crate) fn enumerate(&self) -> impl Iterator<Item = (T::Id, &T)> {
        self.keys().zip(&self.inner)
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

// NOTE: これと衝突するので、RawId を引数に取るインスタンスは宣言できない。
impl<T: VecArenaItem> Index<T::Id> for VecArena<T> {
    type Output = T;

    fn index(&self, index: T::Id) -> &T {
        let id: RawId = index.into();
        &self.inner[id.to_index()]
    }
}

impl<T: VecArenaItem> IndexMut<T::Id> for VecArena<T> {
    fn index_mut(&mut self, index: T::Id) -> &mut T {
        let id: RawId = index.into();
        &mut self.inner[id.to_index()]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fmt::Display;

    #[derive(Copy, Clone)]
    struct User(RawId);

    impl_vec_arena_id! { User, UserData }

    impl Debug for User {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            Debug::fmt(&self.0, f)
        }
    }

    struct UserData {
        name: &'static str,
    }

    impl Debug for UserData {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            Display::fmt(self.name, f)
        }
    }

    impl From<&'static str> for UserData {
        fn from(name: &'static str) -> Self {
            Self { name }
        }
    }

    #[test]
    fn test_typed_keys() {
        let mut users: VecArena<UserData> = VecArena::new();

        // 生成
        let alice: User = users.alloc("Alice".into());
        // 参照
        assert_eq!(alice.of(&users).name, "Alice");

        // 更新
        let bob: User = users.alloc("Bob".into());
        assert_eq!(bob.of(&users).name, "Bob");

        bob.of_mut(&mut users).name = "Brown";

        let brown = bob;
        assert_eq!(brown.of(&users).name, "Brown");

        // 列挙
        assert_eq!(
            users
                .enumerate()
                .map(|(user, data)| format!("id={:?}, name={}", user, data.name))
                .collect::<Vec<_>>()
                .join("; "),
            r#"id=0, name=Alice; id=1, name=Brown"#
        );

        // 範囲検査
        assert!(users.has(alice));
        let eve = User::from(RawId::from_index(10000));
        assert!(!users.has(eve));
    }

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
