use super::RawId;
use std::{
    cmp::Ordering,
    fmt::{self, Debug, Formatter},
    hash::{Hash, Hasher},
    marker::PhantomData,
    num::NonZeroU32,
    ops::{Index, IndexMut},
};

/// `VecArena` の型つきの ID を表す。
///
/// `T` は値を区別するための幽霊型。
pub(crate) struct VecArenaId<Tag> {
    inner: NonZeroU32,
    _phantom: PhantomData<*mut Tag>,
}

impl<Tag> VecArenaId<Tag> {
    pub(crate) fn from_index(index: usize) -> Self {
        RawId::from_index(index).into()
    }

    pub(crate) fn to_index(self) -> usize {
        RawId::from(self).to_index()
    }

    pub(crate) fn of<T>(self, arena: &VecArena<Tag, T>) -> &T {
        &arena.inner[self.to_index()]
    }

    pub(crate) fn of_mut<T>(self, arena: &mut VecArena<Tag, T>) -> &mut T {
        &mut arena.inner[self.to_index()]
    }
}

// VecArenaId <--> NonZeroU32
impl<T> From<NonZeroU32> for VecArenaId<T> {
    fn from(inner: NonZeroU32) -> Self {
        Self {
            inner,
            _phantom: PhantomData,
        }
    }
}

impl<T> From<VecArenaId<T>> for NonZeroU32 {
    fn from(id: VecArenaId<T>) -> Self {
        id.inner
    }
}

// VecArenaId <--> RawId
impl<T> From<RawId> for VecArenaId<T> {
    fn from(raw_id: RawId) -> Self {
        Self {
            inner: raw_id.into(),
            _phantom: PhantomData,
        }
    }
}

impl<T> From<VecArenaId<T>> for RawId {
    fn from(id: VecArenaId<T>) -> RawId {
        RawId::from(id.inner)
    }
}

// Copy + Clone
impl<Tag> Clone for VecArenaId<Tag> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner,
            _phantom: PhantomData,
        }
    }
}

impl<Tag> Copy for VecArenaId<Tag> {}

impl<Tag> PartialEq for VecArenaId<Tag> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl<Tag> Eq for VecArenaId<Tag> {}

impl<Tag> PartialOrd for VecArenaId<Tag> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.inner.partial_cmp(&other.inner)
    }
}

impl<Tag> Ord for VecArenaId<Tag> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.inner.cmp(&other.inner)
    }
}

impl<Tag> Hash for VecArenaId<Tag> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(&self.inner, state)
    }
}

impl<Tag> Debug for VecArenaId<Tag> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.to_index(), f)
    }
}

pub(crate) struct VecArena<Tag, T> {
    inner: Vec<T>,
    _phantom: PhantomData<*mut Tag>,
}

impl<Tag, T> VecArena<Tag, T> {
    pub(crate) const fn new() -> Self {
        Self {
            inner: vec![],
            _phantom: PhantomData,
        }
    }

    pub(crate) const fn from_vec(inner: Vec<T>) -> Self {
        Self {
            inner,
            _phantom: PhantomData,
        }
    }

    pub(crate) fn len(&self) -> usize {
        self.inner.len()
    }

    pub(crate) fn resize_with(&mut self, new_len: usize, default_fn: impl Fn() -> T) {
        let additional = new_len.saturating_sub(self.inner.len());
        self.inner.reserve_exact(additional);
        self.inner.resize_with(new_len, default_fn);
    }

    #[allow(unused)]
    pub(crate) fn iter(&self) -> impl Iterator<Item = &T> {
        self.inner.iter()
    }

    #[allow(unused)]
    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.inner.iter_mut()
    }

    #[allow(unused)]
    fn has_raw(&self, id: RawId) -> bool {
        id.to_index() < self.inner.len()
    }

    #[allow(unused)]
    fn alloc_raw(&mut self, value: T) -> RawId {
        let id = RawId::from_index(self.inner.len());
        self.inner.push(value);
        id
    }

    #[allow(unused)]
    fn index_raw(&self, id: RawId) -> &T {
        &self.inner[id.to_index()]
    }

    #[allow(unused)]
    fn index_raw_mut(&mut self, id: RawId) -> &mut T {
        &mut self.inner[id.to_index()]
    }

    #[allow(unused)]
    fn get_raw(&self, id: RawId) -> Option<&T> {
        self.inner.get(id.to_index())
    }

    #[allow(unused)]
    fn get_raw_mut(&mut self, id: RawId) -> Option<&mut T> {
        self.inner.get_mut(id.to_index())
    }

    #[allow(unused)]
    fn keys_raw(&self) -> impl Iterator<Item = RawId> {
        (1..=self.inner.len() as u32).map(|id| unsafe { RawId::new_unchecked(id) })
    }

    fn enumerate_raw(&self) -> impl Iterator<Item = (RawId, &T)> {
        self.keys_raw().zip(&self.inner)
    }
}

impl<Tag, T> VecArena<Tag, T> {
    #[allow(unused)]
    pub(crate) fn has(&self, id: VecArenaId<Tag>) -> bool {
        self.has_raw(id.into())
    }

    #[allow(unused)]
    pub(crate) fn alloc(&mut self, value: T) -> VecArenaId<Tag> {
        self.alloc_raw(value).into()
    }

    #[allow(unused)]
    pub(crate) fn get(&self, id: VecArenaId<Tag>) -> Option<&T> {
        self.get_raw(id.into())
    }

    #[allow(unused)]
    pub(crate) fn keys(&self) -> impl Iterator<Item = VecArenaId<Tag>> {
        self.keys_raw().map(Into::into)
    }

    #[allow(unused)]
    pub(crate) fn enumerate(&self) -> impl Iterator<Item = (VecArenaId<Tag>, &T)> {
        self.keys().zip(&self.inner)
    }
}

impl<Tag, T: Debug> Debug for VecArena<Tag, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.enumerate_raw()).finish()
    }
}

impl<Tag, T> Default for VecArena<Tag, T> {
    fn default() -> Self {
        VecArena::new()
    }
}

impl<Tag, T: Clone> Clone for VecArena<Tag, T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            _phantom: PhantomData,
        }
    }
}

// NOTE: これと衝突するので、RawId を引数に取るインスタンスは宣言できない。
impl<Tag, T> Index<VecArenaId<Tag>> for VecArena<Tag, T> {
    type Output = T;

    fn index(&self, index: VecArenaId<Tag>) -> &T {
        let id: RawId = index.into();
        let index = id.to_index();

        debug_assert!(index < self.len());
        unsafe { self.inner.get_unchecked(index) }
    }
}

impl<Tag, T> IndexMut<VecArenaId<Tag>> for VecArena<Tag, T> {
    fn index_mut(&mut self, index: VecArenaId<Tag>) -> &mut T {
        let id: RawId = index.into();
        let index = id.to_index();

        debug_assert!(index < self.len());
        unsafe { self.inner.get_unchecked_mut(index) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fmt::Display;

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

    struct UserTag;

    type User = VecArenaId<UserTag>;

    type UserArena = VecArena<UserTag, UserData>;

    #[test]
    fn test_typed_keys() {
        let mut users: UserArena = UserArena::new();

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

    type StrArena = VecArena<&'static str, &'static str>;

    #[test]
    fn test_debug() {
        let mut arena = StrArena::new();
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
