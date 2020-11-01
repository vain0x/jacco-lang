use super::debug_with::DebugWithContext;
use std::{
    cmp::Ordering,
    fmt::{self, Debug, Formatter},
    hash::{Hash, Hasher},
    marker::PhantomData,
    mem::size_of,
    num::NonZeroU32,
    ops::{Index, IndexMut, Range},
};

// -----------------------------------------------
// ID (NonZeroU32)
// -----------------------------------------------

#[allow(unused)]
const ID_MIN: NonZeroU32 = unsafe { NonZeroU32::new_unchecked(1) };

const ID_MAX: NonZeroU32 = unsafe { NonZeroU32::new_unchecked(u32::MAX) };

const fn id_from_index(index: usize) -> NonZeroU32 {
    let value: u32 = (index + 1) as u32;
    unsafe { NonZeroU32::new_unchecked(value) }
}

const fn id_to_index(id: NonZeroU32) -> usize {
    (id.get() - 1) as usize
}

#[allow(unused)]
const fn id_add_offset(id: NonZeroU32, offset: usize) -> NonZeroU32 {
    let value: u32 = id.get() + offset as u32;
    unsafe { NonZeroU32::new_unchecked(value) }
}

fn id_iter(range: Range<NonZeroU32>) -> impl Iterator<Item = NonZeroU32> {
    (range.start.get()..range.end.get()).map(|i| unsafe { NonZeroU32::new_unchecked(i) })
}

// -----------------------------------------------
// VecArenaId
// -----------------------------------------------

/// `VecArena` の型つきのインデックスを表す。
///
/// `Tag` は値を区別するための幽霊型。
///
/// ## 意図
///
/// 型安全性: インデックスの不正な使い方を型検査により防ぐ。
///
/// サイズ: 64ビット環境で usize は8バイトだが、要素数が 2^32 (約40億) に至らないのであれば、インデックスは4バイトで十分。
/// `VecArenaId` および `Option<VecArenaId>` は4バイトに収まって、メモリの節約になる。
pub(crate) struct VecArenaId<Tag> {
    inner: NonZeroU32,
    _phantom: PhantomData<Tag>,
}

impl<Tag> VecArenaId<Tag> {
    #[allow(unused)]
    const MIN: Self = Self::from_inner(ID_MIN);

    #[allow(unused)]
    pub(crate) const MAX: Self = Self::from_inner(ID_MAX);

    #[allow(unused)]
    pub(crate) const TODO: Self = Self::from_inner(ID_MAX);

    const fn from_inner(inner: NonZeroU32) -> Self {
        Self {
            inner,
            _phantom: PhantomData,
        }
    }

    const unsafe fn new_unchecked(value: u32) -> Self {
        Self::from_inner(NonZeroU32::new_unchecked(value))
    }

    pub(crate) const fn from_index(index: usize) -> Self {
        Self::from_inner(id_from_index(index))
    }

    pub(crate) const fn to_index(self) -> usize {
        id_to_index(self.inner)
    }

    #[allow(unused)]
    pub(crate) const fn add_offset(self, offset: usize) -> VecArenaId<Tag> {
        Self::from_inner(id_add_offset(self.inner, offset))
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

// Copy + Clone
// derive(Clone) だと Tag: Clone のときしか実装されない。
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

#[allow(dead_code)]
const SIZE_OF_ID_OPTION_IS_4_BYTE: [(); 0] = [(); 4 - size_of::<Option<VecArenaId<()>>>()];

// -----------------------------------------------
// VecArena
// -----------------------------------------------

/// 型つき ID によりインデックスアクセス可能な `Vec`
pub(crate) struct VecArena<Tag, T> {
    inner: Vec<T>,
    _phantom: PhantomData<*mut Tag>,
}

impl<Tag, T> VecArena<Tag, T> {
    pub(crate) const fn from_vec(inner: Vec<T>) -> Self {
        Self {
            inner,
            _phantom: PhantomData,
        }
    }

    pub(crate) fn from_iter(iter: impl IntoIterator<Item = T>) -> Self {
        Self::from_vec(iter.into_iter().collect())
    }

    pub(crate) const fn new() -> Self {
        Self::from_vec(vec![])
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub(crate) fn len(&self) -> usize {
        self.inner.len()
    }

    fn next_id(&self) -> VecArenaId<Tag> {
        id_from_index(self.len()).into()
    }

    pub(crate) fn reserve(&mut self, additional: usize) {
        self.inner.reserve(additional);
    }

    #[allow(unused)]
    pub(crate) fn resize_with(&mut self, new_len: usize, default_fn: impl Fn() -> T) {
        let additional = new_len.saturating_sub(self.inner.len());
        self.inner.reserve_exact(additional);
        self.inner.resize_with(new_len, default_fn);
    }

    #[allow(unused)]
    pub(crate) fn slice(&self) -> VecArenaSlice<Tag> {
        let start = VecArenaId::MIN;
        let end = self.next_id();
        VecArenaSlice(Range { start, end })
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &T> {
        self.inner.iter()
    }

    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.inner.iter_mut()
    }
}

impl<Tag, T> VecArena<Tag, T> {
    #[allow(unused)]
    pub(crate) fn has(&self, id: VecArenaId<Tag>) -> bool {
        id.to_index() < self.inner.len()
    }

    /// 要素への参照から ID を逆算する。
    #[allow(unused)]
    pub(crate) fn id_from_ref<'a>(&'a self, value: &'a T) -> Option<VecArenaId<Tag>> {
        assert_ne!(std::mem::size_of::<T>(), 0);

        // ptr < start のとき (ptr - start) は巨大な数になるので、結果として None を返す。
        let i = {
            let start = self.inner.as_ptr();
            let ptr = value as *const T;
            (ptr as usize).wrapping_sub(start as usize) / std::mem::size_of::<T>()
        };

        if i < self.len() {
            Some(VecArenaId::from_index(i))
        } else {
            None
        }
    }

    pub(crate) fn alloc(&mut self, value: T) -> VecArenaId<Tag> {
        let id = self.next_id();
        self.inner.push(value);
        id
    }

    pub(crate) fn alloc_slice(&mut self, items: impl IntoIterator<Item = T>) -> VecArenaSlice<Tag> {
        let start = self.next_id();
        self.inner.extend(items.into_iter());
        let end = self.next_id();
        VecArenaSlice(Range { start, end })
    }

    pub(crate) fn extend_with(&mut self, len: usize, default_fn: impl Fn() -> T) {
        self.resize_with(len.max(self.len()), default_fn);
    }

    #[allow(unused)]
    pub(crate) fn get(&self, id: VecArenaId<Tag>) -> Option<&T> {
        self.inner.get(id.to_index())
    }

    #[allow(unused)]
    pub(crate) fn get_mut(&mut self, id: VecArenaId<Tag>) -> Option<&mut T> {
        self.inner.get_mut(id.to_index())
    }

    pub(crate) fn into_vec(self) -> Vec<T> {
        self.inner
    }

    pub(crate) fn keys(&self) -> impl Iterator<Item = VecArenaId<Tag>> {
        (1..=self.inner.len() as u32).map(|id| unsafe { VecArenaId::new_unchecked(id) })
    }

    pub(crate) fn enumerate(&self) -> impl Iterator<Item = (VecArenaId<Tag>, &T)> {
        self.keys().zip(&self.inner)
    }

    pub(crate) fn enumerate_mut(&mut self) -> impl Iterator<Item = (VecArenaId<Tag>, &mut T)> {
        self.keys().zip(&mut self.inner)
    }
}

impl<Tag: 'static, T: 'static> VecArena<Tag, T> {
    pub(crate) const EMPTY: &'static Self = &Self {
        inner: vec![],
        _phantom: PhantomData,
    };
}

impl<Tag, T: Debug> Debug for VecArena<Tag, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.enumerate()).finish()
    }
}

// アリーナへの参照があれば ID の内容をデバッグ表示できる。
impl<'a, Tag, T> DebugWithContext<VecArena<Tag, T>> for VecArenaId<Tag>
where
    T: DebugWithContext<VecArena<Tag, T>>,
{
    fn fmt(&self, arena: &VecArena<Tag, T>, f: &mut Formatter<'_>) -> fmt::Result {
        DebugWithContext::fmt(self.of(arena), arena, f)
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

impl<Tag, T> Index<VecArenaId<Tag>> for VecArena<Tag, T> {
    type Output = T;

    fn index(&self, id: VecArenaId<Tag>) -> &T {
        let index = id.to_index();

        debug_assert!(index < self.len());
        unsafe { self.inner.get_unchecked(index) }
    }
}

impl<Tag, T> IndexMut<VecArenaId<Tag>> for VecArena<Tag, T> {
    fn index_mut(&mut self, id: VecArenaId<Tag>) -> &mut T {
        let index = id.to_index();

        debug_assert!(index < self.len());
        unsafe { self.inner.get_unchecked_mut(index) }
    }
}

pub(crate) type RawId = VecArenaId<()>;

// -----------------------------------------------
// スライス
// -----------------------------------------------

pub(crate) struct VecArenaSlice<Tag>(Range<VecArenaId<Tag>>);

impl<Tag> VecArenaSlice<Tag> {
    pub(crate) const EMPTY: Self = Self(Range {
        start: VecArenaId::MAX,
        end: VecArenaId::MAX,
    });

    pub(crate) fn len(&self) -> usize {
        let start = self.0.start;
        let end = self.0.end;
        end.to_index().saturating_sub(start.to_index())
    }

    pub(crate) fn is_last(&self, id: VecArenaId<Tag>) -> bool {
        let start = self.0.start;
        let end = self.0.end;
        start != end && id.add_offset(1) == end
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = VecArenaId<Tag>> {
        let start = self.0.start.inner;
        let end = self.0.end.inner;
        id_iter(start..end).map(VecArenaId::from_inner)
    }

    pub(crate) fn enumerate<'a, T>(
        &self,
        arena: &'a VecArena<Tag, T>,
    ) -> impl Iterator<Item = (VecArenaId<Tag>, &'a T)> {
        self.iter().zip(self.of(arena))
    }

    #[allow(unused)]
    pub(crate) fn map_with<T>(&self, f: impl Fn() -> T) -> VecArena<Tag, T> {
        let mut inner = Vec::with_capacity(self.len());
        inner.resize_with(self.len(), f);
        VecArena::from_vec(inner)
    }

    #[allow(unused)]
    pub(crate) fn map_with_value<T: Clone>(&self, value: T) -> VecArena<Tag, T> {
        let mut inner = Vec::with_capacity(self.len());
        inner.resize(self.len(), value);
        VecArena::from_vec(inner)
    }

    // 結果はスライスじゃないかもしれないが、ランダムアクセスは可能
    pub(crate) fn of<'a, T>(&self, arena: &'a VecArena<Tag, T>) -> &'a [T] {
        let (start, end) = (self.0.start, self.0.end);
        if start >= end {
            return &[];
        }

        &arena.inner[start.to_index()..end.to_index()]
    }
}

impl<Tag> Clone for VecArenaSlice<Tag> {
    fn clone(&self) -> Self {
        VecArenaSlice(self.0.clone())
    }
}

impl<Tag> Default for VecArenaSlice<Tag> {
    fn default() -> Self {
        Self::EMPTY
    }
}

// -----------------------------------------------
// テスト
// -----------------------------------------------

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
        let eve = User::from_index(10000);
        assert!(!users.has(eve));
    }

    type StrArena = VecArena<(), &'static str>;

    #[test]
    fn test_debug() {
        let mut arena = StrArena::new();
        arena.alloc("Alice");
        arena.alloc("Bob");
        arena.alloc("Catherine");
        arena.alloc("Dave");

        assert_eq!(
            format!("{:?}", arena),
            r#"{0: "Alice", 1: "Bob", 2: "Catherine", 3: "Dave"}"#
        );
    }

    #[test]
    fn test_id_from_ref() {
        let mut arena = StrArena::new();
        let alice = arena.alloc("Alice");
        let bob = arena.alloc("Bob");

        assert_eq!(arena.id_from_ref(alice.of(&arena)), Some(alice));
        assert_eq!(arena.id_from_ref(bob.of(&arena)), Some(bob));

        let cloned_alice = alice.of(&arena).clone();
        assert_eq!(arena.id_from_ref(&cloned_alice), None);
    }

    #[test]
    #[should_panic]
    fn test_id_from_ref_does_not_work_for_zero_sided_types() {
        let mut arena: VecArena<(), ()> = VecArena::new();
        let id = arena.alloc(());
        arena.id_from_ref(id.of(&arena));
    }
}
