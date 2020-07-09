use std::{
    fmt::{self, Debug, Formatter},
    mem::size_of,
    num::NonZeroU32,
};

/// 配列のインデックスを表す整数値。
///
/// ## 意図
///
/// 64ビット環境で usize は8バイトだが、要素数が 2^32 (約40億) に至らないのであれば、インデックスは4バイトで十分。
/// `RawId` および `Option<RawId>` は4バイトに収まって、メモリの節約になる。
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub(crate) struct RawId(NonZeroU32);

impl RawId {
    pub(super) unsafe fn new_unchecked(value: u32) -> Self {
        Self(unsafe { NonZeroU32::new_unchecked(value) })
    }

    pub(super) const fn inner(self) -> NonZeroU32 {
        self.0
    }

    pub(crate) const fn from_index(index: usize) -> Self {
        let id: u32 = (index + 1) as u32;
        let id: NonZeroU32 = unsafe { NonZeroU32::new_unchecked(id) };
        Self(id)
    }

    pub(crate) const fn to_index(self) -> usize {
        (self.0.get() - 1) as usize
    }

    #[allow(unused)]
    pub(crate) const fn add_offset(self, offset: usize) -> RawId {
        let id = self.0.get() + offset as u32;
        let id = unsafe { NonZeroU32::new_unchecked(id) };
        Self(id)
    }
}

impl From<usize> for RawId {
    fn from(index: usize) -> Self {
        RawId::from_index(index)
    }
}

impl From<RawId> for usize {
    fn from(id: RawId) -> usize {
        id.to_index()
    }
}

impl Debug for RawId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.to_index(), f)
    }
}

#[allow(dead_code)]
const SIZE_OF_RAW_ID_OPTION_IS_4_BYTE: [(); 0] = [(); 4 - size_of::<Option<RawId>>()];
