use super::*;

// タプル構造体のとき $name = 0 とするために tt にしている。
#[macro_export]
macro_rules! impl_node_seq {
    ($($name:tt),+) => {
        fn len(&self) -> usize {
            let mut n = 0;
            $({
                let _ = &self.$name;
                n += 1;
            })*;
            n
        }

        fn get(&self, mut i: usize) -> Option<PElementRef> {
            $({
                if i == 0 {
                    return try_as_element_ref(&self.$name);
                }
                i -= 1;
            })*;
            debug_assert!(i == 0);
            unreachable!();
        }

        fn get_mut(&mut self, mut i: usize) -> Option<PElementMut> {
            $({
                if i == 0 {
                    return try_as_element_mut(&mut self.$name);
                }
                i -= 1;
            })*;
            debug_assert!(i == 0);
            unreachable!()
        }
    };
}

#[macro_export]
macro_rules! impl_node_choice {
    ($($variant:path),+ $(,)?) => {
        fn len(&self) -> usize {
            match self {
                $(
                    $variant(node) => PNode::len(node),
                )*
            }
        }

        fn get(&self, i: usize) -> Option<PElementRef> {
            match self {
                $(
                    $variant(node) => PNode::get(node, i),
                )*
            }
        }

        fn get_mut(&mut self, i: usize) -> Option<PElementMut> {
            match self {
                $(
                    $variant(node) => PNode::get_mut(node, i),
                )*
            }
        }
    };
}

pub(crate) trait PNode {
    fn len(&self) -> usize;

    fn get(&self, i: usize) -> Option<PElementRef>;

    fn get_mut(&mut self, i: usize) -> Option<PElementMut>;

    fn last(&self) -> PElementRef {
        assert_ne!(self.len(), 0);
        self.get(self.len() - 1).unwrap()
    }

    fn first_token(&self) -> Option<&TokenData> {
        for i in 0..self.len() {
            if let Some(token) = self.get(i).and_then(PElementRef::first_token) {
                return Some(token);
            }
        }
        None
    }

    fn last_token(&self) -> Option<&TokenData> {
        for i in (0..self.len()).rev() {
            if let Some(token) = self.get(i).and_then(PElementRef::last_token) {
                return Some(token);
            }
        }
        None
    }

    fn location(&self) -> Location {
        match (self.first_token(), self.last_token()) {
            (Some(first), Some(last)) => first.location().clone().unite(last.location()),
            _ => Location::default(),
        }
    }
}

impl<'a, N: PNode> PNode for Box<N> {
    fn len(&self) -> usize {
        N::len(self)
    }

    fn get(&self, i: usize) -> Option<PElementRef> {
        N::get(self, i)
    }

    fn get_mut(&mut self, i: usize) -> Option<PElementMut> {
        N::get_mut(self, i)
    }
}

/// 構文要素への参照の変換を提供するトレイト。
///
/// - `&TokenData` -> `Option<PElementRef>`
/// - `&impl PNode` -> `Option<PElementRef>`
/// - `&Option<impl PNode>` -> `Option<PElementRef>`
/// - `&Option<Box<impl PNode>>` -> `Option<PElementRef>`
pub(crate) trait TryAsElementRef<'a> {
    fn try_as_element_ref(self) -> Option<PElementRef<'a>>;
}

impl<'a> TryAsElementRef<'a> for &'a TokenData {
    fn try_as_element_ref(self) -> Option<PElementRef<'a>> {
        Some(PElementRef::Token(self))
    }
}

impl<'a, N: PNode> TryAsElementRef<'a> for &'a N {
    fn try_as_element_ref(self) -> Option<PElementRef<'a>> {
        Some(PElementRef::Node(self))
    }
}

impl<'a, T> TryAsElementRef<'a> for &'a Option<T>
where
    &'a T: TryAsElementRef<'a>,
{
    fn try_as_element_ref(self) -> Option<PElementRef<'a>> {
        self.as_ref().and_then(try_as_element_ref)
    }
}

/// トークンまたはノード (あるいはその Option) への可変参照を、構文要素への可変参照に変換する。
pub(crate) fn try_as_element_ref<'a, T: TryAsElementRef<'a>>(it: T) -> Option<PElementRef<'a>> {
    it.try_as_element_ref()
}

/// `TryAsElementRef` の可変版
pub(crate) trait TryAsElementMut<'a> {
    fn try_as_element_mut(self) -> Option<PElementMut<'a>>;
}

impl<'a> TryAsElementMut<'a> for &'a mut TokenData {
    fn try_as_element_mut(self) -> Option<PElementMut<'a>> {
        Some(PElementMut::Token(self))
    }
}

impl<'a, N: PNode> TryAsElementMut<'a> for &'a mut N {
    fn try_as_element_mut(self) -> Option<PElementMut<'a>> {
        Some(PElementMut::Node(self))
    }
}

impl<'a, T> TryAsElementMut<'a> for &'a mut Option<T>
where
    &'a mut T: TryAsElementMut<'a>,
{
    fn try_as_element_mut(self) -> Option<PElementMut<'a>> {
        self.as_mut().and_then(try_as_element_mut)
    }
}

pub(crate) fn try_as_element_mut<'a, T: TryAsElementMut<'a>>(it: T) -> Option<PElementMut<'a>> {
    it.try_as_element_mut()
}
