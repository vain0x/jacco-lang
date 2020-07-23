use super::*;
use std::marker::PhantomData;

// スタックにおける自分の深さを持つ
#[derive(Copy, Clone)]
pub(crate) struct ParseEvent<Tag>(usize, PhantomData<Tag>);

impl<Tag> ParseEvent<Tag> {
    fn new(event: usize) -> Self {
        Self(event, PhantomData)
    }

    fn index(&self) -> usize {
        self.0
    }
}

pub(crate) struct StartEventTag;
pub(crate) struct EndEventTag;
pub(crate) struct TyTag;
pub(crate) struct PatTag;
pub(crate) struct ExprTag;
pub(crate) struct DeclTag;

pub(crate) type ParseStart<Tag = ()> = ParseEvent<(StartEventTag, Tag)>;
pub(crate) type ParseEnd<Tag = ()> = ParseEvent<(EndEventTag, Tag)>;
pub(crate) type TyStart = ParseEvent<(StartEventTag, TyTag)>;
pub(crate) type TyEnd = ParseEvent<(EndEventTag, TyTag)>;
pub(crate) type PatStart = ParseEvent<(StartEventTag, PatTag)>;
pub(crate) type PatEnd = ParseEvent<(EndEventTag, PatTag)>;
pub(crate) type ExprStart = ParseEvent<(StartEventTag, ExprTag)>;
pub(crate) type ExprEnd = ParseEvent<(EndEventTag, ExprTag)>;
pub(crate) type DeclStart = ParseEvent<(StartEventTag, DeclTag)>;
pub(crate) type DeclEnd = ParseEvent<(EndEventTag, DeclTag)>;

pub(crate) struct PElementBuilder {
    start: usize,
    end_opt: Option<(PElementKind, usize)>,
    children: Vec<Option<PNodeBuilder>>,
}

impl PElementBuilder {
    fn new(start: usize) -> Self {
        Self {
            start,
            end_opt: None,
            children: vec![],
        }
    }

    fn new_with_children(start: usize, children: Vec<Option<PNodeBuilder>>) -> Self {
        Self {
            start,
            end_opt: None,
            children,
        }
    }

    fn end(&mut self, kind: PElementKind, end: usize) {
        assert_eq!(self.end_opt, None);

        self.end_opt = Some((kind, end));
    }

    pub(crate) fn finish(self, arena: &mut PElementArena) -> PElement {
        let (kind, _end) = self.end_opt.unwrap();
        let mut children = vec![];

        for child_opt in self.children {
            match child_opt {
                None => continue,
                Some(PNodeBuilder::Token(token)) => {
                    children.push(PNode::Token(token));
                }
                Some(PNodeBuilder::Element(element)) => {
                    let element = element.finish(arena);
                    children.push(PNode::Element(element));
                }
            }
        }

        let mut element = PElementData::new(kind);
        *element.children_mut() = children;
        arena.alloc(element)
    }
}

pub(crate) enum PNodeBuilder {
    Token(PToken),
    Element(PElementBuilder),
}

impl PNodeBuilder {
    fn is_token(&self) -> bool {
        match self {
            PNodeBuilder::Token(_) => true,
            _ => false,
        }
    }

    fn as_element(&self) -> Option<&PElementBuilder> {
        match self {
            PNodeBuilder::Element(element) => Some(element),
            _ => None,
        }
    }
}

pub(crate) struct PTreeBuilder {
    stack: Vec<PNodeBuilder>,
}

impl PTreeBuilder {
    pub(crate) fn new() -> Self {
        PTreeBuilder { stack: vec![] }
    }

    // 終了していないノードは除去する
    fn split_off(&mut self, start: usize) -> Vec<Option<PNodeBuilder>> {
        let mut nodes = vec![];

        for node in self.stack.drain(start..) {
            match node {
                PNodeBuilder::Token(_) => nodes.push(Some(node)),
                PNodeBuilder::Element(element) if element.end_opt.is_some() => {
                    nodes.push(Some(PNodeBuilder::Element(element)))
                }
                PNodeBuilder::Element(element) => {
                    nodes.extend(element.children.into_iter());
                }
            }
        }

        nodes
    }

    fn alloc<Tag>(&mut self, node: PNodeBuilder) -> ParseEvent<Tag> {
        let event = ParseEvent::new(self.stack.len());
        self.stack.push(node);
        event
    }

    pub(crate) fn start_element<Tag>(
        &mut self,
        current: usize,
    ) -> ParseEvent<(StartEventTag, Tag)> {
        self.alloc(PNodeBuilder::Element(PElementBuilder::new(current)))
    }

    // 直前に終了したノード child を最初の子要素として持つ新しいノードを作る
    pub(crate) fn start_parent<ChildTag, Tag>(
        &mut self,
        child: &ParseEvent<(EndEventTag, ChildTag)>,
    ) -> ParseEvent<(StartEventTag, Tag)> {
        // 注意: (x) |> f() のような式のとき、child は (x) ではなく x を指すので、
        //      スタックに置かれているはずの (x) に対応する要素を探す必要がある。
        let mut i = child.index().min(self.stack.len() - 1);
        while self.stack[i].is_token() {
            i -= 1;
        }

        let start = self
            .stack
            .get(i)
            .and_then(|node| node.as_element())
            .map(|element| element.start)
            .unwrap();
        let children = self.split_off(i);

        self.alloc(PNodeBuilder::Element(PElementBuilder::new_with_children(
            start, children,
        )))
    }

    pub(crate) fn end_element<Tag>(
        &mut self,
        kind: PElementKind,
        #[allow(unused)] start: ParseStart<Tag>,
        current: usize,
    ) -> ParseEvent<(EndEventTag, Tag)> {
        let children = self.split_off(start.index() + 1);

        let mut element = match self.stack.pop() {
            Some(PNodeBuilder::Element(element)) => element,
            _ => unreachable!(),
        };
        element.children.extend(children);
        element.end(kind, current);

        self.alloc(PNodeBuilder::Element(element))
    }

    pub(crate) fn on_token(&mut self, token: PToken) {
        self.stack.push(PNodeBuilder::Token(token));
    }

    pub(crate) fn finish(mut self, arena: &mut PElementArena) -> PElement {
        let children = self.split_off(0);
        let eof = match children.last() {
            Some(Some(PNodeBuilder::Token(token))) => *token,
            _ => unreachable!(),
        };

        let mut root = PElementBuilder::new_with_children(0, children);
        root.end(PElementKind::RootDecl, eof.to_index());
        root.finish(arena)
    }
}
