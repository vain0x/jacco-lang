use super::*;
use crate::utils::{VecArena, VecArenaId};
use std::marker::PhantomData;

pub(crate) struct EventTag;
pub(crate) type EventId = VecArenaId<EventTag>;
pub(crate) type EventArena = VecArena<EventTag, Option<PElement>>;

#[derive(Copy, Clone)]
pub(crate) struct ParseEvent<Tag> {
    id: EventId,
    /// このイベントのスタックにおける深さ
    depth: usize,
    _phantom: PhantomData<Tag>,
}

impl<Tag> ParseEvent<Tag> {
    fn new(id: EventId, depth: usize) -> Self {
        Self {
            id,
            depth,
            _phantom: PhantomData,
        }
    }

    pub(crate) fn id(&self) -> EventId {
        self.id
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

struct PElementBuilder {
    id: EventId,
    start: usize,
    end_opt: Option<(PElementKind, usize)>,
    children: Vec<Option<PNodeBuilder>>,
}

impl PElementBuilder {
    fn new(id: EventId, start: usize, children: Vec<Option<PNodeBuilder>>) -> Self {
        Self {
            id,
            start,
            end_opt: None,
            children,
        }
    }

    fn end(&mut self, kind: PElementKind, end: usize) {
        assert_eq!(self.end_opt, None);

        self.end_opt = Some((kind, end));
    }

    fn finish(self, elements: &mut PElementArena, events: &mut EventArena) -> PElement {
        let (kind, _end) = self.end_opt.unwrap();
        let mut children = vec![];

        for child_opt in self.children {
            match child_opt {
                None => continue,
                Some(PNodeBuilder::Token(token)) => {
                    children.push(PNode::Token(token));
                }
                Some(PNodeBuilder::Element(element)) => {
                    let element = element.finish(elements, events);
                    children.push(PNode::Element(element));
                }
            }
        }

        let element = elements.alloc(PElementData::new(kind, children));

        log::trace!("id={:?} events.len={}", self.id, events.len());
        let old = self.id.of_mut(events).replace(element);
        assert!(old.is_none());

        element
    }
}

enum PNodeBuilder {
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
    events: EventArena,
}

impl PTreeBuilder {
    pub(crate) fn new() -> Self {
        PTreeBuilder {
            stack: vec![],
            events: VecArena::new(),
        }
    }

    fn split_off(&mut self, start: usize) -> Vec<Option<PNodeBuilder>> {
        let mut nodes = vec![];

        for node in self.stack.drain(start..) {
            match node {
                PNodeBuilder::Token(_) => nodes.push(Some(node)),
                PNodeBuilder::Element(element) if element.end_opt.is_some() => {
                    nodes.push(Some(PNodeBuilder::Element(element)))
                }
                PNodeBuilder::Element(element) => {
                    // 終了していないノードは flatten する。
                    nodes.extend(element.children.into_iter());
                }
            }
        }

        nodes
    }

    fn push<Tag>(&mut self, element: PElementBuilder) -> ParseEvent<Tag> {
        let id = element.id;

        let depth = self.stack.len();
        self.stack.push(PNodeBuilder::Element(element));

        ParseEvent::new(id, depth)
    }

    pub(crate) fn start_element<Tag>(
        &mut self,
        current: usize,
    ) -> ParseEvent<(StartEventTag, Tag)> {
        let element = {
            let id = self.events.alloc(None);
            PElementBuilder::new(id, current, vec![])
        };
        self.push(element)
    }

    // 直前に終了したノード child を最初の子要素として持つ新しいノードを作る
    pub(crate) fn start_parent<ChildTag, Tag>(
        &mut self,
        child: &ParseEvent<(EndEventTag, ChildTag)>,
    ) -> ParseEvent<(StartEventTag, Tag)> {
        // 注意: (x) |> f() のような式のとき、child は (x) ではなく x を指すので、
        //      スタックに置かれているはずの (x) に対応する要素を探す必要がある。
        let mut i = child.depth.min(self.stack.len() - 1);
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

        let element = {
            let id = self.events.alloc(None);
            PElementBuilder::new(id, start, children)
        };
        self.push(element)
    }

    pub(crate) fn end_element<Tag>(
        &mut self,
        kind: PElementKind,
        #[allow(unused)] start: ParseStart<Tag>,
        current: usize,
    ) -> ParseEvent<(EndEventTag, Tag)> {
        let children = self.split_off(start.depth + 1);

        let mut element = match self.stack.pop() {
            Some(PNodeBuilder::Element(element)) => element,
            _ => unreachable!(),
        };
        element.children.extend(children);
        element.end(kind, current);

        self.push(element)
    }

    pub(crate) fn on_token(&mut self, token: PToken) {
        self.stack.push(PNodeBuilder::Token(token));
    }

    pub(crate) fn finish(mut self, elements: &mut PElementArena) -> (PElement, EventArena) {
        let children = self.split_off(0);
        let eof = match children.last() {
            Some(Some(PNodeBuilder::Token(token))) => *token,
            _ => unreachable!(),
        };

        let mut root = {
            let id = self.events.alloc(None);
            PElementBuilder::new(id, 0, children)
        };
        root.end(PElementKind::RootDecl, eof.to_index());
        let root = root.finish(elements, &mut self.events);

        (root, self.events)
    }
}
