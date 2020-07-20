use std::marker::PhantomData;

#[derive(Copy, Clone)]
pub(crate) struct ParseEvent<Tag>(usize, PhantomData<Tag>);

impl<Tag> ParseEvent<Tag> {
    pub(crate) fn new(event: usize) -> Self {
        Self(event, PhantomData)
    }

    pub(crate) fn event(self) -> usize {
        self.0
    }
}

pub(crate) struct StartEventTag;
pub(crate) struct EndEventTag;
pub(crate) struct ParamTag;
pub(crate) struct ArgTag;
pub(crate) struct TyTag;
pub(crate) struct ExprTag;
pub(crate) struct DeclTag;

pub(crate) type ParamStart = ParseEvent<(StartEventTag, ParamTag)>;
pub(crate) type ParamEnd = ParseEvent<(EndEventTag, ParamTag)>;
pub(crate) type ArgStart = ParseEvent<(StartEventTag, ArgTag)>;
pub(crate) type ArgEnd = ParseEvent<(EndEventTag, ArgTag)>;
pub(crate) type TyStart = ParseEvent<(StartEventTag, TyTag)>;
pub(crate) type TyEnd = ParseEvent<(EndEventTag, TyTag)>;
pub(crate) type ExprStart = ParseEvent<(StartEventTag, ExprTag)>;
pub(crate) type ExprEnd = ParseEvent<(EndEventTag, ExprTag)>;
pub(crate) type DeclStart = ParseEvent<(StartEventTag, DeclTag)>;
pub(crate) type DeclEnd = ParseEvent<(EndEventTag, DeclTag)>;

pub(crate) enum ParseEventData {
    StartNew,
    StartParent,
    End,
    Token,
}
