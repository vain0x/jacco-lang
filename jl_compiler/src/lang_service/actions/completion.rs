use super::{Doc, LangService, TPos16};

pub(crate) fn completion(_doc: Doc, _pos: TPos16, _ls: &mut LangService) -> Vec<()> {
    vec![]
}
