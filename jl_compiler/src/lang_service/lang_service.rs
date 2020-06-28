use crate::source::{Pos, Range};

pub struct LangService {}

pub type Source = usize;

pub struct Location {
    source: Source,
    range: Range,
}

impl LangService {
    pub fn new() -> Self {
        LangService {}
    }

    pub fn did_initialize(&mut self) {}

    pub fn shutdown(&mut self) {}

    pub fn open_doc(&mut self, source: Source, version: i64, text: String) {}

    pub fn change_doc(&mut self, source: Source, version: i64, text: String) {}

    pub fn close_doc(&mut self, source: Source) {}

    pub fn completion(&mut self, source: Source, pos: Pos) -> Vec<()> {
        vec![]
    }

    pub fn definitions(&mut self, source: Source, pos: Pos) -> Vec<Location> {
        vec![]
    }

    pub fn document_highlight(&mut self, source: Source, pos: Pos) -> Vec<()> {
        vec![]
    }

    pub fn hover(&mut self, source: Source, pos: Pos) -> Option<()> {
        None
    }

    pub fn references(
        &mut self,
        source: Source,
        pos: Pos,
        include_definition: bool,
    ) -> Vec<Location> {
        vec![]
    }

    pub fn prepare_rename(&mut self, source: Source, pos: Pos) -> Option<()> {
        None
    }

    pub fn rename(&mut self, source: Source, pos: Pos, new_name: String) -> Option<()> {
        None
    }

    pub fn validate(&mut self, source: Source) -> (Option<i64>, Vec<()>) {
        (Some(0), vec![])
    }
}
