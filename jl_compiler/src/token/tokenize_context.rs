use super::*;
use crate::source::{SourceCode, TPos, TRange};
use std::{cmp::min, rc::Rc};
use text_position_rs::TextPosition as _;

/// Tokenization context. 字句解析の文脈
pub(crate) struct TokenizeContext {
    source_code: Rc<SourceCode>,
    current_index: usize,
    last_index: usize,
    last_pos: TPos,
    tokens: Vec<TokenData>,
}

impl TokenizeContext {
    pub(crate) fn new(source_code: Rc<SourceCode>) -> Self {
        TokenizeContext {
            source_code,
            current_index: 0,
            last_index: 0,
            last_pos: TPos::ZERO,
            tokens: vec![],
        }
    }

    pub(crate) fn assert_invariants(&self) {
        assert!(self.last_index <= self.current_index);
        assert!(self.current_index <= self.source_code.len());
    }

    pub(crate) fn at_eof(&self) -> bool {
        self.current_index >= self.source_code.len()
    }

    pub(crate) fn nth(&self, index: usize) -> char {
        self.source_code[min(self.current_index + index, self.source_code.len())..]
            .chars()
            .next()
            .unwrap_or('\0')
    }

    pub(crate) fn next(&self) -> char {
        self.nth(0)
    }

    pub(crate) fn current_text(&self) -> &str {
        &self.source_code[self.last_index..self.current_index]
    }

    fn bump_many(&mut self, len: usize) {
        assert!(self.current_index + len <= self.source_code.len());

        self.current_index += len;

        self.assert_invariants();
    }

    pub(crate) fn bump(&mut self) {
        self.bump_many(self.next().len_utf8());
    }

    pub(crate) fn is_followed_by(&self, text: &str) -> bool {
        self.source_code[self.current_index..].starts_with(text)
    }

    pub(crate) fn eat(&mut self, text: &str) -> bool {
        if self.is_followed_by(text) {
            self.bump_many(text.len());
            true
        } else {
            false
        }
    }

    fn push_token(&mut self, token_data: TokenData) {
        self.tokens.push(token_data);
    }

    pub(crate) fn commit(&mut self, token: TokenKind) {
        let text = self.current_text().to_string();

        let text_len = TPos::from_str(&text);
        let range = TRange::at(self.last_pos, text_len);
        let token = TokenData::new(token, text, range);

        self.push_token(token);

        self.last_index = self.current_index;
        self.last_pos += text_len;

        self.assert_invariants();
    }

    pub(crate) fn finish(mut self) -> Vec<TokenData> {
        assert_eq!(self.current_index, self.last_index);
        assert_eq!(self.current_index, self.source_code.len());

        self.commit(TokenKind::Eof);

        assert!(self
            .tokens
            .last()
            .map_or(false, |t| t.kind() == TokenKind::Eof));

        self.tokens
    }
}
