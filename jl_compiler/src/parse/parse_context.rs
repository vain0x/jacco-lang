use super::*;

/// Parsing context. 構文解析の文脈
pub(crate) struct Px {
    tokens: Vec<TokenData>,
    skipped: Vec<TokenData>,
    logger: Logger,
}

impl Px {
    pub(crate) fn new(mut tokens: Vec<TokenData>, logger: Logger) -> Self {
        tokens.reverse();

        Px {
            tokens,
            skipped: vec![],
            logger,
        }
    }

    pub(crate) fn logger(&self) -> &Logger {
        &self.logger
    }

    fn nth_data(&self, offset: usize) -> Option<&TokenData> {
        assert!(offset < self.tokens.len());

        self.tokens.get(self.tokens.len() - offset - 1)
    }

    pub(crate) fn nth(&self, offset: usize) -> TokenKind {
        self.nth_data(offset).map_or(TokenKind::Eof, |t| t.kind())
    }

    pub(crate) fn next(&self) -> TokenKind {
        self.nth(0)
    }

    pub(crate) fn bump(&mut self) -> TokenData {
        assert!(!self.tokens.is_empty());

        self.tokens.pop().unwrap()
    }

    pub(crate) fn skip(&mut self) {
        let token = self.bump();

        self.skipped.push(token);
    }

    pub(crate) fn expect(&mut self, kind: TokenKind) -> TokenData {
        assert_eq!(self.next(), kind);

        self.bump()
    }

    pub(crate) fn eat(&mut self, kind: TokenKind) -> Option<TokenData> {
        if self.next() == kind {
            Some(self.bump())
        } else {
            None
        }
    }

    pub(crate) fn finish(mut self) -> (TokenData, Vec<TokenData>) {
        assert_eq!(self.tokens.len(), 1);

        let eof = self.expect(TokenKind::Eof);
        (eof, self.skipped)
    }
}
