use super::*;

/// Parsing context. 構文解析の文脈
pub(crate) struct Px {
    tokens: PTokens,
    current: usize,
    names: PNameArena,
    elements: PElementArena,
    skipped: Vec<PToken>,
    logger: Logger,
}

impl Px {
    pub(crate) fn new(tokens: PTokens, logger: Logger) -> Self {
        Px {
            tokens,
            current: 0,
            names: PNameArena::new(),
            elements: PElementArena::new(),
            skipped: vec![],
            logger,
        }
    }

    pub(crate) fn tokens(&self) -> &PTokens {
        &self.tokens
    }

    pub(crate) fn logger(&self) -> &Logger {
        &self.logger
    }

    fn nth_data(&self, offset: usize) -> &TokenData {
        assert!(self.current + offset < self.tokens.len());

        let index = (self.current + offset).min(self.tokens.len() - 1);
        &self.tokens[PToken::from_index(index)]
    }

    pub(crate) fn nth(&self, offset: usize) -> TokenKind {
        self.nth_data(offset).kind()
    }

    pub(crate) fn next(&self) -> TokenKind {
        self.nth(0)
    }

    pub(crate) fn bump(&mut self) -> PToken {
        assert!(self.current < self.tokens.len());

        let p_token = PToken::from_index(self.current);
        self.current += 1;
        p_token
    }

    pub(crate) fn skip(&mut self) {
        let token = self.bump();

        self.skipped.push(token);
    }

    pub(crate) fn expect(&mut self, kind: TokenKind) -> PToken {
        assert_eq!(self.next(), kind);

        self.bump()
    }

    pub(crate) fn eat(&mut self, kind: TokenKind) -> Option<PToken> {
        if self.next() == kind {
            Some(self.bump())
        } else {
            None
        }
    }

    pub(crate) fn finish(mut self) -> (PToken, PNameArena, Vec<PToken>, PTokens, PElementArena) {
        assert_eq!(self.current + 1, self.tokens.len());

        let eof = self.expect(TokenKind::Eof);
        (eof, self.names, self.skipped, self.tokens, self.elements)
    }
}

pub(crate) fn alloc_name(quals: Vec<PNameQual>, token: PToken, px: &mut Px) -> PName {
    let text = token.text(px.tokens()).to_string();
    let full_name = {
        let mut s = String::new();

        for qual in &quals {
            s += qual.name.text(px.tokens());
            s += "::";
        }

        s += token.text(px.tokens());
        s
    };

    px.names.alloc(PNameData {
        quals,
        token,
        text,
        full_name,
    })
}
