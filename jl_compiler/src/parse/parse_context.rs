use super::*;

/// Parsing context. 構文解析の文脈
pub(crate) struct Px {
    tokens: PTokens,
    current: usize,
    elements: PElementArena,
    skipped: Vec<PToken>,
    pub(crate) ast: ATree,
    pub(crate) builder: PTreeBuilder,
    #[allow(unused)]
    logger: Logger,
}

impl Px {
    pub(crate) fn new(tokens: PTokens, logger: Logger) -> Self {
        Px {
            tokens,
            current: 0,
            elements: PElementArena::new(),
            builder: PTreeBuilder::new(),
            ast: ATree::default(),
            skipped: vec![],
            logger,
        }
    }

    pub(crate) fn tokens(&self) -> &PTokens {
        &self.tokens
    }

    #[allow(unused)]
    pub(crate) fn logger(&self) -> &Logger {
        &self.logger
    }

    pub(crate) fn start_element<Tag>(&mut self) -> ParseStart<Tag> {
        let current = self.current;
        self.builder.start_element(current)
    }

    pub(crate) fn start_parent<ChildTag, Tag>(
        &mut self,
        child: &ParseEnd<ChildTag>,
    ) -> ParseStart<Tag> {
        self.builder.start_parent(child)
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
        self.builder.on_token(p_token);
        self.current += 1;
        p_token
    }

    pub(crate) fn skip(&mut self) {
        let token = self.bump();

        self.skipped.push(token);
    }

    pub(crate) fn eat(&mut self, kind: TokenKind) -> Option<PToken> {
        if self.next() == kind {
            Some(self.bump())
        } else {
            None
        }
    }

    pub(crate) fn finish(
        mut self,
    ) -> (
        PToken,
        Vec<PToken>,
        PTokens,
        PElementArena,
        ATree,
        PTreeBuilder,
    ) {
        assert_eq!(self.current + 1, self.tokens.len());
        assert_eq!(self.next(), TokenKind::Eof);

        let eof = self.bump();
        (
            eof,
            self.skipped,
            self.tokens,
            self.elements,
            self.ast,
            self.builder,
        )
    }
}

impl<Tag> ParseStart<Tag> {
    pub(super) fn end(self, kind: PElementKind, px: &mut Px) -> ParseEnd<Tag> {
        let current = px.current;
        px.builder.end_element(kind, self, current)
    }
}
