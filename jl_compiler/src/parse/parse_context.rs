use super::*;

impl<Tag> ParseEvent<(StartEventTag, Tag)> {
    pub(super) fn end(self, kind: PElementKind, px: &mut Px) -> ParseEvent<(EndEventTag, Tag)> {
        let current = px.current;
        px.events.end_element(kind, self, current)
    }
}

pub(crate) struct ParseEventListener {
    events: Vec<ParseEventData>,
}

impl ParseEventListener {
    pub(crate) fn new() -> Self {
        ParseEventListener { events: vec![] }
    }

    fn alloc<Tag>(&mut self, data: ParseEventData) -> ParseEvent<(StartEventTag, Tag)> {
        let event = ParseEvent::new(self.events.len());
        self.events.push(data);
        event
    }

    fn start_element<Tag>(&mut self, current: usize) -> ParseEvent<(StartEventTag, Tag)> {
        ParseEvent::new(current)
    }

    fn start_parent<ChildTag, Tag>(
        &mut self,
        child: ParseEvent<(EndEventTag, ChildTag)>,
    ) -> ParseEvent<(StartEventTag, Tag)> {
        todo!()
    }

    fn end_element<Tag>(
        &mut self,
        kind: PElementKind,
        start: ParseEvent<(StartEventTag, Tag)>,
        current: usize,
    ) -> ParseEvent<(EndEventTag, Tag)> {
        todo!()
    }

    fn on_token(&mut self) {}
}

/// Parsing context. 構文解析の文脈
pub(crate) struct Px {
    tokens: PTokens,
    current: usize,
    names: PNameArena,
    elements: PElementArena,
    skipped: Vec<PToken>,
    pub(crate) events: ParseEventListener,
    logger: Logger,
}

impl Px {
    pub(crate) fn new(tokens: PTokens, logger: Logger) -> Self {
        Px {
            tokens,
            current: 0,
            names: PNameArena::new(),
            elements: PElementArena::new(),
            events: ParseEventListener::new(),
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

    fn start_element<Tag>(&mut self) -> ParseEvent<(StartEventTag, Tag)> {
        let current = self.current;
        self.events.start_element(current)
    }

    pub(crate) fn start_expr(&mut self) -> ExprStart {
        self.start_element()
    }

    pub(crate) fn start_parent<ChildTag>(
        &mut self,
        child: &ParseEvent<(EndEventTag, ChildTag)>,
    ) -> ExprStart {
        todo!()
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

        self.events.on_token();

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
