use super::*;

#[derive(Clone, Copy)]
pub(crate) struct Keyword(TokenKind);

static KEYWORD_TABLE: &[(TokenKind, &str)] = &[
    (TokenKind::As, "as"),
    (TokenKind::Break, "break"),
    (TokenKind::Continue, "continue"),
    (TokenKind::Else, "else"),
    (TokenKind::Enum, "enum"),
    (TokenKind::Extern, "extern"),
    (TokenKind::False, "false"),
    (TokenKind::Fn, "fn"),
    (TokenKind::For, "for"),
    (TokenKind::Generic, "generic"),
    (TokenKind::If, "if"),
    (TokenKind::In, "in"),
    (TokenKind::Let, "let"),
    (TokenKind::Loop, "loop"),
    (TokenKind::Match, "match"),
    (TokenKind::Mod, "mod"),
    (TokenKind::Move, "move"),
    (TokenKind::Mut, "mut"),
    (TokenKind::Pub, "pub"),
    (TokenKind::Ref, "ref"),
    (TokenKind::Return, "return"),
    (TokenKind::Static, "static"),
    (TokenKind::Struct, "struct"),
    (TokenKind::Super, "super"),
    (TokenKind::True, "true"),
    (TokenKind::Type, "type"),
    (TokenKind::Use, "use"),
    (TokenKind::While, "while"),
];

impl Keyword {
    pub(crate) fn kind(self) -> TokenKind {
        self.0
    }

    pub(crate) fn parse(text: &str) -> Option<Keyword> {
        KEYWORD_TABLE
            .iter()
            .filter_map(|&(kind, word)| {
                if word == text {
                    Some(Keyword(kind))
                } else {
                    None
                }
            })
            .next()
    }
}
