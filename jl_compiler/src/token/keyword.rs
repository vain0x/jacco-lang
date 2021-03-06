use super::*;

#[derive(Clone, Copy)]
pub(crate) struct Keyword(TokenKind);

impl Keyword {
    pub(crate) fn kind(self) -> TokenKind {
        self.0
    }

    pub(crate) fn parse(text: &str) -> Option<Keyword> {
        let kind = match text {
            "_" => TokenKind::Underscore,
            "as" => TokenKind::As,
            "box" => TokenKind::Box,
            "break" => TokenKind::Break,
            "case" => TokenKind::Case,
            "cl" => TokenKind::Cl,
            "crate" => TokenKind::Crate,
            "const" => TokenKind::Const,
            "continue" => TokenKind::Continue,
            "default" => TokenKind::Default,
            "defer" => TokenKind::Defer,
            "do" => TokenKind::Do,
            "dyn" => TokenKind::Dyn,
            "else" => TokenKind::Else,
            "enum" => TokenKind::Enum,
            "ensures" => TokenKind::Ensures,
            "export" => TokenKind::Export,
            "extern" => TokenKind::Extern,
            "false" => TokenKind::False,
            "fn" => TokenKind::Fn,
            "for" => TokenKind::For,
            "from" => TokenKind::From,
            "if" => TokenKind::If,
            "impl" => TokenKind::Impl,
            "import" => TokenKind::Import,
            "in" => TokenKind::In,
            "inline" => TokenKind::Inline,
            "let" => TokenKind::Let,
            "loop" => TokenKind::Loop,
            "macro" => TokenKind::Macro,
            "match" => TokenKind::Match,
            "mod" => TokenKind::Mod,
            "move" => TokenKind::Move,
            "mut" => TokenKind::Mut,
            "of" => TokenKind::Of,
            "out" => TokenKind::Out,
            "priv" => TokenKind::Priv,
            "pub" => TokenKind::Pub,
            "raw" => TokenKind::Raw,
            "ref" => TokenKind::Ref,
            "requires" => TokenKind::Requires,
            "return" => TokenKind::Return,
            "safe" => TokenKind::Safe,
            "self" => TokenKind::SelfLower,
            "static" => TokenKind::Static,
            "struct" => TokenKind::Struct,
            "super" => TokenKind::Super,
            "then" => TokenKind::Then,
            "throw" => TokenKind::Throw,
            "to" => TokenKind::To,
            "trait" => TokenKind::Trait,
            "true" => TokenKind::True,
            "try" => TokenKind::Try,
            "type" => TokenKind::Type,
            "union" => TokenKind::Union,
            "unit" => TokenKind::Unit,
            "unless" => TokenKind::Unless,
            "unsafe" => TokenKind::Unsafe,
            "until" => TokenKind::Until,
            "use" => TokenKind::Use,
            "val" => TokenKind::Val,
            "void" => TokenKind::Void,
            "when" => TokenKind::When,
            "where" => TokenKind::Where,
            "while" => TokenKind::While,
            "with" => TokenKind::With,
            "yield" => TokenKind::Yield,
            _ => return None,
        };
        Some(Keyword(kind))
    }
}
