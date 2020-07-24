use crate::utils::TakeOut;
use std::mem::replace;

/// 字句の種類
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum TokenKind {
    /// `take_out` の呼び出し後
    TakenOut,
    Eof,
    Space,
    Comment,
    Other,
    Number,
    Char,
    Str,
    Ident,
    /// `as` キーワード
    As,
    Box,
    Break,
    Case,
    Cl,
    Crate,
    Const,
    Continue,
    Default,
    Defer,
    Do,
    Dyn,
    Else,
    Enum,
    Ensures,
    Export,
    Extern,
    False,
    Fn,
    For,
    From,
    If,
    Impl,
    Import,
    In,
    Inline,
    Let,
    Loop,
    Macro,
    Match,
    Mod,
    Move,
    Mut,
    Of,
    Out,
    Priv,
    Pub,
    Raw,
    Ref,
    Requires,
    Return,
    Safe,
    SelfLower,
    Static,
    Struct,
    Super,
    Then,
    Throw,
    To,
    Trait,
    True,
    Try,
    Type,
    Union,
    Unless,
    Unsafe,
    Until,
    Use,
    Val,
    Void,
    When,
    Where,
    While,
    With,
    Yield,
    /// `(`
    LeftParen,
    /// `)`
    RightParen,
    /// `[`
    LeftBracket,
    /// `]`
    RightBracket,
    /// `{`
    LeftBrace,
    /// `}`
    RightBrace,
    /// `&`
    And,
    /// `&&`
    AndAnd,
    /// `&&=`
    AndAndEqual,
    /// `&=`
    AndEqual,
    Bang,
    BangEqual,
    BangEqualEqual,
    Colon,
    ColonColon,
    Comma,
    Dot,
    DotDot,
    DotDotEqual,
    /// `..<`
    DotDotLeft,
    Equal,
    EqualEqual,
    EqualEqualEqual,
    Hash,
    HashBang,
    Hat,
    HatEqual,
    /// `<`
    LeftAngle,
    /// `<=`
    LeftEqual,
    /// `<=>`
    LeftEqualRight,
    /// `<<`
    LeftLeft,
    /// `<<=`
    LeftLeftEqual,
    /// `<-`
    LeftSlimArrow,
    Minus,
    MinusEqual,
    MinusMinus,
    Percent,
    PercentEqual,
    Pipe,
    PipeEqual,
    PipePipe,
    PipePipeEqual,
    /// `|>`
    PipeRight,
    Plus,
    PlusEqual,
    PlusPlus,
    Question,
    /// `>`
    RightAngle,
    /// `>=`
    RightEqual,
    /// `=>`
    RightFatArrow,
    /// `->`
    RightSlimArrow,
    /// `>>`
    RightRight,
    /// `>>=`
    RightRightEqual,
    /// `;`
    Semi,
    Slash,
    SlashEqual,
    Star,
    StarEqual,
}

impl TakeOut for TokenKind {
    fn take_out(&mut self) -> Self {
        replace(self, TokenKind::TakenOut)
    }
}
