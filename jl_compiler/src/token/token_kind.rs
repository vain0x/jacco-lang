/// 字句の種類
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum TokenKind {
    Eof,
    Space,
    Comment,
    Other,
    Number,
    Char,
    Str,
    Ident,
    Underscore,
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
    Unit,
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

impl TokenKind {
    #[allow(unused)]
    pub(crate) fn is_leading_trivia(self) -> bool {
        match self {
            TokenKind::Space | TokenKind::Comment | TokenKind::Other => true,
            _ => false,
        }
    }
}
