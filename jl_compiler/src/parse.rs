mod parse_context;
mod parse_stmt;
mod parse_term;
mod parse_tree;

pub(crate) use parse_stmt::parse_tokens;
pub(crate) use parse_tree::*;

use crate::source::SourceFile;
use crate::token::{BinaryOp, Location, TokenData, TokenKind, TokenSource};
use parse_stmt::parse_semi;
use parse_term::{parse_block, parse_term};

type Px = parse_context::ParseContext;

/// 関数の中か外か。
/// QUESTION: より適切な名前？
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Placement {
    /// 関数の内
    Local,

    /// 関数の外
    Global,
}
