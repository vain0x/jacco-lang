//! 構文木・構文解析

mod p_binary_op;
mod p_element;
mod p_unary_op;
mod parse_context;
mod parse_decl;
mod parse_expr;
mod parse_tree;
mod parse_ty;

#[macro_use]
mod p_node;

pub(crate) use p_element::{PElementMut, PElementRef};
pub(crate) use p_node::{try_as_element_mut, try_as_element_ref, PNode};
pub(crate) use p_binary_op::PBinaryOp;
pub(crate) use p_unary_op::PUnaryOp;
pub(crate) use parse_decl::parse_tokens;
pub(crate) use parse_tree::*;

use crate::logs::Logger;
use crate::token::{HaveLocation, Location, TokenData, TokenKind};
use parse_context::{p_error, Px};
use parse_decl::parse_semi;
use parse_expr::{parse_block, parse_expr, parse_name};
use parse_ty::{parse_ty, parse_ty_ascription};

/// 関数の中か外か。
/// FIXME: より適切な名前？
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Placement {
    /// 関数の内
    Local,

    /// 関数の外 (モジュールの直下)
    Global,
}
