//! Jacco 言語の構文木の定義

use super::*;
use crate::{
    source::{HaveLocation, Loc, TRange},
    token::TokenSource,
    utils::{VecArena, VecArenaId},
};
use std::{
    fmt::{self, Debug, Formatter},
    iter::once,
};

#[derive(Copy, Clone)]
pub(crate) enum PLoc {
    Token(PToken),
    TokenBehind(PToken),
}

impl PLoc {
    pub(crate) fn new(token: PToken) -> Self {
        PLoc::Token(token)
    }

    pub(crate) fn behind(self) -> Self {
        match self {
            PLoc::Token(token) => PLoc::TokenBehind(token),
            PLoc::TokenBehind(_) => self,
        }
    }

    pub(crate) fn range(self, root: &PRoot) -> TRange {
        match self {
            PLoc::Token(token) => token.of(&root.tokens).location().range(),
            PLoc::TokenBehind(token) => token.of(&root.tokens).location().range().behind(),
        }
    }
}

impl Debug for PLoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let (token, suffix) = match self {
            PLoc::Token(token) => (token, ""),
            PLoc::TokenBehind(token) => (token, ":behind"),
        };

        Debug::fmt(&token, f)?;
        write!(f, "{}", suffix)
    }
}

pub(crate) struct PNameTag;

pub(crate) type PName = VecArenaId<PNameTag>;

pub(crate) type PNameArena = VecArena<PNameTag, PNameData>;

#[derive(Clone, Debug)]
pub(crate) struct PNameQual {
    pub(crate) name: PToken,
    pub(crate) colon_colon: PToken,
}

#[derive(Clone, Debug)]
pub(crate) struct PNameData {
    pub(crate) quals: Vec<PNameQual>,
    pub(crate) token: PToken,
    pub(crate) text: String,
    pub(crate) full_name: String,
}

impl PName {
    pub(crate) fn text(self, names: &PNameArena) -> &str {
        &names[self].text
    }

    pub(crate) fn is_underscore(self, names: &PNameArena) -> bool {
        let data = &names[self];
        data.quals.is_empty() && data.text == "_"
    }

    pub(crate) fn full_name(self, names: &PNameArena) -> String {
        names[self].full_name.to_string()
    }

    pub(crate) fn name_path(self, names: &PNameArena, tokens: &PTokens) -> Vec<String> {
        names[self]
            .quals
            .iter()
            .map(|q| q.name.text(&tokens).to_string())
            .chain(once(names[self].text.to_string()))
            .collect()
    }

    /// パスの先頭部分の文字列
    pub(crate) fn root_text<'a>(self, names: &'a PNameArena, tokens: &'a PTokens) -> &'a str {
        match names[self].quals.first() {
            Some(qual) => qual.name.text(tokens),
            None => &names[self].text,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PNeverTy {
    pub(crate) bang: PToken,
}

#[derive(Clone, Debug)]
pub(crate) struct PUnitTy {
    pub(crate) left_paren: PToken,
    pub(crate) right_paren_opt: Option<PToken>,
}

#[derive(Clone, Debug)]
pub(crate) struct PPtrTy {
    /// `*` or `**`
    pub(crate) star: PToken,
    pub(crate) mut_opt: Option<PMut>,
    pub(crate) ty_opt: Option<Box<PTy>>,
    pub(crate) rep: OneOrTwo,
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum OneOrTwo {
    One,
    Two,
}

#[derive(Clone, Debug)]
pub(crate) enum PTy {
    Name(PName),
    Never(PNeverTy),
    Unit(PUnitTy),
    Ptr(PPtrTy),
}

#[derive(Clone, Debug)]
pub(crate) struct PRecordPat {
    pub(crate) name: PName,
    pub(crate) left_brace: PToken,
    // fields: Vec<PFieldPat>,
    pub(crate) right_brace_opt: Option<PToken>,
}

#[derive(Clone, Debug)]
pub(crate) enum PPat {
    Char(PToken),
    Name(PName),
    Record(PRecordPat),
}

#[derive(Clone, Debug)]
pub(crate) struct PParam {
    pub(crate) name: PName,
    pub(crate) colon_opt: Option<PToken>,
    pub(crate) ty_opt: Option<PTy>,
    pub(crate) comma_opt: Option<PToken>,
}

#[derive(Clone, Debug)]
pub(crate) struct PParamList {
    pub(crate) left_paren: PToken,
    pub(crate) params: Vec<PParam>,
    pub(crate) right_paren_opt: Option<PToken>,
}

#[derive(Clone, Debug)]
pub(crate) struct PArg {
    pub(crate) expr: PExpr,
    pub(crate) comma_opt: Option<PToken>,
}

#[derive(Clone, Debug)]
pub(crate) struct PArgList {
    /// FIXME: `a[i]` 構文のときは丸カッコではなく `[]` がここに入る
    pub(crate) left_paren: PToken,
    pub(crate) args: Vec<PArg>,
    pub(crate) right_paren_opt: Option<PToken>,
}

impl PArgList {
    pub(crate) fn is_tuple(&self) -> bool {
        match self.args.as_slice() {
            []
            | [PArg {
                comma_opt: Some(_), ..
            }]
            | [_, _, ..] => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PBlock {
    pub(crate) left_brace: PToken,
    pub(crate) decls: Vec<PDecl>,
    pub(crate) last_opt: Option<Box<PExpr>>,
    pub(crate) right_brace_opt: Option<PToken>,
}

#[derive(Clone, Debug)]
pub(crate) struct PNumberExpr {
    pub(crate) token: PToken,
}

#[derive(Clone, Debug)]
pub(crate) struct PCharExpr {
    pub(crate) token: PToken,
}

#[derive(Clone, Debug)]
pub(crate) struct PStrExpr {
    pub(crate) token: PToken,
}

#[derive(Clone, Debug)]
pub(crate) struct PTrueExpr(pub(crate) PToken);

#[derive(Clone, Debug)]
pub(crate) struct PFalseExpr(pub(crate) PToken);

#[derive(Clone, Debug)]
pub(crate) struct PFieldExpr {
    pub(crate) name: PName,
    pub(crate) colon_opt: Option<PToken>,
    pub(crate) value_opt: Option<PExpr>,
    pub(crate) comma_opt: Option<PToken>,
}

#[derive(Clone, Debug)]
pub(crate) struct PRecordExpr {
    pub(crate) name: PName,
    pub(crate) left_brace: PToken,
    pub(crate) fields: Vec<PFieldExpr>,
    pub(crate) right_brace_opt: Option<PToken>,
}

#[derive(Clone, Debug)]
pub(crate) struct PTupleExpr {
    pub(crate) arg_list: PArgList,
}

#[derive(Clone, Debug)]
pub(crate) struct PDotFieldExpr {
    pub(crate) left: Box<PExpr>,
    pub(crate) dot: PToken,
    pub(crate) name_opt: Option<PToken>,
}

#[derive(Clone, Debug)]
pub(crate) struct PCallExpr {
    pub(crate) left: Box<PExpr>,
    pub(crate) arg_list: PArgList,
}

#[derive(Clone, Debug)]
pub(crate) struct PIndexExpr {
    pub(crate) left: Box<PExpr>,
    pub(crate) arg_list: PArgList,
}

#[derive(Clone, Debug)]
pub(crate) struct PAsExpr {
    pub(crate) left: Box<PExpr>,
    pub(crate) keyword: PToken,
    pub(crate) ty_opt: Option<PTy>,
}

#[derive(Clone, Debug)]
pub(crate) struct PUnaryOpExpr {
    pub(crate) op: PUnaryOp,
    pub(crate) op_token: PToken,
    pub(crate) mut_opt: Option<PMut>,
    pub(crate) arg_opt: Option<Box<PExpr>>,
}

#[derive(Clone, Debug)]
pub(crate) struct PBinaryOpExpr {
    pub(crate) op: PBinaryOp,
    pub(crate) op_token: PToken,
    pub(crate) left: Box<PExpr>,
    pub(crate) right_opt: Option<Box<PExpr>>,
}

#[derive(Clone, Debug)]
pub(crate) struct PPipeExpr {
    pub(crate) left: Box<PExpr>,
    pub(crate) pipe: PToken,
    pub(crate) right_opt: Option<Box<PExpr>>,
}

#[derive(Clone, Debug)]
pub(crate) struct PBlockExpr(pub(crate) PBlock);

#[derive(Clone, Debug)]
pub(crate) struct PBreakExpr {
    pub(crate) keyword: PToken,
    pub(crate) arg_opt: Option<Box<PExpr>>,
    pub(crate) loop_id_opt: Option<usize>,
}

#[derive(Clone, Debug)]
pub(crate) struct PContinueExpr {
    pub(crate) keyword: PToken,
    pub(crate) loop_id_opt: Option<usize>,
}

#[derive(Clone, Debug)]
pub(crate) struct PReturnExpr {
    pub(crate) keyword: PToken,
    pub(crate) arg_opt: Option<Box<PExpr>>,
    pub(crate) fn_id_opt: Option<usize>,
}

#[derive(Clone, Debug)]
pub(crate) struct PIfExpr {
    pub(crate) keyword: PToken,
    pub(crate) cond_opt: Option<Box<PExpr>>,
    pub(crate) body_opt: Option<PBlock>,
    pub(crate) else_opt: Option<PToken>,
    pub(crate) alt_opt: Option<Box<PExpr>>,
}

#[derive(Clone, Debug)]
pub(crate) struct PArm {
    pub(crate) pat: PPat,
    pub(crate) arrow_opt: Option<PToken>,
    pub(crate) body_opt: Option<Box<PExpr>>,
    pub(crate) comma_opt: Option<PToken>,
}

#[derive(Clone, Debug)]
pub(crate) struct PMatchExpr {
    pub(crate) keyword: PToken,
    pub(crate) cond_opt: Option<Box<PExpr>>,
    pub(crate) left_brace_opt: Option<PToken>,
    pub(crate) arms: Vec<PArm>,
    pub(crate) right_brace_opt: Option<PToken>,
}

#[derive(Clone, Debug)]
pub(crate) struct PWhileExpr {
    pub(crate) keyword: PToken,
    pub(crate) cond_opt: Option<Box<PExpr>>,
    pub(crate) body_opt: Option<PBlock>,
    pub(crate) loop_id_opt: Option<usize>,
}

#[derive(Clone, Debug)]
pub(crate) struct PLoopExpr {
    pub(crate) keyword: PToken,
    pub(crate) body_opt: Option<PBlock>,
    pub(crate) loop_id_opt: Option<usize>,
}

#[derive(Clone, Debug)]
pub(crate) enum PExpr {
    Number(PNumberExpr),
    Char(PCharExpr),
    Str(PStrExpr),
    True(PTrueExpr),
    False(PFalseExpr),
    Name(PName),
    Record(PRecordExpr),
    Tuple(PTupleExpr),
    DotField(PDotFieldExpr),
    Call(PCallExpr),
    Index(PIndexExpr),
    As(PAsExpr),
    UnaryOp(PUnaryOpExpr),
    BinaryOp(PBinaryOpExpr),
    Pipe(PPipeExpr),
    Block(PBlockExpr),
    Break(PBreakExpr),
    Continue(PContinueExpr),
    Return(PReturnExpr),
    If(PIfExpr),
    Match(PMatchExpr),
    While(PWhileExpr),
    Loop(PLoopExpr),
}

#[derive(Clone, Debug)]
pub(crate) struct PExprDecl {
    pub(crate) expr: PExpr,
    pub(crate) semi_opt: Option<PToken>,
}

#[derive(Clone, Debug)]
pub(crate) struct PLetDecl {
    pub(crate) keyword: PToken,
    pub(crate) name_opt: Option<PName>,
    pub(crate) colon_opt: Option<PToken>,
    pub(crate) ty_opt: Option<PTy>,
    pub(crate) equal_opt: Option<PToken>,
    pub(crate) init_opt: Option<PExpr>,
    pub(crate) semi_opt: Option<PToken>,
}

#[derive(Clone, Debug)]
pub(crate) struct PConstDecl {
    pub(crate) keyword: PToken,
    pub(crate) name_opt: Option<PName>,
    pub(crate) colon_opt: Option<PToken>,
    pub(crate) ty_opt: Option<PTy>,
    pub(crate) equal_opt: Option<PToken>,
    pub(crate) init_opt: Option<PExpr>,
    pub(crate) semi_opt: Option<PToken>,
}

#[derive(Clone, Debug)]
pub(crate) struct PStaticDecl {
    pub(crate) keyword: PToken,
    pub(crate) name_opt: Option<PName>,
    pub(crate) colon_opt: Option<PToken>,
    pub(crate) ty_opt: Option<PTy>,
    pub(crate) equal_opt: Option<PToken>,
    pub(crate) init_opt: Option<PExpr>,
    pub(crate) semi_opt: Option<PToken>,
}

#[derive(Clone, Debug)]
pub(crate) struct PFnDecl {
    pub(crate) vis_opt: Option<PVis>,
    pub(crate) keyword: PToken,
    pub(crate) name_opt: Option<PName>,
    pub(crate) param_list_opt: Option<PParamList>,
    pub(crate) arrow_opt: Option<PToken>,
    pub(crate) result_ty_opt: Option<PTy>,
    pub(crate) block_opt: Option<PBlock>,

    /// 名前解決用
    pub(crate) fn_id_opt: Option<usize>,
}

#[derive(Clone, Debug)]
pub(crate) struct PExternFnDecl {
    pub(crate) extern_keyword: PToken,
    pub(crate) fn_keyword: PToken,
    pub(crate) name_opt: Option<PName>,
    pub(crate) param_list_opt: Option<PParamList>,
    pub(crate) arrow_opt: Option<PToken>,
    pub(crate) result_ty_opt: Option<PTy>,
    pub(crate) semi_opt: Option<PToken>,

    /// 名前解決用
    pub(crate) extern_fn_id_opt: Option<usize>,
}

#[derive(Clone, Debug)]
pub(crate) struct PConstVariantDecl {
    pub(crate) name: PName,
    pub(crate) equal_opt: Option<PToken>,
    pub(crate) value_opt: Option<Box<PExpr>>,
    pub(crate) comma_opt: Option<PToken>,

    // 名前解決用
    pub(crate) const_variant_id_opt: Option<usize>,
}

#[derive(Clone, Debug)]
pub(crate) struct PFieldDecl {
    pub(crate) name: PName,
    pub(crate) colon_opt: Option<PToken>,
    pub(crate) ty_opt: Option<PTy>,
    pub(crate) comma_opt: Option<PToken>,

    // 名前解決用
    pub(crate) field_id_opt: Option<usize>,
}

#[derive(Clone, Debug)]
pub(crate) struct PRecordVariantDecl {
    pub(crate) name: PName,
    pub(crate) left_brace: PToken,
    pub(crate) fields: Vec<PFieldDecl>,
    pub(crate) right_brace_opt: Option<PToken>,
    pub(crate) comma_opt: Option<PToken>,
}

#[derive(Clone, Debug)]
pub(crate) enum PVariantDecl {
    Const(PConstVariantDecl),
    Record(PRecordVariantDecl),
}

#[derive(Clone, Debug)]
pub(crate) struct PEnumDecl {
    pub(crate) vis_opt: Option<PVis>,
    pub(crate) keyword: PToken,
    pub(crate) name_opt: Option<PName>,
    pub(crate) left_brace_opt: Option<PToken>,
    pub(crate) variants: Vec<PVariantDecl>,
    pub(crate) right_brace_opt: Option<PToken>,
}

#[derive(Clone, Debug)]
pub(crate) struct PStructDecl {
    pub(crate) keyword: PToken,
    pub(crate) variant_opt: Option<PVariantDecl>,
    pub(crate) semi_opt: Option<PToken>,
}

#[derive(Clone, Debug)]
pub(crate) struct PUseDecl {
    pub(crate) keyword: PToken,
    pub(crate) name_opt: Option<PName>,
    pub(crate) semi_opt: Option<PToken>,
}

#[derive(Clone, Debug)]
pub(crate) enum PDecl {
    Expr(PExprDecl),
    Let(PLetDecl),
    Const(PConstDecl),
    Static(PStaticDecl),
    Fn(PFnDecl),
    ExternFn(PExternFnDecl),
    Enum(PEnumDecl),
    Struct(PStructDecl),
    Use(PUseDecl),
}

#[derive(Clone, Debug)]
pub(crate) struct PRoot {
    pub(crate) decls: Vec<PDecl>,
    pub(crate) eof: PToken,
    pub(crate) names: PNameArena,
    pub(crate) skipped: Vec<PToken>,
    pub(crate) tokens: PTokens,
}

impl PRoot {
    /// use 宣言で指名されているモジュールの名前を収集する。(いまのところトップレベルにある use 宣言の先頭にある名前だけを収集する。)
    pub(crate) fn collect_used_mod_names(&self, mod_names: &mut Vec<String>) {
        for decl in &self.decls {
            match decl {
                PDecl::Use(PUseDecl {
                    name_opt: Some(name),
                    ..
                }) => mod_names.push(name.root_text(&self.names, &self.tokens).to_string()),
                _ => {}
            }
        }
    }
}

// FIXME: hot fix
macro_rules! impl_node {
    ($($node_ty:ty),* $(,)?) => {
        $(
            impl HaveLocation for $node_ty {
                fn location(&self) -> Loc {
                    Loc::new(TokenSource::Special(stringify!($node_ty)), TRange::ZERO)
                }
            }

            impl<'a> From<(&'a $node_ty, &'a PRoot)> for PToken {
                fn from((node, root): (&'a $node_ty, &'a PRoot)) -> Self {
                    node.some_token(root)
                }
            }

            impl $node_ty {
                #[allow(unused)]
                pub(crate) fn ends_with_block(&self) -> bool {
                    // セミコロン抜けのエラーを強制的に抑制している。
                    true
                }
            }
        )*
    };
}

impl_node! {
    PNameQual,
    PName,
    PNeverTy,
    PUnitTy,
    PPtrTy,
    PRecordPat,
    PParam,
    PParamList,
    PArg,
    PArgList,
    PBlock,
    PNumberExpr,
    PCharExpr,
    PStrExpr,
    PTrueExpr,
    PFalseExpr,
    PFieldExpr,
    PRecordExpr,
    PTupleExpr,
    PDotFieldExpr,
    PCallExpr,
    PIndexExpr,
    PAsExpr,
    PUnaryOpExpr,
    PBinaryOpExpr,
    PPipeExpr,
    PBlockExpr,
    PBreakExpr,
    PContinueExpr,
    PReturnExpr,
    PIfExpr,
    PArm,
    PMatchExpr,
    PWhileExpr,
    PLoopExpr,
    PExprDecl,
    PLetDecl,
    PConstDecl,
    PStaticDecl,
    PFnDecl,
    PExternFnDecl,
    PConstVariantDecl,
    PFieldDecl,
    PRecordVariantDecl,
    PEnumDecl,
    PStructDecl,
    PRoot,
    PTy,
    PPat,
    PExpr,
    PVariantDecl,
    PUseDecl,
    PDecl,
}

impl PToken {
    pub(crate) fn some_token(self, _root: &PRoot) -> PToken {
        self
    }
}

impl PNameQual {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.name
    }
}

impl PName {
    pub(crate) fn some_token(&self, root: &PRoot) -> PToken {
        root.names[*self].token
    }
}

impl PNeverTy {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.bang
    }
}

impl PUnitTy {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.left_paren
    }
}

impl PPtrTy {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.star
    }
}

impl PTy {
    pub(crate) fn some_token(&self, root: &PRoot) -> PToken {
        match self {
            PTy::Name(inner) => inner.some_token(root),
            PTy::Never(inner) => inner.some_token(root),
            PTy::Unit(inner) => inner.some_token(root),
            PTy::Ptr(inner) => inner.some_token(root),
        }
    }
}

impl PRecordPat {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.left_brace
    }
}

impl PParam {
    pub(crate) fn some_token(&self, root: &PRoot) -> PToken {
        self.colon_opt.unwrap_or_else(|| self.name.some_token(root))
    }
}

impl PParamList {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.left_paren
    }
}

impl PArg {
    pub(crate) fn some_token(&self, root: &PRoot) -> PToken {
        self.comma_opt.unwrap_or_else(|| self.expr.some_token(root))
    }
}

impl PArgList {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.left_paren
    }
}

impl PBlock {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.left_brace
    }
}

impl PNumberExpr {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.token
    }
}

impl PCharExpr {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.token
    }
}

impl PStrExpr {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.token
    }
}

impl PTrueExpr {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.0
    }
}

impl PFalseExpr {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.0
    }
}

impl PFieldExpr {
    pub(crate) fn some_token(&self, root: &PRoot) -> PToken {
        self.name.some_token(root)
    }
}

impl PRecordExpr {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.left_brace
    }
}

impl PTupleExpr {
    pub(crate) fn some_token(&self, root: &PRoot) -> PToken {
        self.arg_list.some_token(root)
    }
}

impl PDotFieldExpr {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.dot
    }
}

impl PCallExpr {
    pub(crate) fn some_token(&self, root: &PRoot) -> PToken {
        self.arg_list.some_token(root)
    }
}

impl PIndexExpr {
    pub(crate) fn some_token(&self, root: &PRoot) -> PToken {
        self.arg_list.some_token(root)
    }
}

impl PAsExpr {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.keyword
    }
}

impl PUnaryOpExpr {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.op_token
    }
}

impl PBinaryOpExpr {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.op_token
    }
}

impl PPipeExpr {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.pipe
    }
}

impl PBlockExpr {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.0.left_brace
    }
}

impl PBreakExpr {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.keyword
    }
}

impl PContinueExpr {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.keyword
    }
}

impl PReturnExpr {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.keyword
    }
}

impl PIfExpr {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.keyword
    }
}

impl PArm {
    pub(crate) fn some_token(&self, root: &PRoot) -> PToken {
        self.arrow_opt.unwrap_or_else(|| self.pat.some_token(root))
    }
}

impl PMatchExpr {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.keyword
    }
}

impl PWhileExpr {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.keyword
    }
}

impl PLoopExpr {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.keyword
    }
}

impl PExprDecl {
    pub(crate) fn some_token(&self, root: &PRoot) -> PToken {
        self.semi_opt.unwrap_or_else(|| self.expr.some_token(root))
    }
}

impl PLetDecl {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.keyword
    }
}

impl PConstDecl {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.keyword
    }
}

impl PStaticDecl {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.keyword
    }
}

impl PFnDecl {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.keyword
    }
}

impl PExternFnDecl {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.fn_keyword
    }
}

impl PConstVariantDecl {
    pub(crate) fn some_token(&self, root: &PRoot) -> PToken {
        self.name.some_token(root)
    }
}

impl PFieldDecl {
    pub(crate) fn some_token(&self, root: &PRoot) -> PToken {
        self.name.some_token(root)
    }
}

impl PRecordVariantDecl {
    pub(crate) fn some_token(&self, root: &PRoot) -> PToken {
        self.name.some_token(root)
    }
}

impl PEnumDecl {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.keyword
    }
}

impl PStructDecl {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.keyword
    }
}

impl PUseDecl {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.keyword
    }
}

impl PRoot {
    pub(crate) fn some_token(&self, _root: &PRoot) -> PToken {
        self.eof
    }
}

impl PPat {
    pub(crate) fn some_token(&self, root: &PRoot) -> PToken {
        match self {
            PPat::Char(inner) => inner.some_token(root),
            PPat::Name(inner) => inner.some_token(root),
            PPat::Record(inner) => inner.some_token(root),
        }
    }
}

impl PExpr {
    pub(crate) fn some_token(&self, root: &PRoot) -> PToken {
        match self {
            PExpr::Number(inner) => inner.some_token(root),
            PExpr::Char(inner) => inner.some_token(root),
            PExpr::Str(inner) => inner.some_token(root),
            PExpr::True(inner) => inner.some_token(root),
            PExpr::False(inner) => inner.some_token(root),
            PExpr::Name(inner) => inner.some_token(root),
            PExpr::Record(inner) => inner.some_token(root),
            PExpr::Tuple(inner) => inner.some_token(root),
            PExpr::DotField(inner) => inner.some_token(root),
            PExpr::Call(inner) => inner.some_token(root),
            PExpr::Index(inner) => inner.some_token(root),
            PExpr::As(inner) => inner.some_token(root),
            PExpr::UnaryOp(inner) => inner.some_token(root),
            PExpr::BinaryOp(inner) => inner.some_token(root),
            PExpr::Pipe(inner) => inner.some_token(root),
            PExpr::Block(inner) => inner.some_token(root),
            PExpr::Break(inner) => inner.some_token(root),
            PExpr::Continue(inner) => inner.some_token(root),
            PExpr::Return(inner) => inner.some_token(root),
            PExpr::If(inner) => inner.some_token(root),
            PExpr::Match(inner) => inner.some_token(root),
            PExpr::While(inner) => inner.some_token(root),
            PExpr::Loop(inner) => inner.some_token(root),
        }
    }
}

impl PVariantDecl {
    pub(crate) fn some_token(&self, root: &PRoot) -> PToken {
        match self {
            PVariantDecl::Const(inner) => inner.some_token(root),
            PVariantDecl::Record(inner) => inner.some_token(root),
        }
    }
}

impl PDecl {
    pub(crate) fn some_token(&self, root: &PRoot) -> PToken {
        match self {
            PDecl::Expr(inner) => inner.some_token(root),
            PDecl::Let(inner) => inner.some_token(root),
            PDecl::Const(inner) => inner.some_token(root),
            PDecl::Static(inner) => inner.some_token(root),
            PDecl::Fn(inner) => inner.some_token(root),
            PDecl::ExternFn(inner) => inner.some_token(root),
            PDecl::Enum(inner) => inner.some_token(root),
            PDecl::Struct(inner) => inner.some_token(root),
            PDecl::Use(inner) => inner.some_token(root),
        }
    }
}
