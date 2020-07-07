//! Jacco 言語の構文木の定義

use super::*;
use crate::{front::NName, source::Range, token::TokenSource};

#[derive(Clone, Debug)]
pub(crate) struct PNameQual {
    pub(crate) name: TokenData,
    pub(crate) colon_colon: TokenData,
}

#[derive(Clone, Debug)]
pub(crate) struct PName {
    pub(crate) quals: Vec<PNameQual>,
    pub(crate) token: TokenData,

    /// 名前解決の結果
    /// FIXME: 構文ノードを id か何かで特定できるようにして、このような情報は外部のマップに持つ？
    pub(crate) info_opt: Option<NName>,
}

impl PName {
    pub(crate) fn text(&self) -> &str {
        self.token.text()
    }

    pub(crate) fn is_underscore(&self) -> bool {
        self.quals.is_empty() && self.token.text() == "_"
    }

    pub(crate) fn full_name(&self) -> String {
        let mut s = String::new();

        for qual in &self.quals {
            s += qual.name.text();
            s += "::";
        }

        s += self.token.text();
        s
    }

    pub(crate) fn decompose(self) -> (String, Location) {
        let (_, text, location) = self.token.decompose();
        (text, location)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PNeverTy {
    pub(crate) bang: TokenData,
}

#[derive(Clone, Debug)]
pub(crate) struct PUnitTy {
    pub(crate) left_paren: TokenData,
    pub(crate) right_paren_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) struct PPtrTy {
    pub(crate) star: TokenData,
    pub(crate) mut_opt: Option<PMut>,
    pub(crate) ty_opt: Option<Box<PTy>>,
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
    pub(crate) left_brace: TokenData,
    // fields: Vec<PFieldPat>,
    pub(crate) right_brace_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) enum PPat {
    Name(PName),
    Record(PRecordPat),
}

#[derive(Clone, Debug)]
pub(crate) struct PParam {
    pub(crate) name: PName,
    pub(crate) colon_opt: Option<TokenData>,
    pub(crate) ty_opt: Option<PTy>,
    pub(crate) comma_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) struct PParamList {
    pub(crate) left_paren: TokenData,
    pub(crate) params: Vec<PParam>,
    pub(crate) right_paren_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) struct PArg {
    pub(crate) expr: PExpr,
    pub(crate) comma_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) struct PArgList {
    /// FIXME: `a[i]` 構文のときは丸カッコではなく `[]` がここに入る
    pub(crate) left_paren: TokenData,
    pub(crate) args: Vec<PArg>,
    pub(crate) right_paren_opt: Option<TokenData>,
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
    pub(crate) left_brace: TokenData,
    pub(crate) decls: Vec<PDecl>,
    pub(crate) last_opt: Option<Box<PExpr>>,
    pub(crate) right_brace_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) struct PIntExpr {
    pub(crate) token: TokenData,
}

#[derive(Clone, Debug)]
pub(crate) struct PFloatExpr {
    pub(crate) token: TokenData,
}

#[derive(Clone, Debug)]
pub(crate) struct PCharExpr {
    pub(crate) token: TokenData,
}

#[derive(Clone, Debug)]
pub(crate) struct PStrExpr {
    pub(crate) token: TokenData,
}

#[derive(Clone, Debug)]
pub(crate) struct PTrueExpr(pub(crate) TokenData);

#[derive(Clone, Debug)]
pub(crate) struct PFalseExpr(pub(crate) TokenData);

#[derive(Clone, Debug)]
pub(crate) struct PFieldExpr {
    pub(crate) name: PName,
    pub(crate) colon_opt: Option<TokenData>,
    pub(crate) value_opt: Option<PExpr>,
    pub(crate) comma_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) struct PRecordExpr {
    pub(crate) name: PName,
    pub(crate) left_brace: TokenData,
    pub(crate) fields: Vec<PFieldExpr>,
    pub(crate) right_brace_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) struct PTupleExpr {
    pub(crate) arg_list: PArgList,
}

#[derive(Clone, Debug)]
pub(crate) struct PDotFieldExpr {
    pub(crate) left: Box<PExpr>,
    pub(crate) dot: TokenData,
    pub(crate) name_opt: Option<TokenData>,
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
    pub(crate) keyword: TokenData,
    pub(crate) ty_opt: Option<PTy>,
}

// FIXME: 演算子のトークンを持つ
#[derive(Clone, Debug)]
pub(crate) struct PUnaryOpExpr {
    pub(crate) op: PUnaryOp,
    pub(crate) mut_opt: Option<PMut>,
    pub(crate) arg_opt: Option<Box<PExpr>>,
    pub(crate) location: Location,
}

// FIXME: 演算子のトークンを持つ
#[derive(Clone, Debug)]
pub(crate) struct PBinaryOpExpr {
    pub(crate) op: PBinaryOp,
    pub(crate) left: Box<PExpr>,
    pub(crate) right_opt: Option<Box<PExpr>>,
    pub(crate) location: Location,
}

#[derive(Clone, Debug)]
pub(crate) struct PPipeExpr {
    pub(crate) left: Box<PExpr>,
    pub(crate) pipe: TokenData,
    pub(crate) right_opt: Option<Box<PExpr>>,
}

#[derive(Clone, Debug)]
pub(crate) struct PBlockExpr(pub(crate) PBlock);

#[derive(Clone, Debug)]
pub(crate) struct PBreakExpr {
    pub(crate) keyword: TokenData,
    pub(crate) arg_opt: Option<Box<PExpr>>,
    pub(crate) loop_id_opt: Option<usize>,
}

#[derive(Clone, Debug)]
pub(crate) struct PContinueExpr {
    pub(crate) keyword: TokenData,
    pub(crate) loop_id_opt: Option<usize>,
}

#[derive(Clone, Debug)]
pub(crate) struct PReturnExpr {
    pub(crate) keyword: TokenData,
    pub(crate) arg_opt: Option<Box<PExpr>>,
    pub(crate) fn_id_opt: Option<usize>,
}

#[derive(Clone, Debug)]
pub(crate) struct PIfExpr {
    pub(crate) keyword: TokenData,
    pub(crate) cond_opt: Option<Box<PExpr>>,
    pub(crate) body_opt: Option<PBlock>,
    pub(crate) else_opt: Option<TokenData>,
    pub(crate) alt_opt: Option<Box<PExpr>>,
}

#[derive(Clone, Debug)]
pub(crate) struct PArm {
    pub(crate) pat: PPat,
    pub(crate) arrow_opt: Option<TokenData>,
    pub(crate) body_opt: Option<Box<PExpr>>,
    pub(crate) comma_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) struct PMatchExpr {
    pub(crate) keyword: TokenData,
    pub(crate) cond_opt: Option<Box<PExpr>>,
    pub(crate) left_brace_opt: Option<TokenData>,
    pub(crate) arms: Vec<PArm>,
    pub(crate) right_brace_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) struct PWhileExpr {
    pub(crate) keyword: TokenData,
    pub(crate) cond_opt: Option<Box<PExpr>>,
    pub(crate) body_opt: Option<PBlock>,
    pub(crate) loop_id_opt: Option<usize>,
}

#[derive(Clone, Debug)]
pub(crate) struct PLoopExpr {
    pub(crate) keyword: TokenData,
    pub(crate) body_opt: Option<PBlock>,
    pub(crate) loop_id_opt: Option<usize>,
}

#[derive(Clone, Debug)]
pub(crate) enum PExpr {
    Int(PIntExpr),
    Float(PFloatExpr),
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

impl Default for PExpr {
    fn default() -> Self {
        PExpr::Str(PStrExpr {
            token: TokenData::default(),
        })
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PExprDecl {
    pub(crate) expr: PExpr,
    pub(crate) semi_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) struct PLetDecl {
    pub(crate) keyword: TokenData,
    pub(crate) name_opt: Option<PName>,
    pub(crate) colon_opt: Option<TokenData>,
    pub(crate) ty_opt: Option<PTy>,
    pub(crate) equal_opt: Option<TokenData>,
    pub(crate) init_opt: Option<PExpr>,
    pub(crate) semi_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) struct PConstDecl {
    pub(crate) keyword: TokenData,
    pub(crate) name_opt: Option<PName>,
    pub(crate) colon_opt: Option<TokenData>,
    pub(crate) ty_opt: Option<PTy>,
    pub(crate) equal_opt: Option<TokenData>,
    pub(crate) init_opt: Option<PExpr>,
    pub(crate) semi_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) struct PStaticDecl {
    pub(crate) keyword: TokenData,
    pub(crate) name_opt: Option<PName>,
    pub(crate) colon_opt: Option<TokenData>,
    pub(crate) ty_opt: Option<PTy>,
    pub(crate) equal_opt: Option<TokenData>,
    pub(crate) init_opt: Option<PExpr>,
    pub(crate) semi_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) struct PFnDecl {
    pub(crate) vis_opt: Option<PVis>,
    pub(crate) keyword: TokenData,
    pub(crate) name_opt: Option<PName>,
    pub(crate) param_list_opt: Option<PParamList>,
    pub(crate) arrow_opt: Option<TokenData>,
    pub(crate) result_ty_opt: Option<PTy>,
    pub(crate) block_opt: Option<PBlock>,

    /// 名前解決用
    pub(crate) fn_id_opt: Option<usize>,
}

#[derive(Clone, Debug)]
pub(crate) struct PExternFnDecl {
    pub(crate) extern_keyword: TokenData,
    pub(crate) fn_keyword: TokenData,
    pub(crate) name_opt: Option<PName>,
    pub(crate) param_list_opt: Option<PParamList>,
    pub(crate) arrow_opt: Option<TokenData>,
    pub(crate) result_ty_opt: Option<PTy>,
    pub(crate) semi_opt: Option<TokenData>,

    /// 名前解決用
    pub(crate) extern_fn_id_opt: Option<usize>,
}

#[derive(Clone, Debug)]
pub(crate) struct PConstVariantDecl {
    pub(crate) name: PName,
    pub(crate) equal_opt: Option<TokenData>,
    pub(crate) value_opt: Option<Box<PExpr>>,
    pub(crate) comma_opt: Option<TokenData>,

    // 名前解決用
    pub(crate) const_variant_id_opt: Option<usize>,
}

#[derive(Clone, Debug)]
pub(crate) struct PFieldDecl {
    pub(crate) name: PName,
    pub(crate) colon_opt: Option<TokenData>,
    pub(crate) ty_opt: Option<PTy>,
    pub(crate) comma_opt: Option<TokenData>,

    // 名前解決用
    pub(crate) field_id_opt: Option<usize>,
}

#[derive(Clone, Debug)]
pub(crate) struct PRecordVariantDecl {
    pub(crate) name: PName,
    pub(crate) left_brace: TokenData,
    pub(crate) fields: Vec<PFieldDecl>,
    pub(crate) right_brace_opt: Option<TokenData>,
    pub(crate) comma_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) enum PVariantDecl {
    Const(PConstVariantDecl),
    Record(PRecordVariantDecl),
}

#[derive(Clone, Debug)]
pub(crate) struct PEnumDecl {
    pub(crate) vis_opt: Option<PVis>,
    pub(crate) keyword: TokenData,
    pub(crate) name_opt: Option<PName>,
    pub(crate) left_brace_opt: Option<TokenData>,
    pub(crate) variants: Vec<PVariantDecl>,
    pub(crate) right_brace_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) struct PStructDecl {
    pub(crate) keyword: TokenData,
    pub(crate) variant_opt: Option<PVariantDecl>,
    pub(crate) semi_opt: Option<TokenData>,
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
}

#[derive(Clone, Debug)]
pub(crate) struct PRoot {
    pub(crate) decls: Vec<PDecl>,
    pub(crate) eof: TokenData,
    pub(crate) skipped: Vec<TokenData>,
}

// FIXME: hot fix
macro_rules! impl_node {
    ($($node_ty:ty),* $(,)?) => {
        $(
            impl HaveLocation for $node_ty {
                fn location(&self) -> Location {
                    Location::new(TokenSource::Special(stringify!($node_ty)), Range::default())
                }
            }

            impl $node_ty {
                #[allow(unused)]
                pub(crate) fn ends_with_block(&self) -> bool {
                    // セミコロン抜けのエラーを防ぐ
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
    PIntExpr,
    PFloatExpr,
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
    PDecl,
}
