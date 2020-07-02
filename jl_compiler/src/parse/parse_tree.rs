//! Jacco 言語の構文木の定義

use super::*;
use crate::front::NName;
use crate::{impl_node_choice, impl_node_seq};

#[derive(Clone, Debug)]
pub(crate) struct PName {
    pub(crate) token: TokenData,

    /// 名前解決の結果
    /// FIXME: 構文ノードを id か何かで特定できるようにして、このような情報は外部のマップに持つ？
    pub(crate) info_opt: Option<NName>,
}

impl PName {
    pub(crate) fn text(&self) -> &str {
        self.token.text()
    }

    pub(crate) fn decompose(self) -> (String, Location) {
        let (_, text, location) = self.token.decompose();
        (text, location)
    }
}

impl PNode for PName {
    fn len(&self) -> usize {
        1
    }

    fn get(&self, i: usize) -> Option<PElementRef> {
        assert_eq!(i, 0);
        try_as_element_ref(&self.token)
    }

    fn get_mut(&mut self, i: usize) -> Option<PElementMut> {
        assert_eq!(i, 0);
        try_as_element_mut(&mut self.token)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PNameTy(pub(crate) PName);

impl PNode for PNameTy {
    impl_node_seq! { 0 }
}

#[derive(Clone, Debug)]
pub(crate) struct PNeverTy {
    pub(crate) bang: TokenData,
}

impl PNode for PNeverTy {
    impl_node_seq! { bang }
}

#[derive(Clone, Debug)]
pub(crate) struct PUnitTy {
    pub(crate) left_paren: TokenData,
    pub(crate) right_paren_opt: Option<TokenData>,
}

impl PNode for PUnitTy {
    impl_node_seq! { left_paren, right_paren_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PPtrTy {
    pub(crate) star: TokenData,
    pub(crate) mut_opt: Option<PMut>,
    pub(crate) ty_opt: Option<Box<PTy>>,
}

impl PNode for PPtrTy {
    impl_node_seq! { star, mut_opt, ty_opt }
}

#[derive(Clone, Debug)]
pub(crate) enum PTy {
    Name(PNameTy),
    Never(PNeverTy),
    Unit(PUnitTy),
    Ptr(PPtrTy),
}

impl PNode for PTy {
    impl_node_choice! {
        PTy::Name,
        PTy::Never,
        PTy::Unit,
        PTy::Ptr,
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PParam {
    pub(crate) name: PName,
    pub(crate) colon_opt: Option<TokenData>,
    pub(crate) ty_opt: Option<PTy>,
    pub(crate) comma_opt: Option<TokenData>,
}

impl PNode for PParam {
    impl_node_seq! { name, colon_opt, ty_opt, comma_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PParamList {
    pub(crate) left_paren: TokenData,
    pub(crate) params: Vec<PParam>,
    pub(crate) right_paren_opt: Option<TokenData>,
}

impl PNode for PParamList {
    fn len(&self) -> usize {
        self.params.len() + 2
    }

    fn get(&self, mut i: usize) -> Option<PElementRef> {
        if i == 0 {
            return try_as_element_ref(&self.left_paren);
        }
        i -= 1;

        if let Some(param) = self.params.get(i) {
            return try_as_element_ref(param);
        }
        i -= self.params.len();

        if i == 0 {
            return try_as_element_ref(&self.right_paren_opt);
        }

        unreachable!();
    }

    fn get_mut(&mut self, mut i: usize) -> Option<PElementMut> {
        if i == 0 {
            return try_as_element_mut(&mut self.left_paren);
        }
        i -= 1;

        let param_count = self.params.len();
        if let Some(param) = self.params.get_mut(i) {
            return try_as_element_mut(param);
        }
        i -= param_count;

        if i == 0 {
            return try_as_element_mut(&mut self.right_paren_opt);
        }

        unreachable!();
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PArg {
    pub(crate) expr: PExpr,
    pub(crate) comma_opt: Option<TokenData>,
}

impl PNode for PArg {
    impl_node_seq! { expr, comma_opt }
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

impl PNode for PArgList {
    fn len(&self) -> usize {
        self.args.len() + 2
    }

    fn get(&self, mut i: usize) -> Option<PElementRef> {
        if i == 0 {
            return try_as_element_ref(&self.left_paren);
        }
        i -= 1;

        if let Some(arg) = self.args.get(i) {
            return try_as_element_ref(arg);
        }
        i -= self.args.len();

        if i == 0 {
            return try_as_element_ref(&self.right_paren_opt);
        }

        unreachable!();
    }

    fn get_mut(&mut self, mut i: usize) -> Option<PElementMut> {
        if i == 0 {
            return try_as_element_mut(&mut self.left_paren);
        }
        i -= 1;

        let arg_count = self.args.len();
        if let Some(arg) = self.args.get_mut(i) {
            return try_as_element_mut(arg);
        }
        i -= arg_count;

        if i == 0 {
            return try_as_element_mut(&mut self.right_paren_opt);
        }

        unreachable!();
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PBlock {
    pub(crate) left_brace: TokenData,
    pub(crate) decls: Vec<PDecl>,
    pub(crate) last_opt: Option<Box<PExpr>>,
    pub(crate) right_brace_opt: Option<TokenData>,
}

impl PNode for PBlock {
    fn len(&self) -> usize {
        self.decls.len() + 3
    }

    fn get(&self, mut i: usize) -> Option<PElementRef> {
        if i == 0 {
            return try_as_element_ref(&self.left_brace);
        }
        i -= 1;

        if let Some(decl) = self.decls.get(i) {
            return try_as_element_ref(decl);
        }
        i -= self.decls.len();

        match i {
            0 => try_as_element_ref(&self.last_opt),
            1 => try_as_element_ref(&self.right_brace_opt),
            _ => unreachable!(),
        }
    }

    fn get_mut(&mut self, mut i: usize) -> Option<PElementMut> {
        if i == 0 {
            return try_as_element_mut(&mut self.left_brace);
        }
        i -= 1;

        let decl_count = self.decls.len();
        if let Some(decl) = self.decls.get_mut(i) {
            return try_as_element_mut(decl);
        }
        i -= decl_count;

        match i {
            0 => try_as_element_mut(&mut self.last_opt),
            1 => try_as_element_mut(&mut self.right_brace_opt),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PIntExpr {
    pub(crate) token: TokenData,
}

impl PNode for PIntExpr {
    impl_node_seq! { token }
}

#[derive(Clone, Debug)]
pub(crate) struct PFloatExpr {
    pub(crate) token: TokenData,
}

impl PNode for PFloatExpr {
    impl_node_seq! { token }
}

#[derive(Clone, Debug)]
pub(crate) struct PCharExpr {
    pub(crate) token: TokenData,
}

impl PNode for PCharExpr {
    impl_node_seq! { token }
}

#[derive(Clone, Debug)]
pub(crate) struct PStrExpr {
    pub(crate) token: TokenData,
}

impl PNode for PStrExpr {
    impl_node_seq! { token }
}

#[derive(Clone, Debug)]
pub(crate) struct PTrueExpr(pub(crate) TokenData);

impl PNode for PTrueExpr {
    impl_node_seq! { 0 }
}

#[derive(Clone, Debug)]
pub(crate) struct PFalseExpr(pub(crate) TokenData);

impl PNode for PFalseExpr {
    impl_node_seq! { 0 }
}

#[derive(Clone, Debug)]
pub(crate) struct PNameExpr(pub(crate) PName);

impl PNode for PNameExpr {
    impl_node_seq! { 0 }
}

#[derive(Clone, Debug)]
pub(crate) struct PFieldExpr {
    pub(crate) name: PName,
    pub(crate) colon_opt: Option<TokenData>,
    pub(crate) value_opt: Option<PExpr>,
    pub(crate) comma_opt: Option<TokenData>,
}

impl PNode for PFieldExpr {
    impl_node_seq! { name, colon_opt, value_opt, comma_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PStructExpr {
    pub(crate) name: PNameTy,
    pub(crate) left_brace: TokenData,
    pub(crate) fields: Vec<PFieldExpr>,
    pub(crate) right_brace_opt: Option<TokenData>,
}

impl PNode for PStructExpr {
    fn len(&self) -> usize {
        self.fields.len() + 3
    }

    fn get(&self, mut i: usize) -> Option<PElementRef> {
        if i == 0 {
            return try_as_element_ref(&self.name);
        }
        i -= 1;

        if i == 0 {
            return try_as_element_ref(&self.left_brace);
        }
        i -= 1;

        let field_count = self.fields.len();
        if let Some(field) = self.fields.get(i) {
            return try_as_element_ref(field);
        }
        i -= field_count;

        if i == 0 {
            return try_as_element_ref(&self.right_brace_opt);
        }

        unreachable!()
    }

    fn get_mut(&mut self, mut i: usize) -> Option<PElementMut> {
        if i == 0 {
            return try_as_element_mut(&mut self.name);
        }
        i -= 1;

        if i == 0 {
            return try_as_element_mut(&mut self.left_brace);
        }
        i -= 1;

        let field_count = self.fields.len();
        if let Some(field) = self.fields.get_mut(i) {
            return try_as_element_mut(field);
        }
        i -= field_count;

        if i == 0 {
            return try_as_element_mut(&mut self.right_brace_opt);
        }

        unreachable!()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PTupleExpr {
    pub(crate) arg_list: PArgList,
}

impl PNode for PTupleExpr {
    impl_node_seq! { arg_list }
}

#[derive(Clone, Debug)]
pub(crate) struct PDotFieldExpr {
    pub(crate) left: Box<PExpr>,
    pub(crate) dot: TokenData,
    pub(crate) name_opt: Option<TokenData>,
}

impl PNode for PDotFieldExpr {
    impl_node_seq! { left, dot, name_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PCallExpr {
    pub(crate) left: Box<PExpr>,
    pub(crate) arg_list: PArgList,
}

impl PNode for PCallExpr {
    impl_node_seq! { left, arg_list }
}

#[derive(Clone, Debug)]
pub(crate) struct PIndexExpr {
    pub(crate) left: Box<PExpr>,
    pub(crate) arg_list: PArgList,
}

impl PNode for PIndexExpr {
    impl_node_seq! { left, arg_list }
}

#[derive(Clone, Debug)]
pub(crate) struct PAsExpr {
    pub(crate) left: Box<PExpr>,
    pub(crate) keyword: TokenData,
    pub(crate) ty_opt: Option<PTy>,
}

impl PNode for PAsExpr {
    impl_node_seq! { left, keyword, ty_opt }
}

// FIXME: 演算子のトークンを持つ
#[derive(Clone, Debug)]
pub(crate) struct PUnaryOpExpr {
    pub(crate) op: PUnaryOp,
    pub(crate) mut_opt: Option<PMut>,
    pub(crate) arg_opt: Option<Box<PExpr>>,
    pub(crate) location: Location,
}

impl PNode for PUnaryOpExpr {
    impl_node_seq! { mut_opt, arg_opt }
}

// FIXME: 演算子のトークンを持つ
#[derive(Clone, Debug)]
pub(crate) struct PBinaryOpExpr {
    pub(crate) op: PBinaryOp,
    pub(crate) left: Box<PExpr>,
    pub(crate) right_opt: Option<Box<PExpr>>,
    pub(crate) location: Location,
}

impl PNode for PBinaryOpExpr {
    impl_node_seq! { left, right_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PPipeExpr {
    pub(crate) left: Box<PExpr>,
    pub(crate) pipe: TokenData,
    pub(crate) right_opt: Option<Box<PExpr>>,
}

impl PNode for PPipeExpr {
    impl_node_seq! { left, pipe, right_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PBlockExpr(pub(crate) PBlock);

impl PNode for PBlockExpr {
    impl_node_seq! { 0 }
}

#[derive(Clone, Debug)]
pub(crate) struct PBreakExpr {
    pub(crate) keyword: TokenData,
    pub(crate) arg_opt: Option<Box<PExpr>>,
    pub(crate) loop_id_opt: Option<usize>,
}

impl PNode for PBreakExpr {
    impl_node_seq! { keyword, arg_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PContinueExpr {
    pub(crate) keyword: TokenData,
    pub(crate) loop_id_opt: Option<usize>,
}

impl PNode for PContinueExpr {
    impl_node_seq! { keyword }
}

#[derive(Clone, Debug)]
pub(crate) struct PReturnExpr {
    pub(crate) keyword: TokenData,
    pub(crate) arg_opt: Option<Box<PExpr>>,
    pub(crate) fn_id_opt: Option<usize>,
}

impl PNode for PReturnExpr {
    impl_node_seq! { keyword, arg_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PIfExpr {
    pub(crate) keyword: TokenData,
    pub(crate) left_paren_opt: Option<TokenData>,
    pub(crate) cond_opt: Option<Box<PExpr>>,
    pub(crate) right_paren_opt: Option<TokenData>,
    pub(crate) body_opt: Option<PBlock>,
    pub(crate) else_opt: Option<TokenData>,
    pub(crate) alt_opt: Option<Box<PExpr>>,
}

impl PNode for PIfExpr {
    impl_node_seq! {
        keyword,
        left_paren_opt,
        cond_opt,
        right_paren_opt,
        body_opt,
        else_opt,
        alt_opt,
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PArm {
    pub(crate) name: PName,
    pub(crate) arrow_opt: Option<TokenData>,
    pub(crate) body_opt: Option<Box<PExpr>>,
    pub(crate) comma_opt: Option<TokenData>,
}

impl PNode for PArm {
    impl_node_seq! { name, arrow_opt, body_opt, comma_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PMatchExpr {
    pub(crate) keyword: TokenData,
    pub(crate) left_paren_opt: Option<TokenData>,
    pub(crate) cond_opt: Option<Box<PExpr>>,
    pub(crate) right_paren_opt: Option<TokenData>,
    pub(crate) left_brace_opt: Option<TokenData>,
    pub(crate) arms: Vec<PArm>,
    pub(crate) right_brace_opt: Option<TokenData>,
}

impl PNode for PMatchExpr {
    fn len(&self) -> usize {
        self.arms.len() + 6
    }

    fn get(&self, mut i: usize) -> Option<PElementRef> {
        match i {
            0 => return try_as_element_ref(&self.keyword),
            1 => return try_as_element_ref(&self.left_paren_opt),
            2 => return try_as_element_ref(&self.cond_opt),
            3 => return try_as_element_ref(&self.right_paren_opt),
            4 => return try_as_element_ref(&self.left_brace_opt),
            _ => {}
        }
        i -= 5;

        let arm_count = self.arms.len();
        if let Some(arm) = self.arms.get(i) {
            return try_as_element_ref(arm);
        }
        i -= arm_count;

        if i == 0 {
            return try_as_element_ref(&self.right_brace_opt);
        }

        unreachable!()
    }

    fn get_mut(&mut self, mut i: usize) -> Option<PElementMut> {
        match i {
            0 => return try_as_element_mut(&mut self.keyword),
            1 => return try_as_element_mut(&mut self.left_paren_opt),
            2 => return try_as_element_mut(&mut self.cond_opt),
            3 => return try_as_element_mut(&mut self.right_paren_opt),
            4 => return try_as_element_mut(&mut self.left_brace_opt),
            _ => {}
        }
        i -= 5;

        let arm_count = self.arms.len();
        if let Some(arm) = self.arms.get_mut(i) {
            return try_as_element_mut(arm);
        }
        i -= arm_count;

        if i == 0 {
            return try_as_element_mut(&mut self.right_brace_opt);
        }

        unreachable!()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PWhileExpr {
    pub(crate) keyword: TokenData,
    pub(crate) left_paren_opt: Option<TokenData>,
    pub(crate) cond_opt: Option<Box<PExpr>>,
    pub(crate) right_paren_opt: Option<TokenData>,
    pub(crate) body_opt: Option<PBlock>,
    pub(crate) loop_id_opt: Option<usize>,
}

impl PNode for PWhileExpr {
    impl_node_seq! { keyword, left_paren_opt, cond_opt, right_paren_opt, body_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PLoopExpr {
    pub(crate) keyword: TokenData,
    pub(crate) body_opt: Option<PBlock>,
    pub(crate) loop_id_opt: Option<usize>,
}

impl PNode for PLoopExpr {
    impl_node_seq! { keyword, body_opt }
}

#[derive(Clone, Debug)]
pub(crate) enum PExpr {
    Int(PIntExpr),
    Float(PFloatExpr),
    Char(PCharExpr),
    Str(PStrExpr),
    True(PTrueExpr),
    False(PFalseExpr),
    Name(PNameExpr),
    Struct(PStructExpr),
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

impl PNode for PExpr {
    impl_node_choice! {
        PExpr::Int,
        PExpr::Float,
        PExpr::Char,
        PExpr::Str,
        PExpr::Name,
        PExpr::True,
        PExpr::False,
        PExpr::Struct,
        PExpr::Tuple,
        PExpr::DotField,
        PExpr::Call,
        PExpr::Index,
        PExpr::As,
        PExpr::UnaryOp,
        PExpr::BinaryOp,
        PExpr::Pipe,
        PExpr::Block,
        PExpr::Break,
        PExpr::Continue,
        PExpr::Return,
        PExpr::If,
        PExpr::Match,
        PExpr::While,
        PExpr::Loop,
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PExprDecl {
    pub(crate) expr: PExpr,
    pub(crate) semi_opt: Option<TokenData>,
}

impl PNode for PExprDecl {
    impl_node_seq! { expr, semi_opt }
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

impl PNode for PLetDecl {
    impl_node_seq! { keyword, name_opt, colon_opt, ty_opt, equal_opt, init_opt, semi_opt }
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

impl PNode for PConstDecl {
    impl_node_seq! { keyword, name_opt, colon_opt, ty_opt, equal_opt, init_opt, semi_opt }
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

impl PNode for PStaticDecl {
    impl_node_seq! { keyword, name_opt, colon_opt, ty_opt, equal_opt, init_opt, semi_opt }
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

impl PNode for PFnDecl {
    impl_node_seq! { vis_opt, keyword, name_opt, param_list_opt, arrow_opt, result_ty_opt, block_opt }
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

impl PNode for PExternFnDecl {
    impl_node_seq! { extern_keyword, fn_keyword, name_opt, param_list_opt, arrow_opt, result_ty_opt, semi_opt }
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

impl PNode for PFieldDecl {
    impl_node_seq! { name, colon_opt, ty_opt, comma_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PStructVariantDecl {
    pub(crate) left_brace: TokenData,
    pub(crate) fields: Vec<PFieldDecl>,
    pub(crate) right_brace_opt: Option<TokenData>,
    pub(crate) comma_opt: Option<TokenData>,
}

impl PNode for PStructVariantDecl {
    fn len(&self) -> usize {
        self.fields.len() + 3
    }

    fn get(&self, mut i: usize) -> Option<PElementRef> {
        if i == 0 {
            return try_as_element_ref(&self.left_brace);
        }
        i -= 1;

        let field_count = self.fields.len();
        if let Some(field) = self.fields.get(i) {
            return try_as_element_ref(field);
        }
        i -= field_count;

        match i {
            0 => try_as_element_ref(&self.right_brace_opt),
            1 => try_as_element_ref(&self.comma_opt),
            _ => unreachable!(),
        }
    }

    fn get_mut(&mut self, mut i: usize) -> Option<PElementMut> {
        if i == 0 {
            return try_as_element_mut(&mut self.left_brace);
        }
        i -= 1;

        let field_count = self.fields.len();
        if let Some(field) = self.fields.get_mut(i) {
            return try_as_element_mut(field);
        }
        i -= field_count;

        match i {
            0 => try_as_element_mut(&mut self.right_brace_opt),
            1 => try_as_element_mut(&mut self.comma_opt),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum PVariantDecl {
    Struct(PStructVariantDecl),
}

impl PNode for PVariantDecl {
    impl_node_choice! { PVariantDecl::Struct }
}

#[derive(Clone, Debug)]
pub(crate) struct PStructDecl {
    pub(crate) keyword: TokenData,
    pub(crate) name_opt: Option<PName>,
    pub(crate) variant_opt: Option<PVariantDecl>,
    pub(crate) semi_opt: Option<TokenData>,
}

impl PNode for PStructDecl {
    impl_node_seq! { keyword, name_opt, variant_opt, semi_opt }
}

#[derive(Clone, Debug)]
pub(crate) enum PDecl {
    Expr(PExprDecl),
    Let(PLetDecl),
    Const(PConstDecl),
    Static(PStaticDecl),
    Fn(PFnDecl),
    ExternFn(PExternFnDecl),
    Struct(PStructDecl),
}

impl PNode for PDecl {
    impl_node_choice! {
        PDecl::Expr,
        PDecl::Let,
        PDecl::Const,
        PDecl::Static,
        PDecl::Fn,
        PDecl::ExternFn,
        PDecl::Struct,
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PRoot {
    pub(crate) decls: Vec<PDecl>,
    pub(crate) eof: TokenData,
    pub(crate) skipped: Vec<TokenData>,
}

impl PNode for PRoot {
    fn len(&self) -> usize {
        self.decls.len() + 1
    }

    fn get(&self, mut i: usize) -> Option<PElementRef> {
        if let Some(decl) = self.decls.get(i) {
            return try_as_element_ref(decl);
        }

        i -= self.decls.len();
        if i == 0 {
            return try_as_element_ref(&self.eof);
        }

        unreachable!()
    }

    fn get_mut(&mut self, mut i: usize) -> Option<PElementMut> {
        let decl_count = self.decls.len();
        if let Some(decl) = self.decls.get_mut(i) {
            return try_as_element_mut(decl);
        }
        i -= decl_count;

        if i == 0 {
            return try_as_element_mut(&mut self.eof);
        }

        unreachable!()
    }
}
