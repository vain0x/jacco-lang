use crate::{
    cps::*,
    front::{cps_conversion::*, cps_generator::*, name_resolution::*},
    parse::*,
};

// 目標: AST 上をトラバースして式や型の意味を記述する。
//      それをどのようなデータ構造に記録していくのかとは分離しておく。

pub(crate) type AfterTy = KTy;
pub(crate) type AfterRval = Option<(KTerm, KTy)>;
pub(crate) type AfterDecl = ();
pub(crate) type BeforeDecls = Option<(KTerm, KTy)>;
pub(crate) type AfterDecls = Option<(KTerm, KTy)>;
pub(crate) type AfterRoot = ();

pub(crate) enum SemanticTarget<'a> {
    RVal(TyExpect<'a>),
    Let(&'a AfterTy),
    Discard,
    TopLevel,
}

struct SemanticAnalyzer<'a> {
    kg: CpsGenerator<'a>,
    ast: &'a ATree,
}

impl<'a> SemanticAnalyzer<'a> {
    pub(crate) fn new(kg: CpsGenerator<'a>, ast: &'a ATree) -> Self {
        Self { kg, ast }
    }

    fn do_on_ty(&mut self, ty_id: ATyId, ty: &ATy) -> AfterTy {
        todo!()
    }

    fn on_ty_or_infer(&mut self, ty_opt: Option<ATyId>) -> AfterTy {
        match ty_opt {
            Some(ty_id) => {
                let ty = ty_id.of(self.ast.tys());
                self.do_on_ty(ty_id, ty)
            }
            None => self.kg.after_ty_infer_missing(),
        }
    }

    fn do_on_rval_expr(
        &mut self,
        expr_id: AExprId,
        expr: &AExpr,
        target: SemanticTarget,
    ) -> AfterRval {
        None
    }

    fn on_rval_expr(&mut self, expr_id: AExprId, target: SemanticTarget) -> AfterRval {
        let expr = expr_id.of(self.ast.exprs());
        self.do_on_rval_expr(expr_id, expr, target)
    }

    fn on_rval_expr_opt(&mut self, expr_opt: Option<AExprId>, target: SemanticTarget) -> AfterRval {
        match expr_opt {
            Some(expr_id) => self.on_rval_expr(expr_id, target),
            None => self.kg.after_rval_expr_missing(),
        }
    }

    fn on_let_decl(&mut self, decl_id: ADeclId, decl: &AFieldLikeDecl) -> AfterDecl {
        let ty = self.on_ty_or_infer(decl.ty_opt);

        let init = self.on_rval_expr_opt(decl.value_opt, SemanticTarget::Let(&ty));

        self.kg.after_let_decl(decl_id, decl, ty, init)
    }

    fn on_decl(&mut self, decl_id: ADeclId, decl: &ADecl, target: SemanticTarget) -> AfterDecl {
        match decl {
            ADecl::Attr => {}
            ADecl::Expr(expr) => {
                self.on_rval_expr(*expr, target);
            }
            ADecl::Let(decl) => {
                self.on_let_decl(decl_id, decl);
            }
            _ => {}
        }
    }

    fn on_decls(&mut self, decls: &ADeclIds, target: SemanticTarget) -> AfterDecls {
        let mut state = self.kg.before_decls(decls);

        for (decl_id, decl) in decls.enumerate(self.ast.decls()) {
            let decl_target = if decls.is_last(decl_id) {
                target
            } else {
                SemanticTarget::Discard
            };

            self.on_decl(decl_id, decl, decl_target);
        }

        self.kg.after_decls(state)
    }

    pub(crate) fn on_root(&mut self, root: &ARoot) -> AfterRoot {
        self.on_decls(&root.decls, SemanticTarget::TopLevel);
    }
}
