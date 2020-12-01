use crate::{
    cps::*,
    front::{cps_gen::*, name_resolution::*},
    logs::*,
    parse::*,
};

pub(crate) struct TyResolver<'a> {
    pub(crate) ast: &'a ATree,
    pub(crate) name_referents: &'a NameReferents,
    pub(crate) name_symbols: &'a NameSymbols,
    pub(crate) logger: &'a DocLogger,
}

impl<'a> TyResolver<'a> {
    fn do_convert_name_ty(&self, ty_id: ATyId, name: ANameId) -> KTy {
        if name.of(self.ast.names()).is_qualified() {
            error_unsupported_path_ty(name.loc(), self.logger);
        }

        match resolve_ty_name(name, self.name_referents, self.name_symbols) {
            Some(ty) => ty,
            None => {
                error_unresolved_ty(name.loc(), self.logger);
                KTy::Unresolved {
                    cause: KTyCause::NameUnresolved(ty_id),
                }
            }
        }
    }

    fn do_convert_ty(&self, ty_id: ATyId, ty: &ATy) -> KTy {
        match ty {
            ATy::Name(name) => self.do_convert_name_ty(ty_id, *name),
            ATy::App(name, ty_args) => {
                let ty = Box::new(self.do_convert_name_ty(ty_id, *name));
                let ty_args = ty_args.iter().map(|ty| self.convert_ty(ty)).collect();
                KTy::App { ty, ty_args }
            }
            ATy::InferTy => {
                // FIXME: メタ変数にする。シグネチャだったらエラーにする。
                error_unresolved_ty(ty_id.loc(), self.logger);
                KTy::Unresolved {
                    cause: KTyCause::InferTy(ty_id),
                }
            }
            ATy::Never => KTy::Never,
            ATy::Unit => KTy::Unit,
            ATy::Ptr(APtrTy { mut_opt, ty_opt }) => {
                let k_mut = mut_opt.unwrap_or(KMut::Const);
                let base_ty = self.convert_ty_opt(*ty_opt);
                base_ty.into_ptr(k_mut)
            }
            ATy::Fn(AFnTy {
                param_tys,
                result_ty_opt,
            }) => {
                let param_tys = param_tys.iter().map(|&ty| self.convert_ty(ty)).collect();
                let result_ty = Box::new(self.convert_ty_or_unit(*result_ty_opt));
                KTy::Fn {
                    ty_params: vec![],
                    param_tys,
                    result_ty,
                }
            }
        }
    }

    pub(crate) fn convert_ty(&self, ty_id: ATyId) -> KTy {
        let ty = ty_id.of(self.ast.tys());
        self.do_convert_ty(ty_id, ty)
    }

    pub(crate) fn convert_ty_opt(&self, ty_opt: Option<ATyId>) -> KTy {
        match ty_opt {
            Some(ty) => self.convert_ty(ty),
            None => KTy::Unresolved {
                cause: KTyCause::Miss,
            },
        }
    }

    pub(crate) fn convert_ty_or_unit(&self, ty_opt: Option<ATyId>) -> KTy {
        match ty_opt {
            Some(ty) => self.convert_ty(ty),
            None => KTy::Unit,
        }
    }

    pub(crate) fn convert_param_tys(&self, param_decls: &[AParamDecl]) -> Vec<KTy> {
        param_decls
            .iter()
            .map(|param_decl| self.convert_ty_opt(param_decl.ty_opt))
            .collect()
    }
}

impl<'a> Xx<'a> {
    pub(crate) fn ty_resolver(&self) -> TyResolver {
        TyResolver {
            ast: self.ast,
            name_referents: self.name_referents,
            name_symbols: self.name_symbols,
            logger: self.logger,
        }
    }
}
