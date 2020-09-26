use crate::{
    cps::*,
    front::{cps_gen::*, cps_gen_lit::*, name_resolution::*},
    parse::*,
    source::*,
};

impl<'a> Xx<'a> {
    fn convert_wildcard_pat_as_cond(&mut self, token: PToken) -> Branch {
        let term = {
            let cause = KVarTermCause::WildcardPat(self.doc, token);
            self.fresh_var("_", cause)
        };
        Branch::Default(term)
    }

    fn emit_default_branch(&mut self, name: ANameId) -> Branch {
        if name.of(self.ast.names()).is_qualified() {
            error_unresolved_value(name.loc(), &self.logger);
        }

        let term = {
            let cause = KVarTermCause::NameDef(self.doc, name);
            self.fresh_var(&name.of(self.ast.names()).text, cause)
        };
        Branch::Default(term)
    }

    fn convert_name_pat_as_cond(&mut self, name: ANameId) -> Branch {
        let loc = name.loc().to_loc(self.doc);

        let value = match resolve_value_path(name, self.path_resolution_context()) {
            Some(it) => it,
            None => return self.emit_default_branch(name),
        };

        match value {
            KLocalValue::Const(k_const) => {
                Branch::Case(KTerm::Const { k_const, loc }, self.const_ty(k_const))
            }
            KLocalValue::UnitLikeStruct(k_struct) => Branch::Case(
                KTerm::RecordTag { k_struct, loc },
                self.record_tag_ty(k_struct),
            ),
            _ => self.emit_default_branch(name),
        }
    }

    fn convert_name_pat_as_assign(&mut self, name_id: ANameId, cond: &KTerm, term: KTerm) {
        let symbol = match term {
            KTerm::Name(symbol) if symbol.local_var.name(&self.local_vars) == "_" => return,
            KTerm::Name(it) => it,
            _ => return,
        };

        self.name_symbols
            .insert(name_id, NameSymbol::LocalVar(symbol.local_var));

        let loc = name_id.loc().to_loc(self.doc);
        self.nodes
            .push(new_let_node(cond.clone(), symbol, new_cont(), loc));
    }

    fn convert_record_pat_as_cond(&mut self, pat: &ARecordPat, loc: Loc) -> AfterRval {
        let k_struct = match resolve_ty_path(pat.left, self.path_resolution_context()) {
            Some(KTy2::Struct(k_struct)) => k_struct,
            _ => {
                error_expected_record_ty(PLoc::from_loc(loc), self.logger);
                return new_error_term(loc);
            }
        };
        let tag_ty = k_struct
            .tag_ty(&self.mod_outline.structs, &self.mod_outline.struct_enums)
            .to_ty2_poly(self.mod_outline);
        (KTerm::RecordTag { k_struct, loc }, tag_ty)
    }

    fn do_convert_pat_as_cond(&mut self, pat_id: APatId, pat: &APat) -> Branch {
        let loc = pat_id.loc().to_loc(self.doc);
        let (term, ty) = match pat {
            APat::Unit => new_unit_term(loc),
            APat::True(_) => new_true_term(loc),
            APat::False(_) => new_false_term(loc),
            APat::Number(token) => {
                convert_number_lit(*token, TyExpect::Todo, self.tokens, self.doc, self.logger)
            }
            APat::Char(token) => convert_char_expr(*token, self.doc, self.tokens),
            APat::Str(token) => convert_str_expr(*token, self.doc, self.tokens),
            APat::Wildcard(token) => return self.convert_wildcard_pat_as_cond(*token),
            APat::Name(name) => return self.convert_name_pat_as_cond(*name),
            APat::Record(record_pat) => self.convert_record_pat_as_cond(record_pat, loc),
        };
        Branch::Case(term, ty)
    }

    fn do_convert_pat_as_assign(&mut self, pat: &APat, cond: &KTerm, term: KTerm) {
        match pat {
            APat::Name(name) => self.convert_name_pat_as_assign(*name, cond, term),
            _ => {}
        }
    }

    pub(crate) fn convert_pat_as_cond(&mut self, pat_id: APatId) -> Branch {
        let pat = pat_id.of(self.ast.pats());
        self.do_convert_pat_as_cond(pat_id, pat)
    }

    pub(crate) fn convert_pat_opt_as_cond(&mut self, pat_opt: Option<APatId>, loc: PLoc) -> Branch {
        match pat_opt {
            Some(pat) => self.convert_pat_as_cond(pat),
            None => {
                let (term, ty) = new_error_term(Loc::new(self.doc, loc));
                Branch::Case(term, ty)
            }
        }
    }

    pub(crate) fn convert_pat_as_assign(&mut self, pat_id: APatId, cond: &KTerm, term: KTerm) {
        let pat = pat_id.of(self.ast.pats());
        self.do_convert_pat_as_assign(pat, cond, term)
    }

    pub(crate) fn convert_pat_opt_as_assign(
        &mut self,
        pat_opt: Option<APatId>,
        cond: &KTerm,
        term: KTerm,
    ) {
        if let Some(pat) = pat_opt {
            self.convert_pat_as_assign(pat, cond, term);
        }
    }
}
