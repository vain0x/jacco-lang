use crate::{cps::*, front::cps_gen::*, logs::*, parse::*, source::*, token::*};

pub(crate) fn new_unit_term(loc: Loc) -> AfterRval {
    (KTerm::Unit { loc }, KTy2::Unit)
}

pub(crate) fn new_true_term(loc: Loc) -> AfterRval {
    (KTerm::True { loc }, KTy2::BOOL)
}

pub(crate) fn new_false_term(loc: Loc) -> AfterRval {
    (KTerm::False { loc }, KTy2::BOOL)
}

pub(crate) fn convert_number_lit(
    token: PToken,
    ty_expect: TyExpect,
    tokens: &PTokens,
    doc: Doc,
    logger: &DocLogger,
) -> AfterRval {
    let text = token.text(tokens).to_string();
    let cause = KTermCause::Token(doc, token);
    let loc = cause.loc();

    match eval_number(&text) {
        Ok((_, number_ty)) => {
            let ty = KTy2::Number(number_ty);
            let term = match number_ty {
                KNumberTy::F32 | KNumberTy::F64 | KNumberTy::FNN => KTerm::Float {
                    text,
                    ty: ty.clone(),
                    loc,
                },
                KNumberTy::C8 | KNumberTy::C16 | KNumberTy::C32 | KNumberTy::CNN => KTerm::Char {
                    text,
                    ty: ty.clone(),
                    loc,
                },
                KNumberTy::UNN => {
                    if let Some(ty) = ty_expect.as_number().filter(|&ty| ty != KNumberTy::UNN) {
                        return (
                            KTerm::Int {
                                text,
                                ty: KTy2::Number(ty),
                                cause,
                            },
                            KTy2::Number(ty),
                        );
                    }

                    // FIXME: 後続のパスが UNN をうまく処理できなくて、unsigned long long になってしまう。いまのところ、ここで i32 にしておく
                    KTerm::Int {
                        text,
                        ty: KTy2::I32,
                        cause,
                    }
                }
                _ => KTerm::Int {
                    text,
                    ty: ty.clone(),
                    cause,
                },
            };
            (term, ty)
        }
        Err(err) => {
            let message = match err {
                LitErr::Flow => "不正な値です",
                LitErr::UnknownSuffix => "不正なサフィックスです",
            };
            logger.error(PLoc::new(token), message);
            new_never_term(cause.loc())
        }
    }
}

pub(crate) fn convert_char_expr(token: PToken, doc: Doc, tokens: &PTokens) -> AfterRval {
    let ty = KTy2::C8;
    let term = KTerm::Char {
        text: token.text(tokens).to_string(),
        ty: ty.clone(),
        loc: KTermCause::Token(doc, token).loc(),
    };
    (term, ty)
}

pub(crate) fn convert_str_expr(token: PToken, doc: Doc, tokens: &PTokens) -> AfterRval {
    let term = KTerm::Str {
        text: token.text(tokens).to_string(),
        loc: KTermCause::Token(doc, token).loc(),
    };
    let ty = KTy2::C8.into_ptr(KMut::Const);
    (term, ty)
}
