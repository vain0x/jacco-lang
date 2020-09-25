use crate::{cps::*, front::cps_gen::*, logs::*, parse::*, source::*, token::*};

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
            match number_ty {
                KNumberTy::F32 | KNumberTy::F64 | KNumberTy::FNN => KTerm::Float { text, ty, loc },
                KNumberTy::C8 | KNumberTy::C16 | KNumberTy::C32 | KNumberTy::CNN => {
                    KTerm::Char { text, ty, loc }
                }
                KNumberTy::UNN => {
                    if let Some(ty) = ty_expect.as_number() {
                        return KTerm::Int {
                            text,
                            ty: KTy2::Number(ty),
                            cause,
                        };
                    }

                    // FIXME: 後続のパスが UNN をうまく処理できなくて、unsigned long long になってしまう。いまのところ、ここで i32 にしておく
                    KTerm::Int {
                        text,
                        ty: KTy2::I32,
                        cause,
                    }
                }
                _ => KTerm::Int { text, ty, cause },
            }
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
    KTerm::Char {
        text: token.text(tokens).to_string(),
        ty: KTy2::C8,
        loc: KTermCause::Token(doc, token).loc(),
    }
}

pub(crate) fn convert_str_expr(token: PToken, doc: Doc, tokens: &PTokens) -> AfterRval {
    KTerm::Str {
        text: token.text(tokens).to_string(),
        loc: KTermCause::Token(doc, token).loc(),
    }
}
