use super::*;
use crate::token::TokenKind;

fn parse_record_pat(
    event: PatStart,
    name: AfterQualifiableName,
    left_brace: PToken,
    px: &mut Px,
) -> AfterPat {
    loop {
        match px.next() {
            TokenKind::Eof | TokenKind::RightBrace => break,
            _ => {
                // FIXME: フィールドパターン
                let _ = px.bump();
            }
        }
    }

    let right_brace_opt = px.eat(TokenKind::RightBrace);
    alloc_record_pat(event, name, left_brace, right_brace_opt, px)
}

pub(crate) fn parse_pat(px: &mut Px) -> Option<AfterPat> {
    let event = px.start_element();
    let pat = match px.next() {
        TokenKind::Char => {
            let token = px.bump();
            alloc_char_pat(event, token, px)
        }
        TokenKind::Ident => {
            let name = parse_qualifiable_name(px).unwrap();

            match px.next() {
                TokenKind::LeftBrace => {
                    let left_brace = px.bump();
                    parse_record_pat(event, name, left_brace, px)
                }
                _ => alloc_name_pat(event, name, px),
            }
        }
        TokenKind::Underscore => {
            let token = px.bump();
            alloc_discard_pat(event, token, px)
        }
        _ => return None,
    };
    Some(pat)
}
