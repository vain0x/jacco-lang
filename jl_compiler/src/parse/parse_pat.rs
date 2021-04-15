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
            // "=>" は確実にパターンの区切りになる。
            TokenKind::Eof | TokenKind::RightBrace | TokenKind::RightFatArrow => break,
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
            if let Some(token) = name.0.as_wildcard() {
                return Some(alloc_wildcard_pat(event, token, px));
            }

            match px.next() {
                TokenKind::LeftBrace => {
                    let left_brace = px.bump();
                    parse_record_pat(event, name, left_brace, px)
                }
                _ => alloc_name_pat(event, name, px),
            }
        }
        _ => return None,
    };
    Some(pat)
}
