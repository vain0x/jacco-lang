use super::{parse_context::Px, parse_expr::parse_name, PName, PPat, PRecordPat};
use crate::token::{TokenData, TokenKind};

fn parse_record_pat(name: PName, left_brace: TokenData, px: &mut Px) -> PRecordPat {
    loop {
        match px.next() {
            TokenKind::Eof | TokenKind::RightBrace | TokenKind::RightBracket => break,
            _ => {
                let _ = px.bump();
            }
        }
    }

    let right_brace_opt = px.eat(TokenKind::RightBrace);
    PRecordPat {
        name,
        left_brace,
        right_brace_opt,
    }
}

pub(crate) fn parse_pat(px: &mut Px) -> Option<PPat> {
    let pat = match px.next() {
        TokenKind::Ident => {
            let name = parse_name(px).unwrap();

            match px.next() {
                TokenKind::LeftBrace => {
                    let left_brace = px.bump();
                    PPat::Record(parse_record_pat(name, left_brace, px))
                }
                _ => PPat::Name(name),
            }
        }
        _ => return None,
    };
    Some(pat)
}
