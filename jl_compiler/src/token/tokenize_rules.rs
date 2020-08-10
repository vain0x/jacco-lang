//! 字句解析のルール

use super::tokenize_context::TokenizeContext;
use super::*;
use crate::source::SourceCode;
use std::rc::Rc;

type Tx = TokenizeContext;

fn char_is_eol(c: char) -> bool {
    c == '\r' || c == '\n'
}

fn char_is_space(c: char) -> bool {
    c.is_whitespace()
}

fn char_is_ident(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn char_is_ident_first(c: char) -> bool {
    char_is_ident(c) && !c.is_ascii_digit()
}

fn char_is_punctuation_first(c: char) -> bool {
    "!\"#$%&'()-=^~@`\\|[]{}+*;:,.<>/".contains(c)
}

fn tokenize_space(tx: &mut Tx) {
    assert!(char_is_space(tx.next()));

    while char_is_space(tx.next()) {
        tx.bump();
    }

    tx.commit(TokenKind::Space);
}

fn tokenize_comment(tx: &mut Tx) {
    assert!(tx.is_followed_by("//"));

    tx.eat("//");

    while !tx.at_eof() && !char_is_eol(tx.next()) {
        tx.bump();
    }

    tx.commit(TokenKind::Comment);
}

fn tokenize_digits(tx: &mut Tx) {
    while tx.next().is_ascii_digit() {
        tx.bump();
        tx.eat("_");
    }
}

fn tokenize_number(tx: &mut Tx) {
    assert!(tx.next().is_ascii_digit());

    let mut kind = TokenKind::Number;

    tokenize_digits(tx);

    if tx.next() == '.' {
        tx.bump();
        tokenize_digits(tx);
    }

    let e = tx.next();
    if e == 'e' || e == 'E' {
        tx.bump();

        let sign_or_digit = tx.next();
        let is_sign = sign_or_digit == '+' || sign_or_digit == '-';
        if is_sign && tx.nth(1).is_ascii_alphanumeric() {
            tx.bump();
            tokenize_digits(tx);
        } else if sign_or_digit.is_ascii_alphanumeric() {
            tokenize_digits(tx);
        } else {
            kind = TokenKind::Other;
        }
    }

    while tx.next().is_ascii_alphanumeric() {
        tx.bump();
    }

    tx.commit(kind);
}

fn tokenize_str_segments(quote: char, tx: &mut Tx) {
    while !tx.at_eof() {
        let c = tx.next();
        if c == '\\' {
            tx.bump();
            tx.bump();
            continue;
        }

        if char_is_eol(c) || c == quote {
            break;
        }

        tx.bump();
    }
}

fn tokenize_char(tx: &mut Tx) {
    assert_eq!(tx.next(), '\'');

    tx.bump();

    tokenize_str_segments('\'', tx);

    tx.eat("'");
    tx.commit(TokenKind::Char);
}

fn tokenize_str(tx: &mut Tx) {
    assert_eq!(tx.next(), '"');

    tx.bump();

    tokenize_str_segments('"', tx);

    tx.eat("\"");
    tx.commit(TokenKind::Str);
}

fn tokenize_ident(tx: &mut Tx) {
    assert!(char_is_ident_first(tx.next()));

    while char_is_ident(tx.next()) {
        tx.bump();
    }

    let kind = match Keyword::parse(tx.current_text()) {
        Some(keyword) => keyword.kind(),
        None => TokenKind::Ident,
    };
    tx.commit(kind);
}

fn do_tokenize_punctuation(tx: &mut Tx) -> Option<(TokenKind, usize)> {
    assert!(char_is_punctuation_first(tx.next()));

    match tx.next() {
        '(' => Some((TokenKind::LeftParen, 1)),
        ')' => Some((TokenKind::RightParen, 1)),
        '[' => Some((TokenKind::LeftBracket, 1)),
        ']' => Some((TokenKind::RightBracket, 1)),
        '{' => Some((TokenKind::LeftBrace, 1)),
        '}' => Some((TokenKind::RightBrace, 1)),
        '<' => match tx.nth(1) {
            '<' => match tx.nth(2) {
                '=' => Some((TokenKind::LeftLeftEqual, 3)),
                _ => Some((TokenKind::LeftLeft, 2)),
            },
            '-' => Some((TokenKind::LeftSlimArrow, 2)),
            '=' => match tx.nth(2) {
                '>' => Some((TokenKind::LeftEqualRight, 3)),
                _ => Some((TokenKind::LeftEqual, 2)),
            },
            _ => Some((TokenKind::LeftAngle, 1)),
        },
        '>' => match tx.nth(1) {
            '>' => match tx.nth(2) {
                '=' => Some((TokenKind::RightRightEqual, 3)),
                _ => Some((TokenKind::RightRight, 2)),
            },
            '=' => Some((TokenKind::RightEqual, 2)),
            _ => Some((TokenKind::RightAngle, 1)),
        },
        '&' => match tx.nth(1) {
            '=' => Some((TokenKind::AndEqual, 2)),
            '&' => match tx.nth(2) {
                '=' => Some((TokenKind::AndAndEqual, 3)),
                _ => Some((TokenKind::AndAnd, 2)),
            },
            _ => Some((TokenKind::And, 1)),
        },
        '!' => match tx.nth(1) {
            '=' => match tx.nth(2) {
                '=' => Some((TokenKind::BangEqualEqual, 3)),
                _ => Some((TokenKind::BangEqual, 2)),
            },
            _ => Some((TokenKind::Bang, 1)),
        },
        ':' => match tx.nth(1) {
            ':' => Some((TokenKind::ColonColon, 2)),
            _ => Some((TokenKind::Colon, 1)),
        },
        ',' => Some((TokenKind::Comma, 1)),
        '.' => match tx.nth(2) {
            '.' => match tx.nth(3) {
                '=' => Some((TokenKind::DotDotEqual, 3)),
                '<' => Some((TokenKind::DotDotLeft, 3)),
                _ => Some((TokenKind::DotDot, 2)),
            },
            _ => Some((TokenKind::Dot, 1)),
        },
        '=' => match tx.nth(1) {
            '>' => Some((TokenKind::RightFatArrow, 2)),
            '=' => match tx.nth(2) {
                '=' => Some((TokenKind::EqualEqualEqual, 3)),
                _ => Some((TokenKind::EqualEqual, 2)),
            },
            _ => Some((TokenKind::Equal, 1)),
        },
        '#' => match tx.nth(1) {
            '!' => Some((TokenKind::HashBang, 2)),
            _ => Some((TokenKind::Hash, 1)),
        },
        '^' => match tx.nth(1) {
            '=' => Some((TokenKind::HatEqual, 2)),
            _ => Some((TokenKind::Hat, 1)),
        },
        '-' => match tx.nth(1) {
            '=' => Some((TokenKind::MinusEqual, 2)),
            '-' => Some((TokenKind::MinusMinus, 2)),
            '>' => Some((TokenKind::RightSlimArrow, 2)),
            _ => Some((TokenKind::Minus, 1)),
        },
        '%' => match tx.nth(1) {
            '=' => Some((TokenKind::PercentEqual, 2)),
            _ => Some((TokenKind::Percent, 1)),
        },
        '|' => match tx.nth(1) {
            '=' => Some((TokenKind::PipeEqual, 2)),
            '|' => match tx.nth(1) {
                '=' => Some((TokenKind::PipePipeEqual, 3)),
                _ => Some((TokenKind::PipePipe, 2)),
            },
            '>' => Some((TokenKind::PipeRight, 2)),
            _ => Some((TokenKind::Pipe, 1)),
        },
        '+' => match tx.nth(1) {
            '=' => Some((TokenKind::PlusEqual, 2)),
            '+' => Some((TokenKind::PlusPlus, 2)),
            _ => Some((TokenKind::Plus, 1)),
        },
        '?' => Some((TokenKind::Question, 1)),
        ';' => Some((TokenKind::Semi, 1)),
        '/' => match tx.nth(1) {
            '=' => Some((TokenKind::SlashEqual, 2)),
            _ => Some((TokenKind::Slash, 1)),
        },
        '*' => match tx.nth(1) {
            '=' => Some((TokenKind::StarEqual, 2)),
            _ => Some((TokenKind::Star, 1)),
        },
        _ => None,
    }
}

fn tokenize_punctuation(tx: &mut Tx) {
    assert!(char_is_punctuation_first(tx.next()));

    let (kind, len) = match do_tokenize_punctuation(tx) {
        None => {
            tx.bump();
            tx.commit(TokenKind::Other);
            return;
        }
        Some(x) => x,
    };

    for _ in 0..len {
        tx.bump();
    }
    tx.commit(kind);
}

pub(crate) fn do_tokenize(tx: &mut Tx) {
    while !tx.at_eof() {
        match tx.next() {
            ' ' | '\t' | '\r' | '\n' => {
                tokenize_space(tx);
            }
            '0'..='9' => {
                tokenize_number(tx);
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                tokenize_ident(tx);
            }
            '\'' => {
                tokenize_char(tx);
            }
            '"' => {
                tokenize_str(tx);
            }
            '/' if tx.nth(1) == '/' => {
                tokenize_comment(tx);
            }
            c if char_is_space(c) => {
                tokenize_space(tx);
            }
            c if char_is_ident_first(c) => {
                tokenize_ident(tx);
            }
            c if char_is_punctuation_first(c) => {
                tokenize_punctuation(tx);
            }
            _ => {
                tx.bump();
                tx.commit(TokenKind::Other);
            }
        }
    }
}

pub(crate) fn tokenize(source_code: Rc<SourceCode>) -> Vec<TokenData> {
    let mut tx = Tx::new(source_code);
    do_tokenize(&mut tx);
    tx.finish()
}
