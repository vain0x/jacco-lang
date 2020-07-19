use super::lit_decimal::{eval_decimal, parse_number_suffix, LitErr};
use crate::cps::{KNumber, KNumberTy};

const BINARY: u32 = 2;

fn u8_is_ascii_bit(c: u8) -> bool {
    c == b'0' || c == b'1'
}

pub(crate) fn eval_binary(s: &str) -> Result<(KNumber, KNumberTy), LitErr> {
    debug_assert_eq!(s.as_bytes()[0], b'0');
    debug_assert_eq!((s.as_bytes()[1] as char).to_ascii_lowercase(), 'b');
    let s = &s[2..];

    // fast path
    if s.bytes().all(u8_is_ascii_bit) {
        return do_eval_binary_as_int(s);
    }

    do_eval_binary_slow(s)
}

fn do_eval_binary_slow(s: &str) -> Result<(KNumber, KNumberTy), LitErr> {
    let s = s.replace('_', "");

    let digit_len = s.bytes().take_while(|&c| u8_is_ascii_bit(c)).count();
    if digit_len == s.len() {
        return do_eval_binary_as_int(&s);
    }

    let (body, suffix) = s.split_at(digit_len);
    let ty = match parse_number_suffix(suffix) {
        None | Some(KNumberTy::F32) | Some(KNumberTy::F64) => return Err(LitErr::UnknownSuffix),
        Some(ty) => Some(ty),
    };

    // FIXME: 文字列を経由しない
    match u64::from_str_radix(body, BINARY) {
        Ok(value) => eval_decimal(&format!("{}_{}", value, suffix)),
        Err(_) => Err(LitErr::Flow),
    }
}

fn do_eval_binary_as_int(s: &str) -> Result<(KNumber, KNumberTy), LitErr> {
    match u64::from_str_radix(s, BINARY) {
        Ok(value) => Ok((KNumber::UNN(value), KNumberTy::UNN)),
        Err(_) => Err(LitErr::Flow),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::lit_decimal::factory::unn;

    #[test]
    fn test_eval_binary_basics() {
        assert_eq!(eval_binary("0b0000"), Ok(unn(0)));

        assert_eq!(eval_binary("0b11001010"), Ok(unn(0b11001010)));
    }

    #[test]
    fn test_eval_binary_with_good_suffix() {
        assert_eq!(
            eval_binary("0b1000_i32"),
            Ok((KNumber::INN(0b1000), KNumberTy::I32))
        );

        assert_eq!(
            eval_binary("0b0111_1111_1111_1111_1111_1111_1111_1111_i32"),
            Ok((
                KNumber::INN(0b0111_1111_1111_1111_1111_1111_1111_1111_i32 as i64),
                KNumberTy::I32
            ))
        );
        assert_eq!(
            eval_binary("0b1000_0000_0000_0000_0000_0000_0000_0000_i64"),
            Ok((
                KNumber::INN(0b1000_0000_0000_0000_0000_0000_0000_0000_i64),
                KNumberTy::I64
            ))
        );
    }

    #[test]
    fn test_eval_binary_with_bad_suffix() {
        assert_eq!(eval_binary("0b1_i88"), Err(LitErr::UnknownSuffix));

        assert_eq!(eval_binary("0b1_f32"), Err(LitErr::UnknownSuffix));
    }

    #[test]
    fn test_eval_binary_with_suffix_overflow() {
        assert_eq!(eval_binary("0b1000_0000i8"), Err(LitErr::Flow));
    }
}
