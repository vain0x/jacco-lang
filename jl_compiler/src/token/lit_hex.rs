use super::lit_decimal::{eval_decimal, parse_number_suffix, LitErr};
use crate::cps::{KNumber, KNumberTy};

const HEX: u32 = 16;

fn u8_is_ascii_hexdigit(c: u8) -> bool {
    match c {
        b'0'..=b'9' | b'A'..=b'F' | b'a'..=b'f' => true,
        _ => false,
    }
}

pub(crate) fn eval_hex(s: &str) -> Result<(KNumber, KNumberTy), LitErr> {
    debug_assert_eq!(s.as_bytes()[0], b'0');
    debug_assert_eq!((s.as_bytes()[1] as char).to_ascii_lowercase(), 'x');
    let s = &s[2..];

    // fast path
    if s.bytes().all(u8_is_ascii_hexdigit) {
        return do_eval_hex_as_int(s);
    }

    do_eval_hex_slow(s)
}

fn do_eval_hex_slow(s: &str) -> Result<(KNumber, KNumberTy), LitErr> {
    let s = s.replace('_', "");

    let digit_len = s.bytes().take_while(|&c| u8_is_ascii_hexdigit(c)).count();
    if digit_len == s.len() {
        return do_eval_hex_as_int(&s);
    }

    let (body, suffix) = s.split_at(digit_len);
    let ty = match parse_number_suffix(suffix) {
        None | Some(KNumberTy::F32) | Some(KNumberTy::F64) => return Err(LitErr::UnknownSuffix),
        Some(ty) => Some(ty),
    };

    // FIXME: 文字列を経由しない
    match u64::from_str_radix(body, HEX) {
        Ok(value) => eval_decimal(&format!("{}_{}", value, suffix)),
        Err(_) => Err(LitErr::Flow),
    }
}

fn do_eval_hex_as_int(s: &str) -> Result<(KNumber, KNumberTy), LitErr> {
    match u64::from_str_radix(s, HEX) {
        Ok(value) => Ok((KNumber::UNN(value), KNumberTy::UNN)),
        Err(_) => Err(LitErr::Flow),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::lit_decimal::factory::unn;

    #[test]
    fn test_eval_hex_basics() {
        assert_eq!(eval_hex("0x0000"), Ok(unn(0)));

        assert_eq!(eval_hex("0xdeadbeefc0ffee"), Ok(unn(0xdeadbeefc0ffee)));
    }

    #[test]
    fn test_eval_hex_ending_with_f64() {
        // `f64` はサフィックスではなく値の一部。
        assert_eq!(eval_hex("0x1f64"), Ok(unn(8036)));
    }

    #[test]
    fn test_eval_hex_with_good_suffix() {
        assert_eq!(
            eval_hex("0x100_i32"),
            Ok((KNumber::INN(0x100), KNumberTy::I32))
        );

        assert_eq!(
            eval_hex("0x7fffffff_i32"),
            Ok((KNumber::INN(0x7fffffff_i32 as i64), KNumberTy::I32))
        );
        assert_eq!(
            eval_hex("0x80000000_i64"),
            Ok((KNumber::INN(0x80000000_i64), KNumberTy::I64))
        );
    }

    #[test]
    fn test_eval_hex_with_bad_suffix() {
        assert_eq!(eval_hex("0x1_i88"), Err(LitErr::UnknownSuffix));
    }

    #[test]
    fn test_eval_hex_with_suffix_overflow() {
        assert_eq!(eval_hex("0x100_i8"), Err(LitErr::Flow));
        assert_eq!(eval_hex("0x80000000_i32"), Err(LitErr::Flow));
    }
}
