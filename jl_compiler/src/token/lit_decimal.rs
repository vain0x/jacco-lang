use crate::cps::{KNumber, KNumberTy};
use std::fmt::Debug;

#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) enum LitErr {
    /// Overflow or underflow
    Flow,
    UnknownSuffix,
}

pub(crate) fn parse_number_suffix(s: &str) -> Option<KNumberTy> {
    let ty = match s {
        "i8" => KNumberTy::I8,
        "i16" => KNumberTy::I16,
        "i32" => KNumberTy::I32,
        "i64" => KNumberTy::I64,
        "isize" => KNumberTy::Isize,
        "u8" => KNumberTy::U8,
        "u16" => KNumberTy::U16,
        "u32" => KNumberTy::U32,
        "u64" => KNumberTy::U64,
        "usize" => KNumberTy::Usize,
        "f32" => KNumberTy::F32,
        "f64" => KNumberTy::F64,
        "c8" => KNumberTy::C8,
        "c16" => KNumberTy::C16,
        "c32" => KNumberTy::C32,
        _ => return None,
    };
    Some(ty)
}

fn u8_is_ascii_digit(c: u8) -> bool {
    (c as char).is_ascii_digit()
}

fn u8_is_ascii_float_char(c: u8) -> bool {
    match c {
        b'0'..=b'9' | b'.' | b'e' | b'E' | b'+' | b'-' => true,
        _ => false,
    }
}

pub(crate) fn eval_number(s: &str) -> Result<(KNumber, KNumberTy), LitErr> {
    if s.starts_with("0b") {
        super::lit_binary::eval_binary(s)
    } else if s.starts_with("0x") {
        super::lit_hex::eval_hex(s)
    } else {
        eval_decimal(s)
    }
}

pub(crate) fn eval_decimal(s: &str) -> Result<(KNumber, KNumberTy), LitErr> {
    // fast path for integer
    if s.bytes().all(u8_is_ascii_digit) {
        return eval_decimal_as_int_as_possible(s).ok_or(LitErr::Flow);
    };

    // fast path for float
    if s.bytes().all(u8_is_ascii_float_char) {
        return eval_decimal_as_float(s).ok_or(LitErr::Flow);
    }

    do_eval_decimal_slow_path(s)
}

fn do_eval_decimal_slow_path(s: &str) -> Result<(KNumber, KNumberTy), LitErr> {
    // 区切り文字を取り除く。指数部の開始文字を e に統一する。
    let s = s.replace('_', "").replace(&['E', 'p', 'P'] as &[char], "e");

    let int_part_len = s.bytes().take_while(|&c| u8_is_ascii_digit(c)).count();
    if int_part_len == s.len() {
        return eval_decimal_as_int_as_possible(&s).ok_or(LitErr::Flow);
    }

    let float_part_len = s[int_part_len..]
        .bytes()
        .take_while(|&c| u8_is_ascii_float_char(c))
        .count();

    let body_len = int_part_len + float_part_len;
    if body_len == s.len() {
        return eval_decimal_as_float(&s).ok_or(LitErr::Flow);
    }

    let (body, suffix) = s.split_at(body_len);
    let ty = parse_number_suffix(suffix).ok_or(LitErr::UnknownSuffix)?;
    eval_decimal_with_ty(body, ty).ok_or(LitErr::Flow)
}

fn eval_decimal_as_int_as_possible(s: &str) -> Option<(KNumber, KNumberTy)> {
    if let Ok(value) = s.parse::<u64>() {
        return Some((KNumber::UNN(value), KNumberTy::UNN));
    }

    eval_decimal_as_float(&s)
}

fn eval_decimal_as_float(s: &str) -> Option<(KNumber, KNumberTy)> {
    s.parse::<f64>()
        .ok()
        .filter(|x| x.is_finite())
        .map(|value| (KNumber::FNN(value), KNumberTy::FNN))
}

fn eval_decimal_with_ty(s: &str, ty: KNumberTy) -> Option<(KNumber, KNumberTy)> {
    let value = match ty {
        KNumberTy::I8 => KNumber::INN(s.parse::<i8>().ok()? as i64),
        KNumberTy::I16 => KNumber::INN(s.parse::<i16>().ok()? as i64),
        KNumberTy::I32 => KNumber::INN(s.parse::<i32>().ok()? as i64),
        KNumberTy::I64 | KNumberTy::INN => KNumber::INN(s.parse::<i64>().ok()?),
        KNumberTy::Isize => KNumber::INN(s.parse::<isize>().ok()? as i64),
        KNumberTy::U8 => KNumber::UNN(s.parse::<u8>().ok()? as u64),
        KNumberTy::U16 => KNumber::UNN(s.parse::<u16>().ok()? as u64),
        KNumberTy::U32 => KNumber::UNN(s.parse::<u32>().ok()? as u64),
        KNumberTy::U64 | KNumberTy::UNN => KNumber::UNN(s.parse::<u64>().ok()?),
        KNumberTy::Usize => KNumber::UNN(s.parse::<usize>().ok()? as u64),
        KNumberTy::F32 => KNumber::FNN(s.parse::<f32>().ok().filter(|x| x.is_finite())? as f64),
        KNumberTy::F64 | KNumberTy::FNN => {
            KNumber::FNN(s.parse::<f64>().ok().filter(|x| x.is_finite())?)
        }
        KNumberTy::C8 => KNumber::CNN(s.parse::<u8>().ok()? as u32),
        KNumberTy::C16 => KNumber::CNN(s.parse::<u16>().ok()? as u32),
        KNumberTy::C32 | KNumberTy::CNN => KNumber::CNN(s.parse::<u32>().ok()?),
    };
    Some((value, ty))
}

#[allow(unused)]
pub(crate) mod factory {
    use crate::cps::{KNumber, KNumberTy};

    pub(crate) fn unn(value: u64) -> (KNumber, KNumberTy) {
        (KNumber::UNN(value), KNumberTy::UNN)
    }

    pub(crate) fn inn(value: i64) -> (KNumber, KNumberTy) {
        (KNumber::INN(value), KNumberTy::INN)
    }

    pub(crate) fn fnn(value: f64) -> (KNumber, KNumberTy) {
        (KNumber::FNN(value), KNumberTy::FNN)
    }
}

#[cfg(test)]
mod tests {
    use super::factory::{fnn, unn};
    use super::*;
    use std::iter::{once, repeat};

    #[test]
    fn test_eval_decimal_zeros() {
        assert_eq!(eval_decimal("0"), Ok(unn(0)));
        assert_eq!(eval_decimal("00"), Ok(unn(0)));

        let z10000 = repeat('0').take(10000).collect::<String>();
        assert_eq!(eval_decimal(&z10000), Ok(unn(0)));
    }

    // 単純なケース
    #[test]
    fn test_eval_decimal_basics() {
        assert_eq!(eval_decimal("1"), Ok(unn(1)));
        assert_eq!(eval_decimal("42"), Ok(unn(42)));

        assert_eq!(eval_decimal("1.25"), Ok(fnn(1.25)));
        assert_eq!(eval_decimal("1e-2"), Ok(fnn(0.01)));
    }

    // 区切り文字を含むケース
    #[test]
    fn test_eval_including_sep() {
        assert_eq!(eval_decimal("1_000_000_007"), Ok(unn(1_000_000_007)));

        // 末尾に区切り文字を含むケース
        assert_eq!(eval_decimal("1_"), Ok(unn(1)));
    }

    // サフィックスを持つケース
    #[test]
    fn test_eval_decimal_with_suffix() {
        assert_eq!(
            eval_decimal("42i64"),
            Ok((KNumber::INN(42), KNumberTy::I64))
        );
        assert_eq!(
            eval_decimal("84_i64"),
            Ok((KNumber::INN(84), KNumberTy::I64))
        );

        assert_eq!(
            eval_decimal("42f64"),
            Ok((KNumber::FNN(42.0), KNumberTy::F64))
        );
        assert_eq!(
            eval_decimal("84_f64"),
            Ok((KNumber::FNN(84.0), KNumberTy::F64))
        );

        assert_eq!(eval_decimal("97c8"), Ok((KNumber::CNN(97), KNumberTy::C8)));
        assert_eq!(eval_decimal("65_c8"), Ok((KNumber::CNN(65), KNumberTy::C8)));
    }

    // サフィックスの精度で表現できないケース
    #[test]
    fn test_eval_decimal_with_suffix_overflow() {
        assert_eq!(eval_decimal("128_i8"), Err(LitErr::Flow));
    }

    // QUESTION: 1.0_i32 のような「小数表記に整数型のサフィックスがついていて、誤差の心配なく整数型にキャストできる」ようなリテラルをサポートするべき？ (Rust はサポートしていない)
    // 小数リテラルが整数型のサフィックスを持つケース
    #[test]
    #[ignore]
    fn test_eval_decimal_float_with_int_suffix() {
        // let value = body
        //     .parse::<f64>()
        //     .ok()
        //     .filter(|x| x.is_finite())
        //     .ok_or(LitErr::Flow)?;
        // let round = value.round();

        // let is_safe_int = {
        //     const EPS: f64 = 1e-9;
        //     const MAX_SAFE_INT: f64 = 9007199254740991.0;
        //     (value - round).abs() < EPS && 0.0 <= round.abs() + EPS && round - EPS <= MAX_SAFE_INT
        // };
        // if !is_safe_int {
        //     return Err(LitErr::Flow);
        // }

        assert_eq!(
            eval_decimal("1e9_i64"),
            Ok((KNumber::INN(1_000_000_000), KNumberTy::I64))
        );

        assert_eq!(
            eval_decimal("255.0_u8"),
            Ok((KNumber::UNN(255), KNumberTy::U8))
        );

        // 2^53-1 (= max safe-integer)
        assert_eq!(
            eval_decimal("9007199254740991.0_u64"),
            Ok((KNumber::UNN(9007199254740991), KNumberTy::U64))
        );
    }

    #[test]
    fn test_eval_decimal_with_invalid_suffix() {
        assert_eq!(eval_decimal("42.195_i64"), Err(LitErr::Flow));

        // 2^53 (> max safe-integer)
        assert_eq!(eval_decimal("9007199254740992.0_u64"), Err(LitErr::Flow));
    }

    #[test]
    fn test_eval_decimal_with_unknown_suffix() {
        assert_eq!(eval_decimal("2_ull"), Err(LitErr::UnknownSuffix));
        assert_eq!(eval_decimal("3_u88"), Err(LitErr::UnknownSuffix));
    }

    #[test]
    fn test_eval_with_fraction() {
        assert_eq!(eval_decimal("4.25"), Ok(fnn(4.25)));
        assert_eq!(eval_decimal(".25"), Ok(fnn(0.25)));
        assert_eq!(eval_decimal("4."), Ok(fnn(4.0)));
    }

    #[test]
    fn test_eval_with_eps_starting_e() {
        assert_eq!(eval_decimal("0e0"), Ok(fnn(0.0)));
        assert_eq!(eval_decimal("1e9"), Ok(fnn(1e9)));

        assert_eq!(eval_decimal("1E9"), Ok(fnn(1e9)));
        assert_eq!(eval_decimal("1e+9"), Ok(fnn(1e+9)));
        assert_eq!(eval_decimal("1e-9"), Ok(fnn(1e-9)));
    }

    #[test]
    fn test_eval_with_eps_starting_p() {
        assert_eq!(eval_decimal("0p0"), Ok(fnn(0.0)));
        assert_eq!(eval_decimal("1p9"), Ok(fnn(1e9)));

        assert_eq!(eval_decimal("1P9"), Ok(fnn(1e9)));
        assert_eq!(eval_decimal("1p+9"), Ok(fnn(1e+9)));
        assert_eq!(eval_decimal("1p-9"), Ok(fnn(1e-9)));
    }

    #[test]
    fn test_eval_decimal_as_int_as_possible_small() {
        assert_eq!(eval_decimal_as_int_as_possible("0"), Some(unn(0)));

        // 小数部があれば float
        assert_eq!(eval_decimal_as_int_as_possible("42.0"), Some(fnn(42.0)));

        // 指数部があれば float
        assert_eq!(eval_decimal_as_int_as_possible("1e7"), Some(fnn(1e7)));

        // 2^64 - 1 (= u64::MAX)
        assert_eq!(
            eval_decimal_as_int_as_possible("18446744073709551615"),
            Some(unn(18446744073709551615))
        );
    }

    #[test]
    fn test_eval_decimal_as_int_as_possible_large() {
        // 2^64 (> u64::MAX)
        assert_eq!(
            eval_decimal_as_int_as_possible("18446744073709551616"),
            Some(fnn(18446744073709552000.0))
        );
    }

    #[test]
    fn test_eval_decimal_as_float_small() {
        assert_eq!(eval_decimal_as_float("0"), Some(fnn(0.0)));

        assert_eq!(eval_decimal_as_float("42.0"), Some(fnn(42.0)));

        // 2^53 - 1 (max safe-integer)
        assert_eq!(
            eval_decimal_as_float("9007199254740991"),
            Some(fnn(9007199254740991.0))
        );
    }

    #[test]
    fn test_eval_decimal_as_float_large() {
        // 2^64 (> u64::MAX)
        assert_eq!(
            eval_decimal_as_float("18446744073709551616"),
            Some(fnn(18446744073709552000.0))
        );
    }

    #[test]
    fn test_eval_decimal_overflow() {
        // 10^1000 (> f64::MAX)
        let s = once('1').chain(repeat('0').take(1000)).collect::<String>();

        assert_eq!(eval_decimal_as_int_as_possible(&s), None);
        assert_eq!(eval_decimal_as_float(&s), None);

        assert_eq!(eval_decimal_as_int_as_possible("1e1000"), None);
        assert_eq!(eval_decimal_as_float("1e1000"), None);

        assert_eq!(eval_decimal_as_int_as_possible("1e1000"), None);
        assert_eq!(eval_decimal_as_float("1e1000"), None);
    }
}
