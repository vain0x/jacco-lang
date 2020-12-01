use super::{TPos, TRange};
use std::mem::take;
use text_position_rs::TextPosition as _;

/// カーソルつき文字列をパースした結果
///
/// カーソルつき文字列は、ある文字列上の位置や範囲を分かりやすく表現するためのマークアップ記法。
/// 位置や範囲を表す以下のカーソルマーカーを使用できる:
///
/// - 位置カーソル: ある一点 (空の範囲) を表す。
///     - 名前なし: `<|>`
///     - 名前つき: `<$A|>`
/// - 範囲カーソル: カーソルマーカーの間の範囲を表す。
///     - 名前なし: `<[`...`]>`
///     - 名前つき: `<$A[`...`]>`
///
/// 範囲カーソルの入れ子や、範囲カーソルの中に位置カーソルがあるケースは未実装。
pub(crate) struct CursorText<'a> {
    text: String,
    cursors: Cursors<'a>,
}

impl<'a> CursorText<'a> {
    pub(crate) fn as_str(&self) -> &str {
        self.text.as_str()
    }

    pub(crate) fn to_pos_vec(self) -> Vec<TPos> {
        self.cursors
            .into_iter()
            .map(|(_, range)| range.start())
            .collect()
    }

    pub(crate) fn to_pos_assoc(self) -> Vec<(&'a str, TPos)> {
        self.cursors
            .into_iter()
            .map(|(name, range)| (name, range.start()))
            .collect()
    }

    pub(crate) fn to_range_vec(self) -> Vec<TRange> {
        self.cursors.into_iter().map(|(_, range)| range).collect()
    }

    pub(crate) fn to_range_assoc(self) -> Vec<(&'a str, TRange)> {
        self.cursors
    }
}

type Cursors<'a> = Vec<(&'a str, TRange)>;

enum CursorKind {
    Pos,
    Range,
}

struct CursorTextParser<'a> {
    text: &'a str,
    plain_text: String,
    pos: TPos,
    last: usize,
}

/// エラーのヒントとして、文字列 s の位置 i の少し手前の文字列を見る。
fn before(s: &str, i: usize) -> &str {
    let mut l = i.saturating_sub(12);

    // Unicode の文字を途中でぶち切らない。
    while !s.is_char_boundary(l) {
        l -= 1;
    }

    &s[l..i]
}

fn invalid_pos_cursor_error(s: &str, i: usize) -> String {
    format!(
        "位置カーソルマーカーの記法が不正です ('<|' の直後には '>' が必要) near '{}'",
        before(s, i)
    )
}

fn unclosed_range_cursor_error(s: &str, i: usize) -> String {
    format!(
        "範囲カーソルマーカーが閉じられていません near '{}'",
        before(s, i)
    )
}

fn parse_cursor_name(s: &str) -> (&str, usize) {
    if !s.starts_with('$') {
        return ("", 0);
    }

    let len = s[1..]
        .bytes()
        .take_while(|&b| (b as char).is_ascii_alphanumeric() || b == b'_')
        .count();
    (&s[1..1 + len], 1 + len)
}

/// 直前のカーソルの終了位置から位置 i までを地の文とみなす。
fn skip_plain_text(i: usize, px: &mut CursorTextParser<'_>) {
    let t = &px.text[px.last..i];
    px.plain_text += t;
    px.pos += TPos::from_str(t);
}

/// カーソルつき文字列をパースする。
pub(crate) fn parse_cursor_text(s: &str) -> Result<CursorText<'_>, String> {
    let px = &mut CursorTextParser {
        text: s,
        plain_text: String::with_capacity(s.len()),
        pos: TPos::ZERO,
        last: 0,
    };

    let mut cursors = vec![];

    // カーソルつき文字列のいま見ている位置
    let mut i = 0;

    loop {
        // `<` を探す。(カーソルマーカーなのか地の文なのかは分からない。)
        let offset = match s[i..].find("<") {
            Some(offset) => offset,
            None => break,
        };

        // `<` の位置
        let cursor_start = i + offset;

        i += offset + "<".len();

        // カーソルマーカーの名前と種類を判別する。(カーソルマーカーではなかったら continue)
        let mut cursor_name = "";
        let kind = match s.as_bytes()[i] {
            b'$' => {
                let (name, len) = parse_cursor_name(&s[i..]);
                cursor_name = name;
                i += len;

                match s.as_bytes()[i] {
                    b'|' => CursorKind::Pos,
                    b'[' => CursorKind::Range,
                    _ => continue,
                }
            }
            b'|' => CursorKind::Pos,
            b'[' => CursorKind::Range,
            _ => continue,
        };
        i += 1; // '|' or '['

        skip_plain_text(cursor_start, px);
        let start_pos = px.pos;

        let cursor_range = match kind {
            CursorKind::Pos => {
                if s.as_bytes()[i] != b'>' {
                    return Err(invalid_pos_cursor_error(s, i));
                }
                i += ">".len();

                TRange::empty(start_pos)
            }
            CursorKind::Range => {
                px.last = i;

                let offset = match s[i..].find("]>") {
                    Some(offset) => offset,
                    None => return Err(unclosed_range_cursor_error(s, cursor_start)),
                };

                skip_plain_text(i + offset, px);
                let end_pos = px.pos;
                i += offset + "]>".len();

                TRange::from(start_pos..end_pos)
            }
        };

        cursors.push((cursor_name, cursor_range));
        px.last = i;
    }

    skip_plain_text(s.len(), px);

    Ok(CursorText {
        text: take(&mut px.plain_text),
        cursors,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pos_of(s: &str) -> TPos {
        TPos::from_str(s)
    }

    #[test]
    fn test_anonymous_pos_cursors() {
        let text = "first <|>line\nsecond <|>line\n";
        let result = parse_cursor_text(text).unwrap();
        assert_eq!(result.as_str(), "first line\nsecond line\n");
        assert_eq!(
            result.to_pos_vec(),
            vec![TPos::new(6, 0, 6, 6), TPos::new(18, 1, 7, 7)]
        );
    }

    #[test]
    fn test_named_pos_cursors() {
        let text = "first <$A|>line\nsecond <$B|>line\n";
        let result = parse_cursor_text(text).unwrap();
        assert_eq!(result.as_str(), "first line\nsecond line\n");
        assert_eq!(
            result.to_pos_assoc(),
            vec![
                ("A", pos_of("first ")),
                ("B", pos_of("first line\nsecond "))
            ]
        );
    }

    #[test]
    fn test_anonymous_range_cursors() {
        let text = "first <[line]>\nsecond <[line\nthird]> line\n";
        let result = parse_cursor_text(text).unwrap();
        assert_eq!(result.as_str(), "first line\nsecond line\nthird line\n");
        assert_eq!(
            result.to_range_vec(),
            vec![
                TRange::from(pos_of("first ")..pos_of("first line")),
                TRange::from(
                    pos_of("first line\nsecond ")..pos_of("first line\nsecond line\nthird")
                ),
            ]
        );
    }

    #[test]
    fn test_named_range_cursors() {
        let text = "first <$A[line]>\nsecond <$B[line\nthird]> line\n";
        let result = parse_cursor_text(text).unwrap();
        assert_eq!(result.as_str(), "first line\nsecond line\nthird line\n");
        assert_eq!(
            result.to_range_assoc(),
            vec![
                ("A", TRange::from(pos_of("first ")..pos_of("first line"))),
                (
                    "B",
                    TRange::from(
                        pos_of("first line\nsecond ")..pos_of("first line\nsecond line\nthird")
                    )
                ),
            ]
        );
    }

    #[test]
    fn test_invalid_pos_cursor_syntax_error() {
        let text = "こんにちは <| せかい";
        match parse_cursor_text(text) {
            Ok(_) => panic!(),
            Err(_) => (),
        }
    }

    #[test]
    fn test_unclosed_range_cursor_error() {
        let text = "こんにちは <[ せかい";
        match parse_cursor_text(text) {
            Ok(_) => panic!(),
            Err(_) => (),
        }
    }
}
