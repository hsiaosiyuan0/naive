use crate::source::*;
use crate::token::*;
use std::char;
use std::str;
use std::u32;
use unic_ucd::GeneralCategory;

pub struct Lexer<'a> {
  src: Source<'a>,
}

fn is_whitespace(c: char) -> bool {
  !is_line_terminator(c) && c.is_whitespace()
}

fn is_unicode_letter(c: char) -> bool {
  if c.is_uppercase() || c.is_lowercase() {
    return true;
  }
  match GeneralCategory::of(c) {
    GeneralCategory::TitlecaseLetter
    | GeneralCategory::ModifierLetter
    | GeneralCategory::OtherLetter
    | GeneralCategory::LetterNumber => true,
    _ => false,
  }
}

fn is_id_start(c: char) -> bool {
  if is_unicode_letter(c) {
    return true;
  }
  match c {
    '$' | '_' | '\\' => true,
    _ => false,
  }
}

fn is_id_part(c: char) -> bool {
  if is_id_start(c) {
    return true;
  }
  let cc = c as u32;
  if cc == 0x200c || cc == 0x200d {
    return true;
  }
  match GeneralCategory::of(c) {
    GeneralCategory::NonspacingMark
    | GeneralCategory::SpacingMark
    | GeneralCategory::DecimalNumber
    | GeneralCategory::ConnectorPunctuation => true,
    _ => false,
  }
}

impl<'a> Lexer<'a> {
  pub fn new(src: Source<'a>) -> Self {
    Lexer { src }
  }

  pub fn next(&mut self) -> Option<Token> {
    self.skip_whitespace();
    self.read_name()
  }

  fn read_unicode_escape_seq(&mut self) -> Option<char> {
    let mut hex = [0, 0, 0, 0];
    for i in 0..hex.len() {
      match self.src.read() {
        Some(c) => {
          // c is a invalid hex digit if it's value over the u8 max 0xff.
          // we use short-circuit here to stop farther process
          if c as u32 > 0xff {
            return None;
          } else {
            // we cannot fully ensure c is a valid hex digit, since
            // the valid range [0-9a-fA-F] is a sub range of u8 [0x0-0xff],
            // we also don't need to do overlapping check here, because we can
            // just depend on the result of `u32::from_str_radix(x, 16)` in later process
            hex[i] = c as u8;
          }
        }
        _ => return None,
      }
    }
    let hex = str::from_utf8(&hex).unwrap();
    match u32::from_str_radix(hex, 16) {
      Ok(i) => match char::from_u32(i) {
        Some(c) => Some(c),
        _ => None, // deformed unicode
      },
      _ => None, // deformed hex digits
    }
  }

  fn ahead_is_id_start(&mut self) -> bool {
    match self.src.peek() {
      Some(c) => is_id_start(c),
      _ => false,
    }
  }

  fn ahead_is_id_part(&mut self) -> bool {
    match self.src.peek() {
      Some(c) => is_id_part(c),
      _ => false,
    }
  }

  fn errmsg(&self) -> String {
    format!(
      "Unexpected char at line: {} column: {}",
      self.src.line, self.src.column
    )
  }

  // we use the prior read char as a barrier which is passed by the formal parameter `bs`,
  // if bs is `\` then we can consider the next 4 characters must be a valid unicode escaping,
  // we try to turn the valid unicode escaping to a char then return the escaped char if the turning
  // is succeed otherwise we just panic the process
  fn read_escape_unicode(&mut self, bs: char) -> char {
    if bs == '\\' && self.src.test_ahead('u') {
      self.src.advance();
      match self.read_unicode_escape_seq() {
        Some(ec) => ec,
        _ => panic!(self.errmsg()),
      }
    } else {
      bs
    }
  }

  pub fn read_name(&mut self) -> Option<Token> {
    if !self.ahead_is_id_start() {
      return None;
    }
    let mut c = self.src.read().unwrap();
    c = self.read_escape_unicode(c);
    let mut val = vec![c];
    loop {
      if self.ahead_is_id_part() {
        c = self.src.read().unwrap();
        val.push(self.read_escape_unicode(c));
      } else {
        break;
      }
    }
    Some(Token {
      kind: TokenKind::Identifier,
      value: val.iter().collect(),
      loc: Location::new(),
    })
  }

  fn skip_comment_single(&mut self) {
    self.src.advance2();
    loop {
      match self.src.read() {
        Some(EOL) | None => break,
        _ => (),
      };
    }
  }

  fn skip_comment_multi(&mut self) {
    self.src.advance2();
    loop {
      match self.src.read() {
        Some('*') => {
          if self.src.test_ahead('/') {
            self.src.advance();
            break;
          }
        }
        None => break,
        _ => (),
      };
    }
  }

  fn ahead_is_whitespace(&mut self) -> bool {
    match self.src.peek() {
      Some(c) => is_whitespace(c),
      _ => false,
    }
  }

  pub fn skip_whitespace(&mut self) {
    loop {
      if self.ahead_is_whitespace() {
        self.src.read();
      } else if self.src.test_ahead2('/', '/') {
        self.skip_comment_single();
      } else if self.src.test_ahead2('/', '*') {
        self.skip_comment_multi();
      } else {
        break;
      }
    }
  }
}

#[cfg(test)]
mod lexer_tests {
  use super::*;

  #[test]
  fn skip_whitespace() {
    let code = String::from(
      " // this is a single-line comment
       /*
        * this is a multiline comment
        */ hello world
       ",
    );
    let src = Source::new(&code);
    let mut lex = Lexer::new(src);
    lex.skip_whitespace();
    assert_eq!('h', lex.src.read().unwrap());
  }

  #[test]
  fn unicode_letter() {
    let mut c: char = '\u{01c5}'; // title case
    assert!(is_unicode_letter(c));
    c = '\u{1C90}'; // uppercase
    assert!(is_unicode_letter(c));
    c = '\u{10D0}'; // lowercase
    assert!(is_unicode_letter(c));
    c = '\u{0559}'; // modifier
    assert!(is_unicode_letter(c));
    c = '\u{0920}'; // other letter
    assert!(is_unicode_letter(c));
    c = '\u{2165}'; // letter number
    assert!(is_unicode_letter(c));
  }

  #[test]
  fn unicode_escape_seq() {
    let code = String::from("\\u01c5\\u0920\\u1x23");
    let src = Source::new(&code);
    let mut lex = Lexer::new(src);
    lex.src.advance2();
    assert_eq!('\u{01c5}', lex.read_unicode_escape_seq().unwrap());
    lex.src.advance2();
    assert_eq!('\u{0920}', lex.read_unicode_escape_seq().unwrap());
    lex.src.advance2();
    assert_eq!(None, lex.read_unicode_escape_seq());
  }

  #[test]
  fn read_name() {
    let code = String::from("\\u01c5\\u0920 a aᢅ");
    let src = Source::new(&code);
    let mut lex = Lexer::new(src);
    let mut tok = lex.read_name().unwrap();
    assert_eq!("\u{01c5}\u{0920}", tok.value);
    lex.skip_whitespace();
    tok = lex.read_name().unwrap();
    assert_eq!("a", tok.value);
    lex.skip_whitespace();
    tok = lex.read_name().unwrap();
    assert_eq!("a\u{1885}", tok.value);
  }
}
