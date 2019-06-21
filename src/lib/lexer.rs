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
    '$' | '_' => true,
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

  pub fn read_unicode_escape_seq(&mut self) -> Option<char> {
    if !self.src.test_ahead2('\\', 'u') {
      return None;
    }
    self.src.advance2();
    let mut hex = [0, 0, 0, 0];
    for i in 0..hex.len() {
      match self.src.read() {
        Some(c) => {
          // if c is larger than u8 max value,
          // it must be invalid hex digit.
          // we short circuit here to stop farther process
          if c as u32 > 0xff {
            return None;
          } else {
            // we cannot fully ensure c is a valid hex digit
            // just save it and depend on the result of
            // `u32::from_str_radix(x, 16)`
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

  pub fn read_name(&mut self) -> Option<Token> {
    None
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
    assert_eq!('\u{01c5}', lex.read_unicode_escape_seq().unwrap());
    assert_eq!('\u{0920}', lex.read_unicode_escape_seq().unwrap());
    assert_eq!(None, lex.read_unicode_escape_seq());
  }
}
