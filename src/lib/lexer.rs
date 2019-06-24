use crate::source::*;
use crate::token::*;
use std::char;
use std::str;
use std::u32;
use unic_ucd::GeneralCategory;

pub struct Lexer<'a> {
  src: Source<'a>,
}

pub struct LexError {
  msg: String,
}

impl LexError {
  fn new(msg: String) -> Self {
    LexError { msg }
  }
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

fn is_single_escape_ch(c: char) -> bool {
  match c {
    '\'' | '"' | '\\' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' => true,
    _ => false,
  }
}

fn escape_ch(c: char) -> char {
  match c {
    '\'' => '\'',
    '"' => '"',
    '\\' => '\\',
    'b' => '\x08',
    'f' => '\x0c',
    'n' => '\x0a',
    'r' => '\x0d',
    't' => '\x09',
    'v' => '\x0b',
    _ => panic!(),
  }
}

fn is_non_escape_ch(c: char) -> bool {
  !is_single_escape_ch(c) && !is_line_terminator(c) && !c.is_ascii_digit() && c != 'x' && c != 'u'
}

impl<'a> Lexer<'a> {
  pub fn new(src: Source<'a>) -> Self {
    Lexer { src }
  }

  //  pub fn next(&mut self) -> Option<Token> {
  //    self.skip_whitespace();
  //    self.read_name()
  //  }

  fn read_unicode_escape_seq(&mut self) -> Option<char> {
    let mut hex = [0, 0, 0, 0];
    for i in 0..hex.len() {
      match self.src.read() {
        Some(c) => {
          if c.is_ascii_hexdigit() {
            hex[i] = c as u8;
          } else {
            return None;
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
  // is succeed otherwise a lexer error is returned
  fn read_escape_unicode(&mut self, bs: char) -> Result<char, LexError> {
    if bs == '\\' && self.src.test_ahead('u') {
      self.src.advance();
      match self.read_unicode_escape_seq() {
        Some(ec) => Ok(ec),
        _ => Err(LexError::new(self.errmsg())),
      }
    } else {
      Ok(bs)
    }
  }

  pub fn read_name(&mut self) -> Result<Token, LexError> {
    let mut c = self.src.read().unwrap();
    match self.read_escape_unicode(c) {
      Ok(cc) => c = cc,
      Err(e) => return Err(e),
    }
    let mut val = vec![c];
    loop {
      if self.ahead_is_id_part() {
        c = self.src.read().unwrap();
        match self.read_escape_unicode(c) {
          Ok(cc) => val.push(cc),
          Err(e) => return Err(e),
        }
      } else {
        break;
      }
    }
    Ok(Token::Identifier(IdentifierData {
      value: val.iter().collect(),
      loc: Location::new(),
    }))
  }

  fn read_decimal_digits(&mut self) -> String {
    let mut ret = String::new();
    loop {
      if let Some(c) = self.src.peek() {
        if c.is_ascii_digit() {
          ret.push(self.src.read().unwrap());
          continue;
        }
      }
      break;
    }
    ret
  }

  fn read_exponent(&mut self) -> Result<String, LexError> {
    let mut ret = String::new();
    // consume e|E
    ret.push(self.src.read().unwrap());
    if let Some(c) = self.src.peek() {
      if c == '+' || c == '-' {
        ret.push(self.src.read().unwrap());
      }
      let digits = self.read_decimal_digits();
      if digits.is_empty() {
        return Err(LexError::new(self.errmsg()));
      } else {
        ret.push_str(digits.as_str());
      }
    }
    Ok(ret)
  }

  fn read_decimal_int_part(&mut self) -> String {
    let mut ret = String::new();
    let c = self.src.read().unwrap();
    ret.push(c);
    if c == '0' {
      return ret;
    }
    ret.push_str(self.read_decimal_digits().as_str());
    ret
  }

  fn read_decimal(&mut self) -> Result<String, LexError> {
    let c = self.src.peek().unwrap();
    let mut ret = String::new();
    let digits_opt = c != '.';
    if c.is_ascii_digit() {
      ret.push_str(self.read_decimal_int_part().as_str());
    }
    // here we process the fractional part
    // if decimal starts with dot then next digits is required to be present
    // if decimal starts non-zero digit then the digits in fractional part is optional
    if self.src.test_ahead('.') {
      ret.push(self.src.read().unwrap());
      let digits = self.read_decimal_digits();
      if digits.is_empty() && !digits_opt {
        return Err(LexError::new(self.errmsg()));
      }
      ret.push_str(digits.as_str());
    }
    if self.src.test_ahead_or('e', 'E') {
      match self.read_exponent() {
        Ok(s) => ret.push_str(s.as_str()),
        err @ Err(_) => return err,
      }
    }

    Ok(ret)
  }

  fn read_hex(&mut self) -> Result<String, LexError> {
    let mut ret = String::new();
    ret.push(self.src.read().unwrap());
    ret.push(self.src.read().unwrap());
    let mut digits = vec![];
    loop {
      match self.src.peek() {
        Some(c) => {
          if c.is_ascii_hexdigit() {
            digits.push(self.src.read().unwrap());
          } else {
            break;
          }
        }
        _ => break,
      }
    }
    if digits.len() == 0 {
      Err(LexError::new(self.errmsg()))
    } else {
      let digits: String = digits.iter().collect();
      ret.push_str(digits.as_str());
      Ok(ret)
    }
  }

  fn ahead_is_decimal_int(&mut self) -> bool {
    if let Some(c) = self.src.peek() {
      c != '0' && c.is_ascii_digit() || c == '.'
    } else {
      false
    }
  }

  pub fn read_numeric(&mut self) -> Result<Token, LexError> {
    let value: Result<String, LexError>;
    let mut is_hex = false;
    if self.src.test_ahead('0') {
      if let Some(c) = self.src.chs.next() {
        self.src.peeked.push_back(c);
        if c == 'x' || c == 'X' {
          is_hex = true;
        }
      }
    }
    if is_hex {
      value = self.read_hex();
    } else {
      value = self.read_decimal();
    }
    match value {
      Ok(v) => Ok(Token::NumericLiteral(NumericLiteralData {
        value: v,
        loc: Location::new(),
      })),
      Err(e) => Err(e),
    }
  }

  fn read_string_escape_seq(&mut self) -> Result<Option<char>, LexError> {
    self.src.advance(); // consume `\`
    match self.src.read() {
      Some(mut c) => {
        if is_single_escape_ch(c) {
          c = escape_ch(c);
        } else if c == '0' {
          c = '\0';
          // 0 [lookahead ∉ DecimalDigit]
          if let Some(c) = self.src.peek() {
            if c.is_ascii_digit() {
              return Err(LexError::new(self.errmsg()));
            }
          }
        } else if c == 'x' {
          let mut hex = [0, 0];
          for i in 0..hex.len() {
            if let Some(cc) = self.src.read() {
              if cc.is_ascii_hexdigit() {
                hex[i] = cc as u8;
                continue;
              }
            }
            return Err(LexError::new(self.errmsg()));
          }
          // we already check each char is a valid hex digit
          // so the entire hex digits can be safely converted to u32
          let hex = str::from_utf8(&hex).unwrap();
          c = char::from_u32(u32::from_str_radix(hex, 16).ok().unwrap()).unwrap()
        } else if c == 'u' {
          match self.read_unicode_escape_seq() {
            Some(ec) => c = ec,
            _ => return Err(LexError::new(self.errmsg())),
          }
        } else if is_line_terminator(c) {
          // <CR> [lookahead ∉ <LF> ]
          if c == '\r' && self.src.test_ahead('\n') {
            self.src.advance();
            return Err(LexError::new(self.errmsg()));
          }
          // here we meet the line continuation symbol, just remove it from source stream
          return Ok(None);
        } else if is_non_escape_ch(c) {
          // do nothing
        } else {
          return Err(LexError::new(self.errmsg()));
        }
        Ok(Some(c))
      }
      _ => Err(LexError::new(self.errmsg())),
    }
  }

  fn read_string(&mut self, t: char) -> Result<Token, LexError> {
    let mut ret = String::new();
    loop {
      match self.src.peek() {
        Some(c) => {
          if c == t {
            self.src.advance();
            break;
          } else if c == '\\' {
            match self.read_string_escape_seq() {
              Ok(Some(c)) => ret.push(c),
              Err(e) => return Err(e),
              _ => (),
            }
          } else {
            ret.push(self.src.read().unwrap());
          }
        }
        _ => break,
      }
    }
    Ok(Token::StringLiteral(StringLiteralData {
      value: ret,
      loc: Location::new(),
    }))
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
    let mut tok = lex.read_name().ok().unwrap();
    assert_eq!("\u{01c5}\u{0920}", tok.id_data().unwrap().value);

    lex.skip_whitespace();
    tok = lex.read_name().ok().unwrap();
    assert_eq!("a", tok.id_data().unwrap().value);

    lex.skip_whitespace();
    tok = lex.read_name().ok().unwrap();
    assert_eq!("a\u{1885}", tok.id_data().unwrap().value);
  }

  #[test]
  fn read_decimal() {
    let code = String::from("1 .1e1 1.e1 1.e+1 .1e-1");
    let src = Source::new(&code);
    let mut lex = Lexer::new(src);
    let mut val = lex.read_decimal().ok().unwrap();
    assert_eq!("1", val);
    lex.skip_whitespace();
    val = lex.read_decimal().ok().unwrap();
    assert_eq!(".1e1", val);
    lex.skip_whitespace();
    val = lex.read_decimal().ok().unwrap();
    assert_eq!("1.e1", val);
    lex.skip_whitespace();
    val = lex.read_decimal().ok().unwrap();
    assert_eq!("1.e+1", val);
    lex.skip_whitespace();
    val = lex.read_decimal().ok().unwrap();
    assert_eq!(".1e-1", val);
  }

  #[test]
  fn read_numeric() {
    let code = String::from("1 .1e1 0xa1 0X123");
    let src = Source::new(&code);
    let mut lex = Lexer::new(src);
    let mut tok = lex.read_numeric().ok().unwrap();
    assert_eq!("1", tok.num_literal_data().unwrap().value);

    lex.skip_whitespace();
    tok = lex.read_numeric().ok().unwrap();
    assert_eq!(".1e1", tok.num_literal_data().unwrap().value);

    lex.skip_whitespace();
    tok = lex.read_numeric().ok().unwrap();
    assert_eq!("0xa1", tok.num_literal_data().unwrap().value);

    lex.skip_whitespace();
    tok = lex.read_numeric().ok().unwrap();
    assert_eq!("0X123", tok.num_literal_data().unwrap().value);
  }

  #[test]
  fn read_string() {
    let code = String::from("'hello world' \"hello \\\n\\u4E16\\u754C\"");
    let src = Source::new(&code);
    let mut lex = Lexer::new(src);
    lex.src.advance();
    let mut tok = lex.read_string('\'').ok().unwrap();
    if let Token::StringLiteral(s) = tok {
      assert_eq!("hello world", s.value);
    }

    lex.skip_whitespace();
    lex.src.advance();
    tok = lex.read_string('"').ok().unwrap();
    if let Token::StringLiteral(s) = tok {
      assert_eq!("hello 世界", s.value);
    }
  }
}
