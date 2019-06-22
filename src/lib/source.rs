use std::collections::VecDeque;
use std::str::Chars;

pub struct Source<'a> {
  pub chs: Chars<'a>,
  pub peeked: VecDeque<char>,
  pub line: i32,
  pub column: i32,
}

pub const EOL: char = '\n';

pub fn is_line_terminator(c: char) -> bool {
  let cc = c as u32;
  cc == 0x0a || cc == 0x0d || cc == 0x2028 || cc == 0x2029
}

impl<'a> Source<'a> {
  pub fn new(code: &'a String) -> Self {
    Source {
      chs: code.chars(),
      peeked: VecDeque::with_capacity(3),
      line: 1,
      column: 0,
    }
  }

  fn next_join_crlf(&mut self) -> Option<char> {
    match self.chs.next() {
      Some(c) => {
        if is_line_terminator(c) {
          if c == '\r' {
            if let Some(c) = self.chs.next() {
              if c != '\n' {
                self.peeked.push_back(c);
              }
            }
          }
          Some(EOL)
        } else {
          Some(c)
        }
      }
      _ => None,
    }
  }

  pub fn read(&mut self) -> Option<char> {
    let c = match self.peeked.pop_front() {
      Some(c) => Some(c),
      _ => self.next_join_crlf(),
    };
    if let Some(c) = c {
      if c == EOL {
        self.line += 1;
        self.column = 0;
      }
    }
    c
  }

  pub fn peek(&mut self) -> Option<char> {
    match self.peeked.front().cloned() {
      Some(c) => Some(c),
      _ => match self.next_join_crlf() {
        Some(c) => {
          self.peeked.push_back(c);
          Some(c)
        }
        _ => None,
      },
    }
  }

  pub fn test_ahead(&mut self, ch: char) -> bool {
    match self.peek() {
      Some(c) => c == ch,
      _ => false,
    }
  }

  pub fn test_ahead_or(&mut self, c1: char, c2: char) -> bool {
    match self.peek() {
      Some(c) => c == c1 || c == c2,
      _ => false,
    }
  }

  pub fn test_ahead_chs(&mut self, chs: &[char]) -> bool {
    let mut pass = true;
    for i in 0..self.peeked.len() {
      pass = match self.peeked.get(i) {
        Some(c) => *c == chs[i],
        _ => false,
      };
      if !pass {
        return false;
      }
    }
    for i in self.peeked.len()..chs.len() {
      pass = match self.next_join_crlf() {
        Some(c) => {
          self.peeked.push_back(c);
          c == chs[i]
        }
        _ => false,
      };
      if !pass {
        return false;
      }
    }
    pass
  }

  pub fn test_ahead2(&mut self, c1: char, c2: char) -> bool {
    self.test_ahead_chs(&[c1, c2])
  }

  pub fn advance(&mut self) {
    self.read();
  }

  pub fn advance2(&mut self) {
    self.read();
    self.read();
  }
}

#[cfg(test)]
mod source_tests {
  use super::*;

  #[test]
  fn peekable_peek() {
    let code = String::from("hello world");
    let mut src = Source::new(&code);
    assert_eq!('h', src.peek().unwrap());
    assert_eq!('h', src.peek().unwrap());
    src.read();
    assert_eq!('e', src.peek().unwrap());
  }

  #[test]
  fn peekable_ahead() {
    let code = String::from("hello world");
    let mut src = Source::new(&code);
    assert!(src.test_ahead('h'));
    assert_eq!('h', src.peek().unwrap());
    assert!(src.test_ahead_chs(&['h', 'e']));
    assert_eq!('h', src.peek().unwrap());
    src.read();
    assert_eq!('e', src.peek().unwrap());
    assert!(src.test_ahead_chs(&['e', 'l', 'l']));
    src.read();
    src.read();
    src.read();
    src.read();
    assert_eq!(' ', src.peek().unwrap());
  }

  #[test]
  fn join_crlf() {
    let code = String::from("1\u{0d}\u{0a}2\u{0d}3\u{0a}");
    let mut src = Source::new(&code);
    assert!(src.test_ahead_chs(&['1', EOL]));
    src.read();
    assert!(src.test_ahead(EOL));
    assert_eq!(EOL, src.read().unwrap());
    assert_eq!((2, 0), (src.line, src.column));
    src.read();
    src.read();
    assert_eq!((3, 0), (src.line, src.column));
    src.read();
    src.read();
    assert_eq!((4, 0), (src.line, src.column));
    assert_eq!(None, src.read());
  }

  #[test]
  fn line_terminator() {
    let code = String::from("\u{2028}\u{0a}\u{0d}\u{0a}");
    let mut src = Source::new(&code);
    assert_eq!((1, 0), (src.line, src.column));
    assert_eq!(EOL, src.read().unwrap());
    assert_eq!((2, 0), (src.line, src.column));
    assert_eq!(EOL, src.read().unwrap());
    assert_eq!((3, 0), (src.line, src.column));
    assert_eq!(EOL, src.read().unwrap());
    assert_eq!((4, 0), (src.line, src.column));
  }

  #[test]
  fn peek() {
    let code = String::from("\u{2028}\u{0a}\u{0d}\u{0a}");
    let mut src = Source::new(&code);
    assert_eq!(EOL, src.peek().unwrap());
    src.read();
    assert_eq!(EOL, src.peek().unwrap());
    src.read();
    assert_eq!(EOL, src.peek().unwrap());
  }
}
