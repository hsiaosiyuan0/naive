use core::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::sync::{Once, ONCE_INIT};

pub struct Location {
  pub line: i32,
  pub column: i32,
}

impl Location {
  pub fn new() -> Self {
    Location { line: 0, column: 0 }
  }
}

pub struct KeywordData {
  pub kind: Keyword,
  pub loc: Location,
}
#[derive(Eq, PartialEq, Hash)]
pub enum Keyword {
  Break,
  Do,
  Instanceof,
  Typeof,
  Case,
  Else,
  New,
  Var,
  Catch,
  Finally,
  Return,
  Void,
  Continue,
  For,
  Switch,
  While,
  Debugger,
  Function,
  This,
  With,
  Default,
  If,
  Throw,
  Delete,
  In,
  Try,
  // future reserved words
  Class,
  Enum,
  Extends,
  Super,
  Const,
  Export,
  Import,
}

macro_rules! gen_map {
  ($($k:expr => $v:expr)*) => {
    {
      let mut s = HashSet::new();
      $(
        s.insert($v);
      )*
      let mut kv = HashMap::new();
      $(
        kv.insert($k, $v);
      )*
      let mut vk = HashMap::new();
      $(
        vk.insert($v, $k);
      )*
      (Some(s), Some(kv), Some(vk))
    }
  };
}

static mut KEYWORDS_SET: Option<HashSet<&'static str>> = None;
static mut KEYWORDS_KEY_NAME: Option<HashMap<Keyword, &'static str>> = None;
static mut KEYWORDS_NAME_KEY: Option<HashMap<&'static str, Keyword>> = None;
fn init_keywords() {
  unsafe {
    let (s, kv, vk) = gen_map! {
      Keyword::Break => "break"
      Keyword::Do => "do"
      Keyword::Instanceof => "instanceof"
      Keyword::Typeof => "typeof"
      Keyword::Case => "case"
      Keyword::Else => "else"
      Keyword::New => "new"
      Keyword::Var => "var"
      Keyword::Catch => "catch"
      Keyword::Finally => "finally"
      Keyword::Return => "return"
      Keyword::Void => "void"
      Keyword::Continue => "continue"
      Keyword::For => "for"
      Keyword::Switch => "switch"
      Keyword::While => "while"
      Keyword::Debugger => "debugger"
      Keyword::Function => "function"
      Keyword::This => "this"
      Keyword::With => "with"
      Keyword::Default => "default"
      Keyword::If => "if"
      Keyword::Throw => "throw"
      Keyword::Delete => "delete"
      Keyword::In => "in"
      Keyword::Try => "try"
      // future reserved words
      Keyword::Class => "class"
      Keyword::Enum => "enum"
      Keyword::Extends => "extends"
      Keyword::Super => "super"
      Keyword::Const => "const"
      Keyword::Export => "export"
      Keyword::Import => "import"
    };
    KEYWORDS_SET = s;
    KEYWORDS_KEY_NAME = kv;
    KEYWORDS_NAME_KEY = vk;
  }
}
fn is_keyword(s: &str) -> bool {
  unsafe { KEYWORDS_SET.as_ref().unwrap().contains(s) }
}
fn keyword_to_name(v: &Keyword) -> &'static str {
  unsafe { KEYWORDS_KEY_NAME.as_ref().unwrap().get(v).unwrap() }
}
fn name_to_keyword(s: &str) -> &Keyword {
  unsafe { KEYWORDS_NAME_KEY.as_ref().unwrap().get(s).unwrap() }
}
impl Keyword {
  pub fn name(&self) -> &'static str {
    keyword_to_name(self)
  }
}

#[derive(Eq, PartialEq, Hash)]
pub enum Symbol {
  BraceL,
  BraceR,
  ParenL,
  ParenR,
  BracketL,
  BracketR,
  Dot,
  Semi,
  Comma,
  LT,
  GT,
  LE,
  GE,
  Eq,
  NotEq,
  EqStrict,
  NotEqStrict,
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  Inc,
  Dec,
  SHL,
  SAR,
  SHR,
  BitAnd,
  BitOr,
  BitXor,
  Not,
  BitNot,
  And,
  Or,
  Conditional,
  Colon,
  Assign,
  AssignAdd,
  AssignSub,
  AssignMul,
  AssignDiv,
  AssignMod,
  AssignSHL,
  AssignSAR,
  AssignSHR,
  AssignBitAnd,
  AssignBitOr,
  AssignBitXor,
}
pub struct SymbolData {
  pub kind: Symbol,
  pub loc: Location,
}

static mut SYMBOLS_SET: Option<HashSet<&'static str>> = None;
static mut SYMBOLS_KEY_NAME: Option<HashMap<Symbol, &'static str>> = None;
static mut SYMBOLS_NAME_KEY: Option<HashMap<&'static str, Symbol>> = None;
fn init_symbols() {
  unsafe {
    let (s, kv, vk) = gen_map! {
      Symbol::BraceL => "{"
      Symbol::BraceR => "}"
      Symbol::ParenL => "("
      Symbol::ParenR => ")"
      Symbol::BracketL => "["
      Symbol::BracketR => "]"
      Symbol::Dot => "."
      Symbol::Semi => ";"
      Symbol::Comma => ""
      Symbol::LT => "<"
      Symbol::GT => ">"
      Symbol::LE => "<="
      Symbol::GE => ">="
      Symbol::Eq => "=="
      Symbol::NotEq => "!="
      Symbol::EqStrict => "==="
      Symbol::NotEqStrict => "!=="
      Symbol::Add => "+"
      Symbol::Sub => "-"
      Symbol::Mul => "*"
      Symbol::Div => "/"
      Symbol::Mod => "%"
      Symbol::Inc => "++"
      Symbol::Dec => "--"
      Symbol::SHL => "<<"
      Symbol::SAR => ">>"
      Symbol::SHR => ">>>"
      Symbol::BitAnd => "&"
      Symbol::BitOr => "|"
      Symbol::BitXor => "^"
      Symbol::Not => "!"
      Symbol::BitNot => "~"
      Symbol::And => "&&"
      Symbol::Or => "||"
      Symbol::Conditional => "?"
      Symbol::Colon => ":"
      Symbol::Assign => "="
      Symbol::AssignAdd => "+="
      Symbol::AssignSub => "-="
      Symbol::AssignMul => "*="
      Symbol::AssignDiv => "/="
      Symbol::AssignMod => "%="
      Symbol::AssignSHL => "<<="
      Symbol::AssignSAR => ">>="
      Symbol::AssignSHR => ">>>="
      Symbol::AssignBitAnd => "&="
      Symbol::AssignBitOr => "|="
      Symbol::AssignBitXor => "^="
    };
    SYMBOLS_SET = s;
    SYMBOLS_KEY_NAME = kv;
    SYMBOLS_NAME_KEY = vk;
  }
}
fn is_symbol(s: &str) -> bool {
  unsafe { SYMBOLS_SET.as_ref().unwrap().contains(s) }
}
fn symbol_to_name(v: &Symbol) -> &'static str {
  unsafe { SYMBOLS_KEY_NAME.as_ref().unwrap().get(v).unwrap() }
}
fn name_to_symbol(s: &str) -> &Symbol {
  unsafe { SYMBOLS_NAME_KEY.as_ref().unwrap().get(s).unwrap() }
}
impl Symbol {
  pub fn name(&self) -> &'static str {
    symbol_to_name(self)
  }
}

#[derive(Eq, PartialEq, Hash)]
pub enum ContextualKeyword {
  Implements,
  Let,
  Private,
  Public,
  Interface,
  Package,
  Protected,
  Static,
  Yield,
}
pub struct ContextualKeywordData {
  pub kind: Symbol,
  pub loc: Location,
}

static mut CONTEXTUAL_KEYWORD_SET: Option<HashSet<&'static str>> = None;
static mut CONTEXTUAL_KEYWORD_KEY_NAME: Option<HashMap<ContextualKeyword, &'static str>> = None;
static mut CONTEXTUAL_KEYWORD_NAME_KEY: Option<HashMap<&'static str, ContextualKeyword>> = None;
fn init_contextual_keyword() {
  unsafe {
    let (s, kv, vk) = gen_map! {
      ContextualKeyword::Implements => "implements"
      ContextualKeyword::Let => "let"
      ContextualKeyword::Private => "private"
      ContextualKeyword::Public => "public"
      ContextualKeyword::Interface => "interface"
      ContextualKeyword::Package => "package"
      ContextualKeyword::Protected => "protected"
      ContextualKeyword::Static => "static"
      ContextualKeyword::Yield => "yield"

    };
    CONTEXTUAL_KEYWORD_SET = s;
    CONTEXTUAL_KEYWORD_KEY_NAME = kv;
    CONTEXTUAL_KEYWORD_NAME_KEY = vk;
  }
}
fn is_contextual_keyword(s: &str) -> bool {
  unsafe { CONTEXTUAL_KEYWORD_SET.as_ref().unwrap().contains(s) }
}
fn contextual_keyword_to_name(v: &ContextualKeyword) -> &'static str {
  unsafe {
    CONTEXTUAL_KEYWORD_KEY_NAME
      .as_ref()
      .unwrap()
      .get(v)
      .unwrap()
  }
}
fn name_to_contextual_keyword(s: &str) -> &ContextualKeyword {
  unsafe {
    CONTEXTUAL_KEYWORD_NAME_KEY
      .as_ref()
      .unwrap()
      .get(s)
      .unwrap()
  }
}
impl ContextualKeyword {
  pub fn name(&self) -> &'static str {
    contextual_keyword_to_name(self)
  }
}

pub struct IdentifierData {
  pub value: String,
  pub loc: Location,
}

pub struct NullLiteralData {
  pub loc: Location,
}

pub enum BooleanLiteral {
  True,
  False,
}
pub struct BooleanLiteralData {
  pub kind: BooleanLiteral,
  pub loc: Location,
}

pub struct StringLiteralData {
  pub value: String,
  pub loc: Location,
}

pub struct NumericLiteralData {
  pub value: String,
  pub loc: Location,
}

pub struct RegExpLiteralData {
  pub value: String,
  pub loc: Location,
}

pub enum Token {
  Keyword(KeywordData),
  Symbol(SymbolData),
  ContextualKeyword(ContextualKeywordData),
  Identifier(IdentifierData),
  NullLiteral(NullLiteralData),
  BooleanLiteral(BooleanLiteralData),
  StringLiteral(StringLiteralData),
  NumericLiteral(NumericLiteralData),
  RegExpLiteral(RegExpLiteralData),
}

static INIT_TOKEN_DATA_ONCE: Once = Once::new();
pub fn init_token_data() {
  INIT_TOKEN_DATA_ONCE.call_once(|| {
    init_keywords();
    init_symbols();
    init_contextual_keyword();
  });
}

#[cfg(test)]
mod token_tests {
  use super::*;

  #[test]
  fn name() {
    init_token_data();
    assert_eq!("break", Keyword::Break.name());
    assert_eq!("}", Symbol::BraceR.name());
    assert_eq!("yield", ContextualKeyword::Yield.name());
  }

  #[test]
  fn static_map_set() {
    init_token_data();
    assert!(is_keyword("break"));
    assert!(is_symbol("{"));
    assert!(is_contextual_keyword("implements"));
  }
}
