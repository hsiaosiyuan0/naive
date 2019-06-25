use std::collections::{HashMap, HashSet};
use std::sync::Once;

pub struct Position {
  pub line: i32,
  pub column: i32,
}

impl Position {
  pub fn new() -> Position {
    Position { line: 0, column: 0 }
  }
}

pub struct SourceLoc {
  pub start: Position,
  pub end: Position,
}

pub struct KeywordData {
  pub kind: Keyword,
  pub loc: SourceLoc,
}
#[derive(Eq, PartialEq, Hash, Copy, Clone)]
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

// `$be` denotes the `beforeExpr` attribute of token.
// Since the grammar of regexp of js is a island grammar when see it at the point of view of whole js grammar,
// we have two opportunities to process that island grammar, either at tokenizing phase or parsing phase.
//
// Here we use a manner which is taken from [acorn](https://github.com/acornjs/acorn/blob/master/acorn/src/tokentype.js),
// it attaches a `beforeExpr` attribute to each token type to indicate that the slashes after those tokens
// would be the beginning of regexp if the value of their `beforeExpr` attributes are `true`,
// it works at tokenizing phase therefore it can obey the definition of regexp to produce RegExprLiteral tokens.
macro_rules! gen_map {
  ($($k:expr => $v:expr, $be:expr)*) => {
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
      let mut be = HashSet::new();
      $(
        be.insert($k);
      )*
      (Some(s), Some(kv), Some(vk), Some(be))
    }
  };
}

static mut KEYWORDS_SET: Option<HashSet<&'static str>> = None;
static mut KEYWORDS_KEY_NAME: Option<HashMap<Keyword, &'static str>> = None;
static mut KEYWORDS_NAME_KEY: Option<HashMap<&'static str, Keyword>> = None;
static mut KEYWORDS_BEFORE_EXPR_SET: Option<HashSet<Keyword>> = None;
fn init_keywords() {
  let (s, kv, vk, be) = gen_map! {
    Keyword::Break => "break", false
    Keyword::Do => "do", true
    Keyword::Instanceof => "instanceof", true
    Keyword::Typeof => "typeof", true
    Keyword::Case => "case", true
    Keyword::Else => "else", true
    Keyword::New => "new", true
    Keyword::Var => "var", false
    Keyword::Catch => "catch", false
    Keyword::Finally => "finally", false
    Keyword::Return => "return", true
    Keyword::Void => "void", true
    Keyword::Continue => "continue", false
    Keyword::For => "for", false
    Keyword::Switch => "switch", false
    Keyword::While => "while", false
    Keyword::Debugger => "debugger", false
    Keyword::Function => "function", false
    Keyword::This => "this", false
    Keyword::With => "with", false
    Keyword::Default => "default", true
    Keyword::If => "if", false
    Keyword::Throw => "throw", true
    Keyword::Delete => "delete", true
    Keyword::In => "in", true
    Keyword::Try => "try", false
    // future reserved words
    Keyword::Class => "class", false
    Keyword::Enum => "enum", false
    Keyword::Extends => "extends", true
    Keyword::Super => "super", false
    Keyword::Const => "const", false
    Keyword::Export => "export", false
    Keyword::Import => "import", false
  };
  unsafe {
    KEYWORDS_SET = s;
    KEYWORDS_KEY_NAME = kv;
    KEYWORDS_NAME_KEY = vk;
    KEYWORDS_BEFORE_EXPR_SET = be;
  }
}
pub fn is_keyword(s: &str) -> bool {
  unsafe { KEYWORDS_SET.as_ref().unwrap().contains(s) }
}
pub fn keyword_to_name(v: &Keyword) -> &'static str {
  unsafe { KEYWORDS_KEY_NAME.as_ref().unwrap().get(v).unwrap() }
}
pub fn name_to_keyword(s: &str) -> Keyword {
  unsafe {
    KEYWORDS_NAME_KEY
      .as_ref()
      .unwrap()
      .get(s)
      .unwrap()
      .to_owned()
  }
}
impl Keyword {
  pub fn name(&self) -> &'static str {
    keyword_to_name(self)
  }

  pub fn is_before_expr(&self) -> bool {
    unsafe { KEYWORDS_BEFORE_EXPR_SET.as_ref().unwrap().contains(self) }
  }
}

#[derive(Eq, PartialEq, Hash, Copy, Clone)]
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
  pub loc: SourceLoc,
}

static mut SYMBOLS_SET: Option<HashSet<&'static str>> = None;
static mut SYMBOLS_KEY_NAME: Option<HashMap<Symbol, &'static str>> = None;
static mut SYMBOLS_NAME_KEY: Option<HashMap<&'static str, Symbol>> = None;
static mut SYMBOLS_BEFORE_EXPR_SET: Option<HashSet<Symbol>> = None;
fn init_symbols() {
  let (s, kv, vk, be) = gen_map! {
    Symbol::BraceL => "{", true
    Symbol::BraceR => "}", false
    Symbol::ParenL => "(", true
    Symbol::ParenR => ")", false
    Symbol::BracketL => "[", true
    Symbol::BracketR => "]", false
    Symbol::Dot => ".", true
    Symbol::Semi => ";", true
    Symbol::Comma => ",", true
    Symbol::LT => "<", true
    Symbol::GT => ">", true
    Symbol::LE => "<=", true
    Symbol::GE => ">=", true
    Symbol::Eq => "==", true
    Symbol::NotEq => "!=", true
    Symbol::EqStrict => "===", true
    Symbol::NotEqStrict => "!==", true
    Symbol::Add => "+", true
    Symbol::Sub => "-", true
    Symbol::Mul => "*", true
    Symbol::Div => "/", true
    Symbol::Mod => "%", true
    Symbol::Inc => "++", false
    Symbol::Dec => "--", false
    Symbol::SHL => "<<", true
    Symbol::SAR => ">>", true
    Symbol::SHR => ">>>", true
    Symbol::BitAnd => "&", true
    Symbol::BitOr => "|", true
    Symbol::BitXor => "^", true
    Symbol::Not => "!", true
    Symbol::BitNot => "~", true
    Symbol::And => "&&", true
    Symbol::Or => "||", true
    Symbol::Conditional => "?", true
    Symbol::Colon => ":", true
    Symbol::Assign => "=", true
    Symbol::AssignAdd => "+=", true
    Symbol::AssignSub => "-=", true
    Symbol::AssignMul => "*=", true
    Symbol::AssignDiv => "/=", true
    Symbol::AssignMod => "%=", true
    Symbol::AssignSHL => "<<=", true
    Symbol::AssignSAR => ">>=", true
    Symbol::AssignSHR => ">>>=", true
    Symbol::AssignBitAnd => "&=", true
    Symbol::AssignBitOr => "|=", true
    Symbol::AssignBitXor => "^=", true
  };
  unsafe {
    SYMBOLS_SET = s;
    SYMBOLS_KEY_NAME = kv;
    SYMBOLS_NAME_KEY = vk;
    SYMBOLS_BEFORE_EXPR_SET = be;
  }
}
pub fn is_symbol(s: &str) -> bool {
  unsafe { SYMBOLS_SET.as_ref().unwrap().contains(s) }
}
pub fn symbol_to_name(v: &Symbol) -> &'static str {
  unsafe { SYMBOLS_KEY_NAME.as_ref().unwrap().get(v).unwrap() }
}
pub fn name_to_symbol(s: &str) -> Symbol {
  unsafe {
    SYMBOLS_NAME_KEY
      .as_ref()
      .unwrap()
      .get(s)
      .unwrap()
      .to_owned()
  }
}
impl Symbol {
  pub fn name(&self) -> &'static str {
    symbol_to_name(self)
  }

  pub fn is_before_expr(&self) -> bool {
    unsafe { SYMBOLS_BEFORE_EXPR_SET.as_ref().unwrap().contains(self) }
  }
}

#[derive(Eq, PartialEq, Hash, Copy, Clone)]
pub enum CtxKeyword {
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
pub struct CtxKeywordData {
  pub kind: CtxKeyword,
  pub loc: SourceLoc,
}

static mut CTX_KEYWORD_SET: Option<HashSet<&'static str>> = None;
static mut CTX_KEYWORD_KEY_NAME: Option<HashMap<CtxKeyword, &'static str>> = None;
static mut CTX_KEYWORD_NAME_KEY: Option<HashMap<&'static str, CtxKeyword>> = None;
static mut CTX_KEYWORD_BEFORE_EXPR_SET: Option<HashSet<CtxKeyword>> = None;
fn init_ctx_keyword() {
  let (s, kv, vk, be) = gen_map! {
    CtxKeyword::Implements => "implements", false
    CtxKeyword::Let => "let", false
    CtxKeyword::Private => "private", false
    CtxKeyword::Public => "public", false
    CtxKeyword::Interface => "interface", false
    CtxKeyword::Package => "package", false
    CtxKeyword::Protected => "protected", false
    CtxKeyword::Static => "static", false
    CtxKeyword::Yield => "yield", true

  };
  unsafe {
    CTX_KEYWORD_SET = s;
    CTX_KEYWORD_KEY_NAME = kv;
    CTX_KEYWORD_NAME_KEY = vk;
    CTX_KEYWORD_BEFORE_EXPR_SET = be;
  }
}
pub fn is_ctx_keyword(s: &str) -> bool {
  unsafe { CTX_KEYWORD_SET.as_ref().unwrap().contains(s) }
}
pub fn ctx_keyword_to_name(v: &CtxKeyword) -> &'static str {
  unsafe { CTX_KEYWORD_KEY_NAME.as_ref().unwrap().get(v).unwrap() }
}
pub fn name_to_ctx_keyword(s: &str) -> CtxKeyword {
  unsafe {
    CTX_KEYWORD_NAME_KEY
      .as_ref()
      .unwrap()
      .get(s)
      .unwrap()
      .to_owned()
  }
}
impl CtxKeyword {
  pub fn name(&self) -> &'static str {
    ctx_keyword_to_name(self)
  }

  pub fn is_before_expr(&self) -> bool {
    unsafe { CTX_KEYWORD_BEFORE_EXPR_SET.as_ref().unwrap().contains(self) }
  }
}

pub struct IdentifierData {
  pub value: String,
  pub loc: SourceLoc,
}

pub struct NullLiteralData {
  pub loc: SourceLoc,
}
pub fn is_null(s: &str) -> bool {
  s == "null"
}

pub enum BooleanLiteral {
  True,
  False,
}
pub struct BooleanLiteralData {
  pub kind: BooleanLiteral,
  pub loc: SourceLoc,
}
pub fn is_bool(s: &str) -> bool {
  s == "true" || s == "false"
}
pub fn name_to_bool(s: &str) -> BooleanLiteral {
  match s {
    "true" => BooleanLiteral::True,
    "false" => BooleanLiteral::False,
    _ => panic!(),
  }
}
impl BooleanLiteral {
  pub fn name(&self) -> &'static str {
    match self {
      BooleanLiteral::True => "true",
      BooleanLiteral::False => "false",
    }
  }
}

pub struct StringLiteralData {
  pub value: String,
  pub loc: SourceLoc,
}

pub struct NumericLiteralData {
  pub value: String,
  pub loc: SourceLoc,
}

pub struct RegExpLiteralData {
  pub value: String,
  pub loc: SourceLoc,
}

pub enum Token {
  Keyword(KeywordData),
  Symbol(SymbolData),
  ContextualKeyword(CtxKeywordData),
  Identifier(IdentifierData),
  NullLiteral(NullLiteralData),
  BooleanLiteral(BooleanLiteralData),
  StringLiteral(StringLiteralData),
  NumericLiteral(NumericLiteralData),
  RegExpLiteral(RegExpLiteralData),
  Nil,
}

impl Token {
  pub fn is_keyword(&self) -> bool {
    match self {
      Token::Keyword(_) => true,
      _ => false,
    }
  }

  pub fn keyword_data(&self) -> Option<&KeywordData> {
    match self {
      Token::Keyword(data) => Some(data),
      _ => None,
    }
  }

  pub fn is_symbol(&self) -> bool {
    match self {
      Token::Symbol(_) => true,
      _ => false,
    }
  }

  pub fn symbol_data(&self) -> Option<&SymbolData> {
    match self {
      Token::Symbol(data) => Some(data),
      _ => None,
    }
  }

  pub fn is_ctx_keyword(&self) -> bool {
    match self {
      Token::ContextualKeyword(_) => true,
      _ => false,
    }
  }

  pub fn ctx_keyword_data(&self) -> Option<&CtxKeywordData> {
    match self {
      Token::ContextualKeyword(data) => Some(data),
      _ => None,
    }
  }

  pub fn is_id(&self) -> bool {
    match self {
      Token::Identifier(_) => true,
      _ => false,
    }
  }

  pub fn id_data(&self) -> Option<&IdentifierData> {
    match self {
      Token::Identifier(data) => Some(data),
      _ => None,
    }
  }

  pub fn is_null(&self) -> bool {
    match self {
      Token::NullLiteral(_) => true,
      _ => false,
    }
  }

  pub fn null_literal_data(&self) -> Option<&NullLiteralData> {
    match self {
      Token::NullLiteral(data) => Some(data),
      _ => None,
    }
  }

  pub fn is_bool(&self) -> bool {
    match self {
      Token::BooleanLiteral(_) => true,
      _ => false,
    }
  }

  pub fn bool_literal_data(&self) -> Option<&BooleanLiteralData> {
    match self {
      Token::BooleanLiteral(data) => Some(data),
      _ => None,
    }
  }

  pub fn is_str(&self) -> bool {
    match self {
      Token::StringLiteral(_) => true,
      _ => false,
    }
  }

  pub fn str_literal_data(&self) -> Option<&StringLiteralData> {
    match self {
      Token::StringLiteral(data) => Some(data),
      _ => None,
    }
  }

  pub fn is_num(&self) -> bool {
    match self {
      Token::NumericLiteral(_) => true,
      _ => false,
    }
  }

  pub fn num_literal_data(&self) -> Option<&NumericLiteralData> {
    match self {
      Token::NumericLiteral(data) => Some(data),
      _ => None,
    }
  }

  pub fn is_regexp(&self) -> bool {
    match self {
      Token::RegExpLiteral(_) => true,
      _ => false,
    }
  }

  pub fn regexp_literal_data(&self) -> Option<&RegExpLiteralData> {
    match self {
      Token::RegExpLiteral(data) => Some(data),
      _ => None,
    }
  }

  pub fn is_before_expr(&self) -> bool {
    match self {
      Token::Keyword(data) => data.kind.is_before_expr(),
      Token::Symbol(data) => data.kind.is_before_expr(),
      Token::ContextualKeyword(data) => data.kind.is_before_expr(),
      _ => false,
    }
  }
}

static INIT_TOKEN_DATA_ONCE: Once = Once::new();
pub fn init_token_data() {
  INIT_TOKEN_DATA_ONCE.call_once(|| {
    init_keywords();
    init_symbols();
    init_ctx_keyword();
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
    assert_eq!("yield", CtxKeyword::Yield.name());
  }

  #[test]
  fn static_map_set() {
    init_token_data();
    assert!(is_keyword("break"));
    assert!(is_symbol("{"));
    assert!(is_ctx_keyword("implements"));
    assert!(Symbol::Assign.is_before_expr());
    assert!(Keyword::Return.is_before_expr());
    assert!(CtxKeyword::Yield.is_before_expr());
  }
}
