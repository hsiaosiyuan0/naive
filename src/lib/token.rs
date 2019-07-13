use std::collections::{HashMap, HashSet};
use std::sync::Once;

#[derive(Debug, Copy, Clone)]
pub struct Position {
  pub line: i32,
  pub column: i32,
}

impl Position {
  pub fn new() -> Position {
    Position { line: 0, column: 0 }
  }
}

#[derive(Debug, Copy, Clone)]
pub struct SourceLoc {
  pub start: Position,
  pub end: Position,
}

impl SourceLoc {
  pub fn new() -> Self {
    SourceLoc {
      start: Position::new(),
      end: Position::new(),
    }
  }
}

#[derive(Debug, Copy, Clone)]
pub struct KeywordData {
  pub kind: Keyword,
  pub loc: SourceLoc,
}
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
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
// Since the grammar of regexp of js is a island grammar when see it at the point of view of the whole js grammar,
// we have two opportunities to process that island grammar, either at tokenizing phase or parsing phase.
//
// Here we use a manner taken from [acorn](https://github.com/acornjs/acorn/blob/master/acorn/src/tokentype.js),
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

macro_rules! gen_map_syb {
  ($($k:expr => $v:expr, $be:expr, $pcd:expr)*) => {
    {
      let (s, kv, vk, be) = gen_map! {
        $(
          $k => $v, $be
        )*
      };
      let mut pcdm = HashMap::new();
      $(
        pcdm.insert($k, $pcd);
      )*
      (s, kv, vk, be, Some(pcdm))
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

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
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
  BinOpStart,
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
  BinOpEnd,
  Conditional,
  Colon,
  AssignStart,
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
  AssignEnd,
}

#[derive(Debug, Clone)]
pub struct SymbolData {
  pub kind: Symbol,
  pub loc: SourceLoc,
}

static mut SYMBOLS_SET: Option<HashSet<&'static str>> = None;
static mut SYMBOLS_KEY_NAME: Option<HashMap<Symbol, &'static str>> = None;
static mut SYMBOLS_NAME_KEY: Option<HashMap<&'static str, Symbol>> = None;
static mut SYMBOLS_BEFORE_EXPR_SET: Option<HashSet<Symbol>> = None;
static mut SYMBOLS_KEY_PRECEDENCE: Option<HashMap<Symbol, i32>> = None;
fn init_symbols() {
  let (s, kv, vk, be, pcd) = gen_map_syb! {
    Symbol::BraceL => "{", true, 0
    Symbol::BraceR => "}", false, 0
    Symbol::ParenL => "(", true, 20
    Symbol::ParenR => ")", false, 0
    Symbol::BracketL => "[", true, 19
    Symbol::BracketR => "]", false, 0
    Symbol::Dot => ".", true, 19
    Symbol::Semi => ";", true, 0
    Symbol::Comma => ",", true, 1
    Symbol::LT => "<", true, 11
    Symbol::GT => ">", true, 11
    Symbol::LE => "<=", true, 11
    Symbol::GE => ">=", true, 11
    Symbol::Eq => "==", true, 10
    Symbol::NotEq => "!=", true, 10
    Symbol::EqStrict => "===", true, 10
    Symbol::NotEqStrict => "!==", true, 10
    Symbol::Add => "+", true, 13
    Symbol::Sub => "-", true, 13
    Symbol::Mul => "*", true, 14
    Symbol::Div => "/", true, 14
    Symbol::Mod => "%", true, 14
    Symbol::Inc => "++", false, 16
    Symbol::Dec => "--", false, 16
    Symbol::SHL => "<<", true, 12
    Symbol::SAR => ">>", true, 12
    Symbol::SHR => ">>>", true, 12
    Symbol::BitAnd => "&", true, 9
    Symbol::BitOr => "|", true, 7
    Symbol::BitXor => "^", true, 8
    Symbol::Not => "!", true, 16
    Symbol::BitNot => "~", true, 16
    Symbol::And => "&&", true, 6
    Symbol::Or => "||", true, 5
    Symbol::Conditional => "?", true, 4
    Symbol::Colon => ":", true, 4
    Symbol::Assign => "=", true, 3
    Symbol::AssignAdd => "+=", true, 3
    Symbol::AssignSub => "-=", true, 3
    Symbol::AssignMul => "*=", true, 3
    Symbol::AssignDiv => "/=", true, 3
    Symbol::AssignMod => "%=", true, 3
    Symbol::AssignSHL => "<<=", true, 3
    Symbol::AssignSAR => ">>=", true, 3
    Symbol::AssignSHR => ">>>=", true, 3
    Symbol::AssignBitAnd => "&=", true, 3
    Symbol::AssignBitOr => "|=", true, 3
    Symbol::AssignBitXor => "^=", true, 3
  };
  unsafe {
    SYMBOLS_SET = s;
    SYMBOLS_KEY_NAME = kv;
    SYMBOLS_NAME_KEY = vk;
    SYMBOLS_BEFORE_EXPR_SET = be;
    SYMBOLS_KEY_PRECEDENCE = pcd;
  }
}
pub fn is_symbol(s: &str) -> bool {
  unsafe { SYMBOLS_SET.as_ref().unwrap().contains(s) }
}
pub fn symbol_to_name(v: &Symbol) -> &'static str {
  unsafe { SYMBOLS_KEY_NAME.as_ref().unwrap().get(v).unwrap() }
}
pub fn symbol_pcd(v: &Symbol) -> i32 {
  unsafe {
    SYMBOLS_KEY_PRECEDENCE
      .as_ref()
      .unwrap()
      .get(v)
      .unwrap()
      .to_owned()
  }
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

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct IdentifierData {
  pub value: String,
  pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct NullLiteralData {
  pub loc: SourceLoc,
}
pub fn is_null(s: &str) -> bool {
  s == "null"
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum BooleanLiteral {
  True,
  False,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct StringLiteralData {
  pub value: String,
  pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct NumericLiteralData {
  pub value: String,
  pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct RegExpLiteralData {
  pub value: String,
  pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct EofData {
  pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
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
  Eof(EofData),
  Nil,
}

impl Token {
  pub fn is_keyword(&self) -> bool {
    match self {
      Token::Keyword(_) => true,
      _ => false,
    }
  }

  pub fn is_keyword_kind(&self, k: Keyword) -> bool {
    match self {
      Token::Keyword(data) => data.kind == k,
      _ => false,
    }
  }

  pub fn is_keyword_kind_in(&self, ks: &Vec<Keyword>) -> bool {
    match self {
      Token::Keyword(d) => ks.contains(&d.kind),
      _ => false,
    }
  }

  pub fn is_keyword_bin(&self, not_in: bool) -> bool {
    match self {
      Token::Keyword(d) => !not_in && d.kind == Keyword::In,
      _ => false,
    }
  }

  pub fn keyword_pcd(&self) -> i32 {
    match self {
      Token::Keyword(d) => match d.kind {
        Keyword::In => 11,
        _ => -1,
      },
      _ => -1,
    }
  }

  pub fn keyword_data(&self) -> &KeywordData {
    match self {
      Token::Keyword(data) => data,
      _ => panic!(),
    }
  }

  pub fn is_symbol(&self) -> bool {
    match self {
      Token::Symbol(_) => true,
      _ => false,
    }
  }

  pub fn symbol_pcd(&self) -> i32 {
    match self {
      Token::Symbol(s) => symbol_pcd(&s.kind),
      _ => panic!(),
    }
  }

  pub fn is_symbol_bin(&self) -> bool {
    match self {
      Token::Symbol(s) => {
        let s = s.kind as i32;
        let start = Symbol::BinOpStart as i32;
        let end = Symbol::BinOpEnd as i32;
        s > start && s < end
      }
      _ => false,
    }
  }

  pub fn is_symbol_assign(&self) -> bool {
    match self {
      Token::Symbol(s) => {
        let s = s.kind as i32;
        let start = Symbol::AssignStart as i32;
        let end = Symbol::AssignEnd as i32;
        s > start && s < end
      }
      _ => false,
    }
  }

  pub fn is_symbol_kind(&self, k: Symbol) -> bool {
    match self {
      Token::Symbol(s) => s.kind == k,
      _ => false,
    }
  }

  pub fn is_symbol_kind_in(&self, ks: &Vec<Symbol>) -> bool {
    match self {
      Token::Symbol(d) => ks.contains(&d.kind),
      _ => false,
    }
  }

  pub fn symbol_data(&self) -> &SymbolData {
    match self {
      Token::Symbol(data) => data,
      _ => panic!(),
    }
  }

  pub fn is_ctx_keyword(&self) -> bool {
    match self {
      Token::ContextualKeyword(_) => true,
      _ => false,
    }
  }

  pub fn ctx_keyword_data(&self) -> &CtxKeywordData {
    match self {
      Token::ContextualKeyword(data) => data,
      _ => panic!(),
    }
  }

  pub fn is_id(&self) -> bool {
    match self {
      Token::Identifier(v) => !v.value.eq("undefined"),
      _ => false,
    }
  }

  pub fn id_data(&self) -> &IdentifierData {
    match self {
      Token::Identifier(data) => data,
      _ => panic!(),
    }
  }

  pub fn is_undef(&self) -> bool {
    match self {
      Token::Identifier(v) => v.value.eq("undefined"),
      _ => false,
    }
  }

  pub fn undef_data(&self) -> &IdentifierData {
    match self {
      Token::Identifier(data) => data,
      _ => panic!(),
    }
  }

  pub fn is_null(&self) -> bool {
    match self {
      Token::NullLiteral(_) => true,
      _ => false,
    }
  }

  pub fn null_data(&self) -> &NullLiteralData {
    match self {
      Token::NullLiteral(data) => data,
      _ => panic!(),
    }
  }

  pub fn is_bool(&self) -> bool {
    match self {
      Token::BooleanLiteral(_) => true,
      _ => false,
    }
  }

  pub fn bool_data(&self) -> &BooleanLiteralData {
    match self {
      Token::BooleanLiteral(data) => data,
      _ => panic!(),
    }
  }

  pub fn is_str(&self) -> bool {
    match self {
      Token::StringLiteral(_) => true,
      _ => false,
    }
  }

  pub fn str_data(&self) -> &StringLiteralData {
    match self {
      Token::StringLiteral(data) => data,
      _ => panic!(),
    }
  }

  pub fn is_num(&self) -> bool {
    match self {
      Token::NumericLiteral(_) => true,
      _ => false,
    }
  }

  pub fn num_data(&self) -> &NumericLiteralData {
    match self {
      Token::NumericLiteral(data) => data,
      _ => panic!(),
    }
  }

  pub fn is_regexp(&self) -> bool {
    match self {
      Token::RegExpLiteral(_) => true,
      _ => false,
    }
  }

  pub fn regexp_data(&self) -> &RegExpLiteralData {
    match self {
      Token::RegExpLiteral(data) => data,
      _ => panic!(),
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

  pub fn is_eof(&self) -> bool {
    match self {
      Token::Eof(_) => true,
      _ => false,
    }
  }

  pub fn loc(&self) -> &SourceLoc {
    match self {
      Token::Keyword(data) => &data.loc,
      Token::Symbol(data) => &data.loc,
      Token::ContextualKeyword(data) => &data.loc,
      Token::Identifier(data) => &data.loc,
      Token::NullLiteral(data) => &data.loc,
      Token::BooleanLiteral(data) => &data.loc,
      Token::StringLiteral(data) => &data.loc,
      Token::NumericLiteral(data) => &data.loc,
      Token::RegExpLiteral(data) => &data.loc,
      Token::Eof(data) => &data.loc,
      Token::Nil => panic!(),
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
