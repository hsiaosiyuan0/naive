use std::collections::HashSet;
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

pub struct Token {
  pub kind: TokenKind,
  pub value: String,
  pub loc: Location,
}

pub enum TokenKind {
  // punctuator
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

  // keywords
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

  // contextual future-reserved-words
  Implements,
  Let,
  Private,
  Public,
  Interface,
  Package,
  Protected,
  Static,
  Yield,

  Identifier,
  StringLiteral,
  NumericLiteral,
}

static mut SYMBOLS: Option<HashSet<&'static str>> = None;
static SYMBOLS_INIT: Once = ONCE_INIT;
pub fn is_symbol(s: &str) -> bool {
  unsafe {
    SYMBOLS_INIT.call_once(|| {
      SYMBOLS = Some({
        let mut set = HashSet::new();
        set.insert("{");
        set.insert("}");
        set.insert("(");
        set.insert(")");
        set.insert("[");
        set.insert("]");
        set.insert(".");
        set.insert(";");
        set.insert(",");
        set.insert("<");
        set.insert(">");
        set.insert("<=");
        set.insert(">=");
        set.insert("==");
        set.insert("!=");
        set.insert("===");
        set.insert("!==");
        set.insert("+");
        set.insert("-");
        set.insert("*");
        set.insert("/");
        set.insert("%");
        set.insert("++");
        set.insert("--");
        set.insert("<<");
        set.insert(">>");
        set.insert("<<<");
        set.insert("&");
        set.insert("|");
        set.insert("^");
        set.insert("!");
        set.insert("~");
        set.insert("&&");
        set.insert("||");
        set.insert("?");
        set.insert(":");
        set.insert("=");
        set.insert("+=");
        set.insert("-=");
        set.insert("*=");
        set.insert("/=");
        set.insert("%=");
        set.insert("<<=");
        set.insert(">>=");
        set.insert(">>>=");
        set.insert("&=");
        set.insert("!=");
        set.insert("^=");
        set
      })
    });
    SYMBOLS.as_ref().unwrap().contains(s)
  }
}

static mut KEYWORDS: Option<HashSet<&'static str>> = None;
static KEYWORDS_INIT: Once = ONCE_INIT;
pub fn is_keyword(s: &str) -> bool {
  unsafe {
    KEYWORDS_INIT.call_once(|| {
      KEYWORDS = Some({
        let mut set = HashSet::new();
        set.insert("break");
        set.insert("do");
        set.insert("instanceof");
        set.insert("typeof");
        set.insert("case");
        set.insert("else");
        set.insert("new");
        set.insert("var");
        set.insert("catch");
        set.insert("finally");
        set.insert("return");
        set.insert("void");
        set.insert("continue");
        set.insert("for");
        set.insert("switch");
        set.insert("while");
        set.insert("debugger");
        set.insert("function");
        set.insert("this");
        set.insert("with");
        set.insert("default");
        set.insert("if");
        set.insert("throw");
        set.insert("delete");
        set.insert("in");
        set.insert("try");
        // future reserved words
        set.insert("class");
        set.insert("enum");
        set.insert("extends");
        set.insert("super");
        set.insert("const");
        set.insert("export");
        set.insert("import");
        set
      })
    });
    KEYWORDS.as_ref().unwrap().contains(s)
  }
}

static mut CONTEXTUAL_KEYWORDS: Option<HashSet<&'static str>> = None;
static CONTEXTUAL_KEYWORDS_INIT: Once = ONCE_INIT;
pub fn is_contextual_keyword(s: &str) -> bool {
  unsafe {
    CONTEXTUAL_KEYWORDS_INIT.call_once(|| {
      CONTEXTUAL_KEYWORDS = Some({
        let mut set = HashSet::new();
        set.insert("implements");
        set.insert("let");
        set.insert("private");
        set.insert("public");
        set.insert("interface");
        set.insert("package");
        set.insert("protected");
        set.insert("static");
        set.insert("yield");
        set
      })
    });
    CONTEXTUAL_KEYWORDS.as_ref().unwrap().contains(s)
  }
}

impl TokenKind {
  fn name(&self) -> &'static str {
    match self {
      // punctuator
      TokenKind::BraceL => "{",
      TokenKind::BraceR => "}",
      TokenKind::ParenL => "(",
      TokenKind::ParenR => ")",
      TokenKind::BracketL => "[",
      TokenKind::BracketR => "]",
      TokenKind::Dot => ".",
      TokenKind::Semi => ";",
      TokenKind::Comma => ",",
      TokenKind::LT => "<",
      TokenKind::GT => ">",
      TokenKind::LE => "<=",
      TokenKind::GE => ">=",
      TokenKind::Eq => "==",
      TokenKind::NotEq => "!=",
      TokenKind::EqStrict => "===",
      TokenKind::NotEqStrict => "!==",
      TokenKind::Add => "+",
      TokenKind::Sub => "-",
      TokenKind::Mul => "*",
      TokenKind::Div => "/",
      TokenKind::Mod => "%",
      TokenKind::Inc => "++",
      TokenKind::Dec => "--",
      TokenKind::SHL => "<<",
      TokenKind::SAR => ">>",
      TokenKind::SHR => ">>>",
      TokenKind::BitAnd => "&",
      TokenKind::BitOr => "|",
      TokenKind::BitXor => "^",
      TokenKind::Not => "!",
      TokenKind::BitNot => "~",
      TokenKind::And => "&&",
      TokenKind::Or => "||",
      TokenKind::Conditional => "?",
      TokenKind::Colon => ":",
      TokenKind::Assign => "=",
      TokenKind::AssignAdd => "+=",
      TokenKind::AssignSub => "-=",
      TokenKind::AssignMul => "*=",
      TokenKind::AssignDiv => "/=",
      TokenKind::AssignMod => "%=",
      TokenKind::AssignSHL => "<<=",
      TokenKind::AssignSAR => ">>=",
      TokenKind::AssignSHR => ">>>=",
      TokenKind::AssignBitAnd => "&=",
      TokenKind::AssignBitOr => "|=",
      TokenKind::AssignBitXor => "^=",
      // keywords
      TokenKind::Break => "break",
      TokenKind::Do => "do",
      TokenKind::Instanceof => "instanceof",
      TokenKind::Typeof => "typeof",
      TokenKind::Case => "case",
      TokenKind::Else => "else",
      TokenKind::New => "new",
      TokenKind::Var => "var",
      TokenKind::Catch => "catch",
      TokenKind::Finally => "finally",
      TokenKind::Return => "return",
      TokenKind::Void => "void",
      TokenKind::Continue => "continue",
      TokenKind::For => "for",
      TokenKind::Switch => "switch",
      TokenKind::While => "while",
      TokenKind::Debugger => "debugger",
      TokenKind::Function => "function",
      TokenKind::This => "this",
      TokenKind::With => "with",
      TokenKind::Default => "default",
      TokenKind::If => "if",
      TokenKind::Throw => "throw",
      TokenKind::Delete => "delete",
      TokenKind::In => "in",
      TokenKind::Try => "try",
      // future reserved words
      TokenKind::Class => "class",
      TokenKind::Enum => "enum",
      TokenKind::Extends => "extends",
      TokenKind::Super => "super",
      TokenKind::Const => "const",
      TokenKind::Export => "export",
      TokenKind::Import => "import",
      // contextual future-reserved-words
      TokenKind::Implements => "implements",
      TokenKind::Let => "let",
      TokenKind::Private => "private",
      TokenKind::Public => "public",
      TokenKind::Interface => "interface",
      TokenKind::Package => "package",
      TokenKind::Protected => "protected",
      TokenKind::Static => "static",
      TokenKind::Yield => "yield",

      TokenKind::Identifier => "identifier",
      TokenKind::StringLiteral => "string",
      TokenKind::NumericLiteral => "numeric",
    }
  }
}

#[cfg(test)]
mod token_tests {
  use super::*;

  #[test]
  fn kind_name() {
    assert_eq!("{", TokenKind::BraceL.name());
    assert_eq!("}", TokenKind::BraceR.name());
    assert_eq!("yield", TokenKind::Yield.name());
  }

  #[test]
  fn static_set() {
    assert!(is_symbol("{"));
    assert!(is_keyword("break"));
    assert!(is_contextual_keyword("let"));
  }
}
