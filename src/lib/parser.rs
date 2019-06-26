use crate::lexer::*;
use crate::token::*;
use core::borrow::Borrow;
use std::ops::Deref;
use std::rc::Rc;

pub struct ParserErr {
  pub msg: String,
}

impl ParserErr {
  pub fn new(msg: String) -> Self {
    ParserErr { msg }
  }

  pub fn default() -> Self {
    ParserErr {
      msg: "".to_string(),
    }
  }
}

pub enum ParserError {
  Parser(ParserErr),
  Lexer(LexError),
}

impl From<ParserErr> for ParserError {
  fn from(e: ParserErr) -> Self {
    ParserError::Parser(e)
  }
}

impl From<LexError> for ParserError {
  fn from(e: LexError) -> Self {
    ParserError::Lexer(e)
  }
}

pub struct Parser<'a> {
  pub lexer: &'a mut Lexer<'a>,
}

impl<'a> Parser<'a> {
  pub fn new(lexer: &'a mut Lexer<'a>) -> Self {
    Parser { lexer }
  }

  fn errmsg(&self, loc: &SourceLoc) -> String {
    format!(
      "Unexpected token at line: {} column: {}",
      loc.start.line, loc.start.column
    )
  }

  fn expr(&mut self) -> Result<Expr, ParserError> {
    match self.primary_expr() {
      Ok(expr) => Ok(expr.into()),
      Err(e) => Err(e),
    }
  }

  fn primary_expr(&mut self) -> Result<PrimaryExpr, ParserError> {
    match self.lexer.next() {
      Ok(tok) => {
        let loc = tok.loc().to_owned();
        if tok.is_keyword_kind(Keyword::This) {
          Ok(ThisExprData::new(loc).into())
        } else if tok.is_id() {
          Ok(IdData::new(loc, tok.id_data().value.clone()).into())
        } else if tok.is_str() {
          Ok(StringData::new(loc, tok.str_data().value.clone()).into())
        } else if tok.is_null() {
          Ok(NullData::new(loc).into())
        } else if tok.is_bool() {
          Ok(BoolData::new(loc, tok.bool_data().kind == BooleanLiteral::True).into())
        } else if tok.is_num() {
          Ok(NumericData::new(loc, tok.num_data().value.clone()).into())
        } else if tok.is_symbol_kind(Symbol::BracketL) {
          self.array_literal(&tok)
        } else {
          Err(ParserErr::new(self.errmsg(tok.loc())).into())
        }
      }
      Err(e) => Err(e.into()),
    }
  }

  fn array_literal(&mut self, open: &Token) -> Result<PrimaryExpr, ParserError> {
    let mut ret = ArrayData {
      loc: open.loc().to_owned(),
      value: vec![],
    };
    loop {
      match self.lexer.peek() {
        Ok(tok) => {
          if tok.is_symbol_kind(Symbol::BracketR) {
            match self.lexer.next() {
              Err(e) => return Err(e.into()),
              _ => break,
            }
          } else if tok.is_symbol_kind(Symbol::Comma) {
            match self.lexer.next() {
              Err(e) => return Err(e.into()),
              _ => (),
            }
          } else {
            match self.expr() {
              Ok(expr) => ret.value.push(expr),
              Err(e) => return Err(e.into()),
            };
          }
        }
        Err(e) => return Err(e.into()),
      }
    }
    Ok(ret.into())
  }
}

pub enum Literal {
  RegExp(RegExpData),
  Null(NullData),
  String(StringData),
  Bool(BoolData),
  Numeric(NumericData),
}

impl From<RegExpData> for Literal {
  fn from(f: RegExpData) -> Self {
    Literal::RegExp(f)
  }
}

impl From<NullData> for Literal {
  fn from(f: NullData) -> Self {
    Literal::Null(f)
  }
}

impl From<StringData> for Literal {
  fn from(f: StringData) -> Self {
    Literal::String(f)
  }
}

impl From<BoolData> for Literal {
  fn from(f: BoolData) -> Self {
    Literal::Bool(f)
  }
}

impl From<NumericData> for Literal {
  fn from(f: NumericData) -> Self {
    Literal::Numeric(f)
  }
}

impl From<Literal> for RegExpData {
  fn from(f: Literal) -> Self {
    match f {
      Literal::RegExp(d) => d,
      _ => panic!(),
    }
  }
}

impl From<Literal> for NullData {
  fn from(f: Literal) -> Self {
    match f {
      Literal::Null(d) => d,
      _ => panic!(),
    }
  }
}

impl From<Literal> for StringData {
  fn from(f: Literal) -> Self {
    match f {
      Literal::String(d) => d,
      _ => panic!(),
    }
  }
}

impl From<Literal> for BoolData {
  fn from(f: Literal) -> Self {
    match f {
      Literal::Bool(d) => d,
      _ => panic!(),
    }
  }
}

impl From<Literal> for NumericData {
  fn from(f: Literal) -> Self {
    match f {
      Literal::Numeric(d) => d,
      _ => panic!(),
    }
  }
}

pub enum PrimaryExpr {
  This(ThisExprData),
  Identifier(IdData),
  Literal(Literal),
  ArrayLiteral(ArrayData),
  ObjectLiteral(ObjectData),
  Parenthesized(ParenData),
}

impl From<ThisExprData> for PrimaryExpr {
  fn from(f: ThisExprData) -> Self {
    PrimaryExpr::This(f)
  }
}

impl From<IdData> for PrimaryExpr {
  fn from(f: IdData) -> Self {
    PrimaryExpr::Identifier(f)
  }
}

impl From<Literal> for PrimaryExpr {
  fn from(f: Literal) -> Self {
    PrimaryExpr::Literal(f)
  }
}

impl From<StringData> for PrimaryExpr {
  fn from(f: StringData) -> Self {
    let node: Literal = f.into();
    node.into()
  }
}

impl From<NullData> for PrimaryExpr {
  fn from(f: NullData) -> Self {
    let node: Literal = f.into();
    node.into()
  }
}

impl From<RegExpData> for PrimaryExpr {
  fn from(f: RegExpData) -> Self {
    let node: Literal = f.into();
    node.into()
  }
}

impl From<BoolData> for PrimaryExpr {
  fn from(f: BoolData) -> Self {
    let node: Literal = f.into();
    node.into()
  }
}

impl From<NumericData> for PrimaryExpr {
  fn from(f: NumericData) -> Self {
    let node: Literal = f.into();
    node.into()
  }
}

impl From<ArrayData> for PrimaryExpr {
  fn from(f: ArrayData) -> Self {
    PrimaryExpr::ArrayLiteral(f)
  }
}

impl From<PrimaryExpr> for ThisExprData {
  fn from(f: PrimaryExpr) -> Self {
    match f {
      PrimaryExpr::This(d) => d,
      _ => panic!(),
    }
  }
}

impl From<PrimaryExpr> for IdData {
  fn from(f: PrimaryExpr) -> Self {
    match f {
      PrimaryExpr::Identifier(d) => d,
      _ => panic!(),
    }
  }
}

impl From<PrimaryExpr> for Literal {
  fn from(f: PrimaryExpr) -> Self {
    match f {
      PrimaryExpr::Literal(d) => d,
      _ => panic!(),
    }
  }
}

impl<'a> From<&'a PrimaryExpr> for &'a Literal {
  fn from(f: &'a PrimaryExpr) -> Self {
    match f {
      PrimaryExpr::Literal(d) => d,
      _ => panic!(),
    }
  }
}

impl<'a> From<&'a Literal> for &'a StringData {
  fn from(f: &'a Literal) -> Self {
    match f {
      Literal::String(d) => d,
      _ => panic!(),
    }
  }
}

impl From<PrimaryExpr> for ArrayData {
  fn from(f: PrimaryExpr) -> Self {
    match f {
      PrimaryExpr::ArrayLiteral(d) => d,
      _ => panic!(),
    }
  }
}

pub enum Expr {
  Primary(Rc<PrimaryExpr>),
  Function,
  Member,
  New,
  Call,
  Unary,
  Binary,
  Assignment,
  Conditional,
}

impl From<PrimaryExpr> for Expr {
  fn from(f: PrimaryExpr) -> Self {
    let expr = Rc::new(f);
    Expr::Primary(expr)
  }
}

impl From<Expr> for Rc<PrimaryExpr> {
  fn from(f: Expr) -> Self {
    match f {
      Expr::Primary(d) => d,
      _ => panic!(),
    }
  }
}

impl<'a> From<&'a Expr> for &'a Rc<PrimaryExpr> {
  fn from(f: &'a Expr) -> Self {
    match f {
      Expr::Primary(d) => d,
      _ => panic!(),
    }
  }
}

pub struct ThisExprData {
  pub loc: SourceLoc,
}

impl ThisExprData {
  fn new(loc: SourceLoc) -> Self {
    ThisExprData { loc }
  }
}

pub struct IdData {
  pub loc: SourceLoc,
  pub name: String,
}

impl IdData {
  fn new(loc: SourceLoc, name: String) -> Self {
    IdData { loc, name }
  }
}

pub struct RegExpData {
  pub loc: SourceLoc,
  pub value: String,
}

impl RegExpData {
  fn new(loc: SourceLoc, value: String) -> Self {
    RegExpData { loc, value }
  }
}

pub struct NullData {
  pub loc: SourceLoc,
}

impl NullData {
  fn new(loc: SourceLoc) -> Self {
    NullData { loc }
  }
}

pub struct StringData {
  pub loc: SourceLoc,
  pub value: String,
}

impl StringData {
  fn new(loc: SourceLoc, value: String) -> Self {
    StringData { loc, value }
  }
}

pub struct BoolData {
  pub loc: SourceLoc,
  pub value: bool,
}

impl BoolData {
  fn new(loc: SourceLoc, value: bool) -> Self {
    BoolData { loc, value }
  }
}

pub struct NumericData {
  pub loc: SourceLoc,
  pub value: String,
}

impl NumericData {
  fn new(loc: SourceLoc, value: String) -> Self {
    NumericData { loc, value }
  }
}

pub struct ArrayData {
  pub loc: SourceLoc,
  pub value: Vec<Expr>,
}

pub struct ObjectProperty {
  pub loc: SourceLoc,
  pub key: Expr,
  pub value: Expr,
}

pub struct ObjectData {
  pub loc: SourceLoc,
  pub properties: Vec<ObjectProperty>,
}

pub struct ParenData {
  pub loc: SourceLoc,
  pub value: Expr,
}

#[cfg(test)]
mod lexer_tests {
  use super::*;
  use crate::source::*;

  #[test]
  fn primary_expr() {
    init_token_data();

    let code = String::from("this a 'hello' null false 0x01 [0x1, a, true, 'world']");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);
    let node = parser.primary_expr();
    let node: ThisExprData = node.ok().unwrap().into();
    assert_eq!(1, node.loc.start.line);

    let node = parser.primary_expr();
    let node: IdData = node.ok().unwrap().into();
    assert_eq!("a", node.name);

    let node = parser.primary_expr();
    let node: Literal = node.ok().unwrap().into();
    let node: StringData = node.into();
    assert_eq!("hello", node.value);

    let node = parser.primary_expr();
    let node: Literal = node.ok().unwrap().into();
    let node: NullData = node.into();
    assert_eq!(1, node.loc.start.line);

    let node = parser.primary_expr();
    let node: Literal = node.ok().unwrap().into();
    let node: BoolData = node.into();
    assert_eq!(false, node.value);

    let node = parser.primary_expr();
    let node: Literal = node.ok().unwrap().into();
    let node: NumericData = node.into();
    assert_eq!("0x01", node.value);

    let node = parser.primary_expr();
    let node: ArrayData = node.ok().unwrap().into();
    assert_eq!(4, node.value.len());
    let node = node.value[3].borrow();
    let node: &Rc<PrimaryExpr> = node.into();
    let node: &PrimaryExpr = node.borrow();
    let node: &Literal = node.into();
    let node: &StringData = node.into();
    assert_eq!("world", node.value);
  }
}
