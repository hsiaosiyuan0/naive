use crate::lexer::*;
use crate::token::*;
use core::borrow::Borrow;
use std::ops::Deref;
use std::rc::Rc;

pub struct ParserError {
  pub msg: String,
}

impl ParserError {
  pub fn new(msg: String) -> Self {
    ParserError { msg }
  }

  pub fn at(tok: &Token) -> Self {
    let loc = tok.loc();
    ParserError {
      msg: format!(
        "Unexpected token at line: {} column: {}",
        loc.start.line, loc.start.column
      ),
    }
  }

  pub fn default() -> Self {
    ParserError {
      msg: "".to_string(),
    }
  }
}

pub enum ParsingError {
  Parser(ParserError),
  Lexer(LexError),
}

impl From<ParserError> for ParsingError {
  fn from(e: ParserError) -> Self {
    ParsingError::Parser(e)
  }
}

impl From<LexError> for ParsingError {
  fn from(e: LexError) -> Self {
    ParsingError::Lexer(e)
  }
}

pub struct Parser<'a> {
  pub lexer: &'a mut Lexer<'a>,
}

impl<'a> Parser<'a> {
  pub fn new(lexer: &'a mut Lexer<'a>) -> Self {
    Parser { lexer }
  }

  fn expr(&mut self) -> Result<Expr, ParsingError> {
    match self.expr_op(None, 0) {
      Ok(expr) => Ok(expr),
      Err(e) => Err(e),
    }
  }

  fn primary_expr(&mut self) -> Result<PrimaryExpr, ParsingError> {
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
          Err(ParserError::at(&tok).into())
        }
      }
      Err(e) => Err(e.into()),
    }
  }

  fn expr_op(&mut self, lhs: Option<Expr>, pcd: i32) -> Result<Expr, ParsingError> {
    self.postfix_expr()
  }

  fn postfix_expr(&mut self) -> Result<Expr, ParsingError> {
    match self.lhs_expr() {
      Ok(expr) => {
        if let Ok(tok) = self.lexer.peek() {
          if tok.is_symbol_kind(Symbol::Inc) || tok.is_symbol_kind(Symbol::Dec) {
            let tok = self.lexer.next().ok().unwrap();
            let expr = UnaryExpr::new(tok.deref().clone(), expr, false).into();
            return Ok(expr);
          }
        }
        Ok(expr)
      }
      Err(e) => Err(e),
    }
  }

  fn lhs_expr(&mut self) -> Result<Expr, ParsingError> {
    if self.ahead_is_keyword(Keyword::New) {
      self.new_expr()
    } else {
      self.call_expr(None)
    }
  }

  fn ahead_is_keyword(&mut self, k: Keyword) -> bool {
    match self.lexer.peek() {
      Ok(tok) => tok.is_keyword_kind(k),
      _ => false,
    }
  }

  fn ahead_is_symbol(&mut self, syb: Symbol) -> bool {
    match self.lexer.peek() {
      Ok(tok) => tok.is_symbol_kind(syb),
      _ => false,
    }
  }

  fn ahead_is_symbol_or(&mut self, s1: Symbol, s2: Symbol) -> bool {
    match self.lexer.peek() {
      Ok(tok) => tok.is_symbol_kind(s1) || tok.is_symbol_kind(s2),
      _ => false,
    }
  }

  fn new_expr(&mut self) -> Result<Expr, ParsingError> {
    self.lexer.advance();
    let callee = match self.ahead_is_keyword(Keyword::New) {
      true => self.new_expr(),
      false => self.member_expr(None),
    };
    let callee = match callee {
      Ok(expr) => expr,
      Err(e) => return Err(e),
    };
    let arguments = match self.ahead_is_symbol(Symbol::ParenL) {
      true => match self.args() {
        Ok(args) => args,
        Err(e) => return Err(e),
      },
      false => vec![],
    };
    let expr = NewExpr { callee, arguments };
    Ok(expr.into())
  }

  fn args(&mut self) -> Result<Vec<Expr>, ParsingError> {
    self.lexer.advance();
    let mut args: Vec<Expr> = vec![];
    loop {
      if self.ahead_is_symbol(Symbol::ParenR) {
        self.lexer.advance();
        break;
      }
      match self.expr() {
        Ok(expr) => args.push(expr),
        Err(e) => return Err(e),
      };
      if self.ahead_is_symbol(Symbol::Comma) {
        self.lexer.advance();
      }
    }
    Ok(args)
  }

  fn call_expr(&mut self, callee: Option<Expr>) -> Result<Expr, ParsingError> {
    let callee = match callee {
      Some(expr) => expr,
      _ => match self.member_expr(None) {
        Ok(expr) => expr,
        Err(e) => return Err(e),
      },
    };

    if !self.ahead_is_symbol(Symbol::ParenL) {
      return Ok(callee);
    }

    let arguments = match self.args() {
      Ok(args) => args,
      Err(e) => return Err(e),
    };
    let mut expr: Expr = CallExpr { callee, arguments }.into();
    if self.ahead_is_symbol_or(Symbol::Dot, Symbol::BracketL) {
      expr = match self.member_expr(Some(expr)) {
        Ok(expr) => expr,
        Err(e) => return Err(e),
      }
    } else if self.ahead_is_symbol(Symbol::ParenL) {
      expr = match self.call_expr(Some(expr)) {
        Ok(expr) => expr,
        Err(e) => return Err(e),
      }
    }
    Ok(expr)
  }

  fn member_expr(&mut self, obj: Option<Expr>) -> Result<Expr, ParsingError> {
    let mut obj = match obj {
      Some(o) => o,
      _ => match self.primary_expr() {
        Ok(expr) => expr.into(),
        Err(e) => return Err(e),
      },
    };
    loop {
      if let Ok(tok) = self.lexer.peek() {
        if tok.is_symbol_kind(Symbol::Dot) {
          self.lexer.advance();
          match self.lexer.next() {
            Ok(tok) => {
              if !tok.is_id() {
                return Err(ParserError::at(&tok).into());
              }
              let prop = PrimaryExpr::Identifier(IdData::new(
                tok.loc().clone(),
                tok.id_data().clone().value,
              ));
              obj = Expr::Member(Rc::new(MemberExpr {
                object: obj,
                property: prop.into(),
                computed: false,
              }));
              continue;
            }
            Err(e) => return Err(e.into()),
          }
        } else if tok.is_symbol_kind(Symbol::BracketL) {
          self.lexer.advance();
          let prop = match self.expr() {
            Ok(expr) => expr,
            Err(e) => return Err(e.into()),
          };
          match self.lexer.next() {
            Ok(tok) => {
              if !tok.is_symbol_kind(Symbol::BracketR) {
                return Err(ParserError::at(&tok).into());
              }
            }
            Err(e) => return Err(e.into()),
          }
          obj = Expr::Member(Rc::new(MemberExpr {
            object: obj,
            property: prop,
            computed: true,
          }));
          continue;
        }
      }
      break;
    }
    Ok(obj)
  }

  fn array_literal(&mut self, open: &Token) -> Result<PrimaryExpr, ParsingError> {
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

#[derive(Debug)]
pub enum Literal {
  RegExp(RegExpData),
  Null(NullData),
  String(StringData),
  Bool(BoolData),
  Numeric(NumericData),
}

impl Literal {
  pub fn is_regexp(&self) -> bool {
    match self {
      Literal::RegExp(_) => true,
      _ => false,
    }
  }

  pub fn is_null(&self) -> bool {
    match self {
      Literal::Null(_) => true,
      _ => false,
    }
  }

  pub fn is_str(&self) -> bool {
    match self {
      Literal::String(_) => true,
      _ => false,
    }
  }

  pub fn is_bool(&self) -> bool {
    match self {
      Literal::Bool(_) => true,
      _ => false,
    }
  }

  pub fn is_num(&self) -> bool {
    match self {
      Literal::Numeric(_) => true,
      _ => false,
    }
  }

  pub fn regexp(&self) -> &RegExpData {
    match self {
      Literal::RegExp(d) => d,
      _ => panic!(),
    }
  }

  pub fn null(&self) -> &NullData {
    match self {
      Literal::Null(d) => d,
      _ => panic!(),
    }
  }

  pub fn str(&self) -> &StringData {
    match self {
      Literal::String(d) => d,
      _ => panic!(),
    }
  }

  pub fn bool(&self) -> &BoolData {
    match self {
      Literal::Bool(d) => d,
      _ => panic!(),
    }
  }

  pub fn num(&self) -> &NumericData {
    match self {
      Literal::Numeric(d) => d,
      _ => panic!(),
    }
  }
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

#[derive(Debug)]
pub enum PrimaryExpr {
  This(ThisExprData),
  Identifier(IdData),
  Literal(Literal),
  ArrayLiteral(ArrayData),
  ObjectLiteral(ObjectData),
  Parenthesized(ParenData),
}

impl PrimaryExpr {
  pub fn is_id(&self) -> bool {
    match self {
      PrimaryExpr::Identifier(_) => true,
      _ => false,
    }
  }

  pub fn is_this(&self) -> bool {
    match self {
      PrimaryExpr::This(_) => true,
      _ => false,
    }
  }

  pub fn is_literal(&self) -> bool {
    match self {
      PrimaryExpr::Literal(_) => true,
      _ => false,
    }
  }

  pub fn is_array(&self) -> bool {
    match self {
      PrimaryExpr::ArrayLiteral(_) => true,
      _ => false,
    }
  }

  pub fn is_object(&self) -> bool {
    match self {
      PrimaryExpr::ObjectLiteral(_) => true,
      _ => false,
    }
  }

  pub fn is_paren(&self) -> bool {
    match self {
      PrimaryExpr::Parenthesized(_) => true,
      _ => false,
    }
  }

  pub fn this(&self) -> &ThisExprData {
    match self {
      PrimaryExpr::This(d) => d,
      _ => panic!(),
    }
  }

  pub fn literal(&self) -> &Literal {
    match self {
      PrimaryExpr::Literal(d) => d,
      _ => panic!(),
    }
  }

  pub fn array(&self) -> &ArrayData {
    match self {
      PrimaryExpr::ArrayLiteral(d) => d,
      _ => panic!(),
    }
  }

  pub fn object(&self) -> &ObjectData {
    match self {
      PrimaryExpr::ObjectLiteral(d) => d,
      _ => panic!(),
    }
  }

  pub fn paren(&self) -> &ParenData {
    match self {
      PrimaryExpr::Parenthesized(d) => d,
      _ => panic!(),
    }
  }

  pub fn id(&self) -> &IdData {
    match self {
      PrimaryExpr::Identifier(d) => d,
      _ => panic!(),
    }
  }
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

#[derive(Debug)]
pub struct UnaryExpr {
  op: Token,
  argument: Expr,
  prefix: bool,
}

impl UnaryExpr {
  fn new(op: Token, argument: Expr, prefix: bool) -> Self {
    UnaryExpr {
      op,
      argument,
      prefix,
    }
  }
}

#[derive(Debug)]
pub struct BinaryExpr {
  op: Token,
  left: Expr,
  right: Expr,
}

#[derive(Debug)]
pub struct MemberExpr {
  object: Expr,
  property: Expr,
  computed: bool,
}

#[derive(Debug)]
pub struct NewExpr {
  callee: Expr,
  arguments: Vec<Expr>,
}

#[derive(Debug)]
pub struct CallExpr {
  callee: Expr,
  arguments: Vec<Expr>,
}

#[derive(Debug)]
pub enum Expr {
  Primary(Rc<PrimaryExpr>),
  Function,
  Member(Rc<MemberExpr>),
  New(Rc<NewExpr>),
  Call(Rc<CallExpr>),
  Unary(Rc<UnaryExpr>),
  Binary(Rc<BinaryExpr>),
  Assignment,
  Conditional,
}

impl Expr {
  fn is_unary(&self) -> bool {
    match self {
      Expr::Unary(_) => true,
      _ => false,
    }
  }

  fn is_member(&self) -> bool {
    match self {
      Expr::Member(_) => true,
      _ => false,
    }
  }

  fn is_new(&self) -> bool {
    match self {
      Expr::New(_) => true,
      _ => false,
    }
  }

  fn is_call(&self) -> bool {
    match self {
      Expr::Call(_) => true,
      _ => false,
    }
  }

  fn primary(&self) -> &PrimaryExpr {
    match self {
      Expr::Primary(expr) => expr,
      _ => panic!(),
    }
  }

  fn member(&self) -> &MemberExpr {
    match self {
      Expr::Member(expr) => expr,
      _ => panic!(),
    }
  }

  fn unary(&self) -> &UnaryExpr {
    match self {
      Expr::Unary(expr) => expr,
      _ => panic!(),
    }
  }

  fn new_expr(&self) -> &NewExpr {
    match self {
      Expr::New(expr) => expr,
      _ => panic!(),
    }
  }

  fn call_expr(&self) -> &CallExpr {
    match self {
      Expr::Call(expr) => expr,
      _ => panic!(),
    }
  }
}

impl From<UnaryExpr> for Expr {
  fn from(f: UnaryExpr) -> Self {
    Expr::Unary(Rc::new(f))
  }
}

impl From<PrimaryExpr> for Expr {
  fn from(f: PrimaryExpr) -> Self {
    let expr = Rc::new(f);
    Expr::Primary(expr)
  }
}

impl From<NewExpr> for Expr {
  fn from(f: NewExpr) -> Self {
    let expr = Rc::new(f);
    Expr::New(expr)
  }
}

impl From<CallExpr> for Expr {
  fn from(f: CallExpr) -> Self {
    let expr = Rc::new(f);
    Expr::Call(expr)
  }
}

#[derive(Debug)]
pub struct ThisExprData {
  pub loc: SourceLoc,
}

impl ThisExprData {
  fn new(loc: SourceLoc) -> Self {
    ThisExprData { loc }
  }
}

#[derive(Debug)]
pub struct IdData {
  pub loc: SourceLoc,
  pub name: String,
}

impl IdData {
  fn new(loc: SourceLoc, name: String) -> Self {
    IdData { loc, name }
  }
}

#[derive(Debug)]
pub struct RegExpData {
  pub loc: SourceLoc,
  pub value: String,
}

impl RegExpData {
  fn new(loc: SourceLoc, value: String) -> Self {
    RegExpData { loc, value }
  }
}

#[derive(Debug)]
pub struct NullData {
  pub loc: SourceLoc,
}

impl NullData {
  fn new(loc: SourceLoc) -> Self {
    NullData { loc }
  }
}

#[derive(Debug)]
pub struct StringData {
  pub loc: SourceLoc,
  pub value: String,
}

impl StringData {
  fn new(loc: SourceLoc, value: String) -> Self {
    StringData { loc, value }
  }
}

#[derive(Debug)]
pub struct BoolData {
  pub loc: SourceLoc,
  pub value: bool,
}

impl BoolData {
  fn new(loc: SourceLoc, value: bool) -> Self {
    BoolData { loc, value }
  }
}

#[derive(Debug)]
pub struct NumericData {
  pub loc: SourceLoc,
  pub value: String,
}

impl NumericData {
  fn new(loc: SourceLoc, value: String) -> Self {
    NumericData { loc, value }
  }
}

#[derive(Debug)]
pub struct ArrayData {
  pub loc: SourceLoc,
  pub value: Vec<Expr>,
}

#[derive(Debug)]
pub struct ObjectProperty {
  pub loc: SourceLoc,
  pub key: Expr,
  pub value: Expr,
}

#[derive(Debug)]
pub struct ObjectData {
  pub loc: SourceLoc,
  pub properties: Vec<ObjectProperty>,
}

#[derive(Debug)]
pub struct ParenData {
  pub loc: SourceLoc,
  pub value: Expr,
}

#[cfg(test)]
mod lexer_tests {
  use super::*;
  use crate::source::*;
  use core::borrow::Borrow;

  #[test]
  fn primary_expr() {
    init_token_data();

    let code = String::from("this a 'hello' null false 0x01 [0x1, a, true, 'world']");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);
    let node = parser.primary_expr().ok().unwrap();
    assert!(node.is_this());

    let node = parser.primary_expr();
    let node = node.ok().unwrap();
    assert!(node.is_id());

    let node = parser.primary_expr();
    let node = node.ok().unwrap();
    assert!(node.is_literal());
    assert!(node.literal().is_str());

    let node = parser.primary_expr();
    let node = node.ok().unwrap();
    assert!(node.is_literal());
    assert!(node.literal().is_null());

    let node = parser.primary_expr();
    let node = node.ok().unwrap();
    assert!(node.is_literal());
    assert!(node.literal().is_bool());

    let node = parser.primary_expr();
    let node = node.ok().unwrap();
    assert!(node.is_literal());
    assert!(node.literal().is_num());

    let node = parser.primary_expr();
    let node = node.ok().unwrap();
    assert!(node.is_array());
    assert_eq!(4, node.array().value.len());
    assert_eq!(
      "world",
      node.array().value[3].primary().literal().str().value
    );
  }

  #[test]
  fn member_expr() {
    init_token_data();

    let code = String::from("a.b++ a[c][d]--");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.expr();
    assert!(!node.is_err());
    let node = node.ok().unwrap();
    assert!(node.is_unary());
    let node = node.unary();
    assert!(node.argument.is_member());
    assert_eq!("a", node.argument.member().object.primary().id().name);

    let node = parser.expr();
    assert!(!node.is_err());
    let node = node.ok().unwrap();
    assert!(node.is_unary());
    assert_eq!(false, node.unary().prefix);
    assert_eq!(true, node.unary().op.is_symbol_kind(Symbol::Dec));
    assert!(node.unary().argument.is_member());
    let node = node.unary().argument.member();
    assert!(node.object.is_member());
    assert_eq!("c", node.object.member().property.primary().id().name);
  }

  #[test]
  fn new_expr() {
    init_token_data();

    let code = String::from("new new a(b)");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.expr();
    assert!(!node.is_err());
    let node = node.ok().unwrap();
    assert!(node.is_new());
    assert!(node.new_expr().callee.is_new());
    assert_eq!(
      "b",
      node.new_expr().callee.new_expr().arguments[0]
        .primary()
        .id()
        .name
    );
  }

  #[test]
  fn call_expr() {
    init_token_data();

    let code = String::from("a(b)(c)");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.expr();
    assert!(!node.is_err());
    let node = node.ok().unwrap();
    let call = node.call_expr();
    assert_eq!("c", call.arguments[0].primary().id().name);

    let call = call.callee.call_expr();
    assert_eq!("a", call.callee.primary().id().name);
  }

  #[test]
  fn call_expr_mix() {
    init_token_data();

    let code = String::from("a()()[b].c");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.expr();
    assert!(!node.is_err());
    let node = node.ok().unwrap();
    let obj = node.member();
    let prop = obj.property.primary().id();
    assert_eq!("c", prop.name);

    let obj = obj.object.member();
    assert_eq!("b", obj.property.primary().id().name);
    let call = obj.object.call_expr();
    assert!(call.callee.is_call());

    let call = call.callee.call_expr();
    assert_eq!("a", call.callee.primary().id().name);
  }
}
