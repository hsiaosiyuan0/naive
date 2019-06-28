use crate::ast::*;
use crate::lexer::*;
use crate::token::*;
use std::ops::Deref;
use std::rc::Rc;

pub struct Parser<'a> {
  pub lexer: &'a mut Lexer<'a>,
}

impl<'a> Parser<'a> {
  pub fn new(lexer: &'a mut Lexer<'a>) -> Self {
    Parser { lexer }
  }

  fn expr(&mut self) -> Result<Expr, ParsingError> {
    match self.expr_op(None, 0, false) {
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

  fn expr_op(
    &mut self,
    lhs: Option<Expr>,
    min_pcd: i32,
    not_in: bool,
  ) -> Result<Expr, ParsingError> {
    let mut lhs = match lhs {
      Some(expr) => expr,
      _ => match self.unary_expr() {
        Ok(expr) => expr,
        Err(e) => return Err(e),
      },
    };
    loop {
      let ahead = match self.lexer.peek() {
        Ok(tok) => tok,
        Err(_) => break,
      };
      let mut pcd;
      if ahead.is_symbol_bin() {
        pcd = ahead.symbol_pcd();
      } else if ahead.is_keyword_bin(not_in) {
        pcd = ahead.keyword_pcd()
      } else {
        break;
      }
      if pcd < min_pcd {
        break;
      }
      self.lexer.advance();
      let op = ahead.deref().clone();
      let mut rhs = match self.unary_expr() {
        Ok(expr) => expr,
        Err(e) => return Err(e),
      };
      if let Ok(next_ok) = self.lexer.peek() {
        if next_ok.is_symbol_bin() {
          rhs = match self.expr_op(Some(rhs), pcd, not_in) {
            Ok(expr) => expr,
            Err(e) => return Err(e),
          };
        }
      }
      let bin = BinaryExpr {
        op,
        left: lhs,
        right: rhs,
      };
      lhs = bin.into();
    }
    Ok(lhs)
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

  fn unary_expr(&mut self) -> Result<Expr, ParsingError> {
    let tok = match self.lexer.peek() {
      Ok(tok) => tok,
      Err(e) => return Err(e.into()),
    };
    let ks = vec![Keyword::Delete, Keyword::Void, Keyword::Typeof];
    let ss = vec![
      Symbol::Inc,
      Symbol::Dec,
      Symbol::Add,
      Symbol::Sub,
      Symbol::BitNot,
      Symbol::Not,
    ];
    if tok.is_keyword_kind_in(&ks) || tok.is_symbol_kind_in(&ss) {
      let op = self.lexer.next().ok().unwrap().deref().clone();
      let argument = match self.unary_expr() {
        Ok(expr) => expr,
        Err(e) => return Err(e),
      };
      let expr = UnaryExpr {
        op,
        argument,
        prefix: true,
      };
      Ok(expr.into())
    } else {
      self.postfix_expr()
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

  #[test]
  fn unary_expr() {
    init_token_data();

    let code = String::from("++--a(b)(c)[d][e]");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.expr().ok().unwrap();
    assert!(node.is_unary());
    let node = node.unary();
    assert!(node.op.is_symbol_kind(Symbol::Inc));

    let node = node.argument.unary();
    assert!(node.argument.is_member());

    let node = node.argument.member();
    assert_eq!("e", node.property.primary().id().name);

    let node = node.object.member();
    assert_eq!("d", node.property.primary().id().name);

    let node = node.object.call_expr();
    assert_eq!("c", node.arguments[0].primary().id().name);

    let node = node.callee.call_expr();
    assert_eq!("b", node.arguments[0].primary().id().name);
    assert_eq!("a", node.callee.primary().id().name);
  }

  #[test]
  fn expr_op() {
    init_token_data();

    let mut code = String::from("a + b * c");
    let mut src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.expr().ok().unwrap();
    let node = node.bin_expr();
    let right = node.right.bin_expr();
    assert!(right.op.is_symbol_kind(Symbol::Mul));
    assert_eq!("b", right.left.primary().id().name);

    let mut code = String::from("a + b++ * c");
    let mut src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.expr().ok().unwrap();
    let node = node.bin_expr();
    let right = node.right.bin_expr();
    assert!(right.op.is_symbol_kind(Symbol::Mul));
    let left = right.left.unary();
    assert_eq!("b", left.argument.primary().id().name);

    let mut code = String::from("a + b++ * ++c");
    let mut src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.expr().ok().unwrap();
    let node = node.bin_expr();
    let right = node.right.bin_expr();
    assert!(right.op.is_symbol_kind(Symbol::Mul));
    let left = right.left.unary();
    assert_eq!("b", left.argument.primary().id().name);
    let right = right.right.unary();
    assert_eq!("c", right.argument.primary().id().name);

    let mut code = String::from("a in b + c");
    let mut src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.expr().ok().unwrap();
    let node = node.bin_expr();
    assert!(node.op.is_keyword_kind(Keyword::In));
    let right = node.right.bin_expr();
    assert!(right.op.is_symbol_kind(Symbol::Add));
    assert_eq!("b", right.left.primary().id().name);
  }
}
