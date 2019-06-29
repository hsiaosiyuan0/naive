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

  fn stmt(&mut self) -> Result<Stmt, ParsingError> {
    if self.ahead_is_symbol(Symbol::BraceL) {
      self.block_stmt()
    } else if self.ahead_is_keyword(Keyword::Var) {
      self.var_dec_stmt(false)
    } else {
      self.expr_stmt()
    }
  }

  fn var_dec_stmt(&mut self, not_in: bool) -> Result<Stmt, ParsingError> {
    self.lexer.advance();
    let mut var_dec = VarDec { decs: vec![] };
    loop {
      match self.var_decor(not_in) {
        Ok(dec) => var_dec.decs.push(dec),
        Err(e) => return Err(e),
      }
      if self.ahead_is_symbol(Symbol::Comma) {
        self.lexer.advance();
        continue;
      }
      if self.ahead_is_symbol(Symbol::Semi) {
        self.lexer.advance();
      }
      break;
    }
    Ok(var_dec.into())
  }

  fn var_decor(&mut self, not_in: bool) -> Result<VarDecor, ParsingError> {
    let id = match self.primary_expr() {
      Ok(expr) => {
        if !expr.is_id() {
          return Err(ParserError::at(expr.literal().loc()).into());
        }
        expr
      }
      Err(e) => return Err(e),
    };
    let mut dec = VarDecor { id, init: None };
    if !self.ahead_is_symbol(Symbol::Assign) {
      return Ok(dec);
    }
    self.lexer.advance();
    dec.init = match self.cond_expr(not_in) {
      Ok(expr) => Some(expr),
      Err(e) => return Err(e),
    };
    return Ok(dec);
  }

  fn block_stmt(&mut self) -> Result<Stmt, ParsingError> {
    self.lexer.advance();
    let mut b = BlockStmt { body: vec![] };
    loop {
      match self.lexer.peek() {
        Ok(tok) => {
          if tok.is_symbol_kind(Symbol::BraceR) {
            self.lexer.advance();
            break;
          }
          match self.stmt() {
            Ok(stmt) => b.body.push(stmt),
            Err(e) => return Err(e),
          }
        }
        Err(e) => return Err(e.into()),
      }
    }
    Ok(b.into())
  }

  fn expr_stmt(&mut self) -> Result<Stmt, ParsingError> {
    match self.expr(false) {
      Ok(expr) => {
        if self.ahead_is_symbol(Symbol::Semi) {
          self.lexer.advance();
        }
        Ok(ExprStmt { expr }.into())
      }
      Err(e) => Err(e),
    }
  }

  fn expr(&mut self, not_in: bool) -> Result<Expr, ParsingError> {
    let first = match self.assign_expr(not_in) {
      Ok(expr) => expr,
      Err(e) => return Err(e),
    };

    let tok = self.lexer.peek();
    if tok.is_err() {
      return Ok(first);
    }
    let tok = tok.ok().unwrap();
    if !tok.is_symbol_kind(Symbol::Comma) {
      return Ok(first);
    }
    self.lexer.advance();

    let mut seq: Vec<Expr> = vec![first];
    loop {
      let expr = match self.assign_expr(not_in) {
        Ok(expr) => expr,
        Err(e) => return Err(e),
      };
      seq.push(expr);
      if self.ahead_is_symbol(Symbol::Comma) {
        self.lexer.advance();
      } else {
        break;
      }
    }
    let seq = SeqExpr { exprs: seq };
    Ok(seq.into())
  }

  fn assign_expr(&mut self, not_in: bool) -> Result<Expr, ParsingError> {
    let lhs = match self.cond_expr(not_in) {
      Ok(expr) => expr,
      Err(e) => return Err(e),
    };

    let op = self.lexer.peek();
    if op.is_err() {
      return Ok(lhs);
    }
    let op = op.ok().unwrap().deref().clone();
    if !op.is_symbol_assign() {
      return Ok(lhs);
    }
    self.lexer.advance();

    let rhs = match self.cond_expr(not_in) {
      Ok(expr) => expr,
      Err(e) => return Err(e),
    };

    let assign = AssignExpr {
      op,
      left: lhs,
      right: rhs,
    };
    Ok(assign.into())
  }

  fn cond_expr(&mut self, not_in: bool) -> Result<Expr, ParsingError> {
    let test = match self.expr_op(None, 0, not_in) {
      Ok(expr) => expr,
      Err(e) => return Err(e),
    };

    let tok = self.lexer.peek();
    if tok.is_err() {
      return Ok(test);
    }
    let tok = tok.ok().unwrap();
    if !tok.is_symbol_kind(Symbol::Conditional) {
      return Ok(test);
    }
    self.lexer.advance();

    let cons = match self.expr_op(None, 0, not_in) {
      Ok(expr) => expr,
      Err(e) => return Err(e),
    };

    match self.lexer.next() {
      Ok(tok) => match tok.is_symbol_kind(Symbol::Colon) {
        false => return Err(ParserError::at_tok(&tok).into()),
        true => (),
      },
      Err(e) => return Err(e.into()),
    }

    let alt = match self.expr_op(None, 0, not_in) {
      Ok(expr) => expr,
      Err(e) => return Err(e),
    };

    let cond = CondExpr { test, cons, alt };
    Ok(cond.into())
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
      let pcd;
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
      match self.expr(false) {
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
                return Err(ParserError::at_tok(&tok).into());
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
          let prop = match self.expr(false) {
            Ok(expr) => expr,
            Err(e) => return Err(e.into()),
          };
          match self.lexer.next() {
            Ok(tok) => {
              if !tok.is_symbol_kind(Symbol::BracketR) {
                return Err(ParserError::at_tok(&tok).into());
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
          Err(ParserError::at_tok(&tok).into())
        }
      }
      Err(e) => Err(e.into()),
    }
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
            match self.assign_expr(false) {
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

  pub fn at(loc: &SourceLoc) -> Self {
    ParserError {
      msg: format!(
        "Unexpected token at line: {} column: {}",
        loc.start.line, loc.start.column
      ),
    }
  }

  pub fn at_tok(tok: &Token) -> Self {
    let loc = tok.loc();
    ParserError::at(loc)
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

    let code = String::from("a.b++ a[c+1][d]--");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.expr(false);
    assert!(!node.is_err());
    let node = node.ok().unwrap();
    assert!(node.is_unary());
    let node = node.unary();
    assert!(node.argument.is_member());
    assert_eq!("a", node.argument.member().object.primary().id().name);

    let node = parser.expr(false);
    assert!(!node.is_err());
    let node = node.ok().unwrap();
    assert!(node.is_unary());
    assert_eq!(false, node.unary().prefix);
    assert_eq!(true, node.unary().op.is_symbol_kind(Symbol::Dec));
    assert!(node.unary().argument.is_member());
    let node = node.unary().argument.member();
    assert!(node.object.is_member());

    let prop = node.object.member().property.bin_expr();
    assert_eq!("c", prop.left.primary().id().name);
    assert_eq!("1", prop.right.primary().literal().num().value);
  }

  #[test]
  fn new_expr() {
    init_token_data();

    let code = String::from("new new a(b)");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.expr(false);
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

    let node = parser.expr(false);
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

    let node = parser.expr(false);
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

    let node = parser.expr(false).ok().unwrap();
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

    let code = String::from("a + b * c");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.expr(false).ok().unwrap();
    let node = node.bin_expr();
    let right = node.right.bin_expr();
    assert!(right.op.is_symbol_kind(Symbol::Mul));
    assert_eq!("b", right.left.primary().id().name);

    let code = String::from("a + b++ * c");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.expr(false).ok().unwrap();
    let node = node.bin_expr();
    let right = node.right.bin_expr();
    assert!(right.op.is_symbol_kind(Symbol::Mul));
    let left = right.left.unary();
    assert_eq!("b", left.argument.primary().id().name);

    let code = String::from("a + b++ * ++c");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.expr(false).ok().unwrap();
    let node = node.bin_expr();
    let right = node.right.bin_expr();
    assert!(right.op.is_symbol_kind(Symbol::Mul));
    let left = right.left.unary();
    assert_eq!("b", left.argument.primary().id().name);
    let right = right.right.unary();
    assert_eq!("c", right.argument.primary().id().name);

    let code = String::from("a in b + c");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.expr(false).ok().unwrap();
    let node = node.bin_expr();
    assert!(node.op.is_keyword_kind(Keyword::In));
    let right = node.right.bin_expr();
    assert!(right.op.is_symbol_kind(Symbol::Add));
    assert_eq!("b", right.left.primary().id().name);
  }

  #[test]
  fn cond_expr() {
    init_token_data();

    let code = String::from("a + b ? c * d : f / e");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.expr(false).ok().unwrap();
    assert!(node.is_cond());
    let node = node.cond_expr();
    let alt = node.alt.bin_expr();
    assert_eq!("f", alt.left.primary().id().name);
  }

  #[test]
  fn assign_expr() {
    init_token_data();

    let code = String::from("v = a + b ? c in d : f / e");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.expr(false).ok().unwrap();
    assert!(node.is_assign());
    let assign = node.assign_expr();
    let lhs = assign.left.primary();
    assert_eq!("v", lhs.id().name);

    let rhs = assign.right.cond_expr();
    let test = rhs.cons.bin_expr();
    assert!(test.op.is_keyword_kind(Keyword::In));
  }

  #[test]
  fn seq_expr() {
    init_token_data();

    let code = String::from("1 + 2, 3 + d, [e, f]");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.expr(false).ok().unwrap();
    assert!(node.is_seq());
    let seq = node.seq_expr();
    assert_eq!(3, seq.exprs.len());
    let arr = seq.exprs[2].primary().array();
    assert_eq!(2, arr.value.len());
  }

  #[test]
  fn block_stmt() {
    init_token_data();

    let code = String::from(
      "{
     a + b;
     1 + 2
    }",
    );
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.stmt().ok().unwrap();
    assert!(node.is_block());
    let block = node.block();
    assert_eq!(2, block.body.len());
    let stmt = block.body[0].expr();
    assert!(stmt.expr.is_bin());
  }

  #[test]
  fn var_dec_stmt() {
    init_token_data();

    let code = String::from("var a, b = c in d, e = 1 + 2");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.stmt().ok().unwrap();
    assert!(node.is_var());
    let var_dec = node.var_dec();
    assert_eq!(3, var_dec.decs.len());
  }
}
