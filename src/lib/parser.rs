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

  pub fn prog(&mut self) -> Result<Prog, ParsingError> {
    let mut body = vec![];
    loop {
      match self.lexer.peek() {
        Ok(tok) => {
          if tok.is_eof() {
            break;
          }
        }
        Err(e) => return Err(e.into()),
      }
      match self.stmt() {
        Ok(stmt) => body.push(stmt),
        Err(e) => return Err(e),
      }
    }
    Ok(Prog { body })
  }

  fn stmt(&mut self) -> Result<Stmt, ParsingError> {
    if self.ahead_is_symbol(Symbol::BraceL) {
      self.block_stmt()
    } else if self.ahead_is_keyword(Keyword::Var) {
      self.var_dec_stmt(false, false)
    } else if self.ahead_is_keyword(Keyword::If) {
      self.if_stmt()
    } else if self.ahead_is_keyword(Keyword::For) {
      self.for_stmt()
    } else if self.ahead_is_keyword(Keyword::Do) {
      self.do_while()
    } else if self.ahead_is_keyword(Keyword::While) {
      self.while_stmt()
    } else if self.ahead_is_keyword(Keyword::Continue) {
      self.cont_stmt()
    } else if self.ahead_is_keyword(Keyword::Break) {
      self.break_stmt()
    } else if self.ahead_is_keyword(Keyword::Return) {
      self.ret_stmt()
    } else if self.ahead_is_keyword(Keyword::With) {
      self.with_stmt()
    } else if self.ahead_is_keyword(Keyword::Switch) {
      self.switch_stmt()
    } else if self.ahead_is_keyword(Keyword::Debugger) {
      self.debug_stmt()
    } else if self.ahead_is_keyword(Keyword::Throw) {
      self.throw_stmt()
    } else if self.ahead_is_keyword(Keyword::Try) {
      self.try_stmt()
    } else if self.ahead_is_keyword(Keyword::Function) {
      self.fn_dec_stmt()
    } else if self.ahead_is_symbol(Symbol::Semi) {
      self.empty_stmt()
    } else {
      self.expr_stmt()
    }
  }

  fn fn_dec_stmt(&mut self) -> Result<Stmt, ParsingError> {
    let tok = self.lexer.next().ok().unwrap();
    let expr = match self.fn_expr(&tok) {
      Ok(expr) => expr,
      Err(e) => return Err(e),
    };

    let fun = expr.fn_expr().clone();
    Ok(Stmt::Function(fun))
  }

  fn throw_stmt(&mut self) -> Result<Stmt, ParsingError> {
    let mut loc = self.loc();
    self.lexer.advance();

    let argument = match self.expr(false) {
      Ok(expr) => expr,
      Err(e) => return Err(e),
    };

    loc.end = self.pos();
    Ok(ThrowStmt { loc, argument }.into())
  }

  fn try_stmt(&mut self) -> Result<Stmt, ParsingError> {
    let mut loc = self.loc();
    self.lexer.advance();

    let block = match self.block_stmt() {
      Ok(stmt) => stmt,
      Err(e) => return Err(e),
    };

    let mut handler = None;
    let mut finalizer = None;
    if self.ahead_is_keyword(Keyword::Catch) {
      handler = match self.try_catch_clause() {
        Ok(cc) => Some(cc),
        Err(e) => return Err(e),
      };
    } else if self.ahead_is_keyword(Keyword::Finally) {
      finalizer = match self.block_stmt() {
        Ok(stmt) => Some(stmt),
        Err(e) => return Err(e),
      };
    }

    loc.end = self.pos();
    let stmt = TryStmt {
      loc,
      block,
      handler,
      finalizer,
    };
    Ok(stmt.into())
  }

  fn try_catch_clause(&mut self) -> Result<CatchClause, ParsingError> {
    self.lexer.advance();

    match self.ahead_is_symbol(Symbol::ParenL) {
      true => self.lexer.advance(),
      false => return Err(ParserError::at(&self.loc()).into()),
    };

    let id = match self.primary_expr() {
      Ok(id) => id,
      Err(e) => return Err(e),
    };

    match self.ahead_is_symbol(Symbol::ParenR) {
      true => self.lexer.advance(),
      false => return Err(ParserError::at(&self.loc()).into()),
    };

    let body = match self.block_stmt() {
      Ok(stmt) => stmt,
      Err(e) => return Err(e),
    };

    Ok(CatchClause { id, body }.into())
  }

  fn debug_stmt(&mut self) -> Result<Stmt, ParsingError> {
    let mut loc = self.loc();
    self.lexer.advance();
    loc.end = self.pos();
    Ok(DebugStmt { loc }.into())
  }

  fn switch_stmt(&mut self) -> Result<Stmt, ParsingError> {
    let mut loc = self.loc();
    self.lexer.advance();

    match self.ahead_is_symbol(Symbol::ParenL) {
      true => self.lexer.advance(),
      false => return Err(ParserError::at(&self.loc()).into()),
    };

    let discrim = match self.expr(false) {
      Ok(expr) => expr,
      Err(e) => return Err(e),
    };

    match self.ahead_is_symbol(Symbol::ParenR) {
      true => self.lexer.advance(),
      false => return Err(ParserError::at(&self.loc()).into()),
    };

    match self.ahead_is_symbol(Symbol::BraceL) {
      true => self.lexer.advance(),
      false => return Err(ParserError::at(&self.loc()).into()),
    };

    let mut cases = vec![];
    let mut met_default = false;
    loop {
      if self.ahead_is_symbol(Symbol::BraceR) {
        self.lexer.advance();
        break;
      } else if self.ahead_is_keyword(Keyword::Case) {
        match self.switch_case() {
          Ok(case) => cases.push(case),
          Err(e) => return Err(e),
        };
      } else if self.ahead_is_keyword(Keyword::Default) {
        if !met_default {
          met_default = true;
          match self.switch_default() {
            Ok(case) => cases.push(case),
            Err(e) => return Err(e),
          };
        } else {
          return Err(ParserError::at(&self.loc()).into());
        }
      }
    }

    loc.end = self.pos();
    let switch = SwitchStmt {
      loc,
      discrim,
      cases,
    };
    Ok(switch.into())
  }

  fn switch_default(&mut self) -> Result<SwitchCase, ParsingError> {
    self.lexer.advance();

    match self.ahead_is_symbol(Symbol::Colon) {
      true => self.lexer.advance(),
      false => return Err(ParserError::at(&self.loc()).into()),
    };

    let mut cons = vec![];
    loop {
      let stmt = match self.stmt() {
        Ok(stmt) => stmt,
        Err(e) => return Err(e),
      };
      cons.push(stmt);
      if self.ahead_is_keyword(Keyword::Case) || self.ahead_is_symbol(Symbol::BraceR) {
        break;
      }
    }

    Ok(SwitchCase { test: None, cons })
  }

  fn switch_case(&mut self) -> Result<SwitchCase, ParsingError> {
    self.lexer.advance();

    let test = match self.expr(false) {
      Ok(expr) => Some(expr),
      Err(e) => return Err(e),
    };

    match self.ahead_is_symbol(Symbol::Colon) {
      true => self.lexer.advance(),
      false => return Err(ParserError::at(&self.loc()).into()),
    };

    let mut cons = vec![];
    loop {
      let stmt = match self.stmt() {
        Ok(stmt) => stmt,
        Err(e) => return Err(e),
      };
      cons.push(stmt);
      if self.ahead_is_keyword(Keyword::Case) || self.ahead_is_symbol(Symbol::BraceR) {
        break;
      }
    }

    Ok(SwitchCase { test, cons })
  }

  fn with_stmt(&mut self) -> Result<Stmt, ParsingError> {
    let mut loc = self.loc();
    self.lexer.advance();

    match self.ahead_is_symbol(Symbol::ParenL) {
      true => self.lexer.advance(),
      false => return Err(ParserError::at(&self.loc()).into()),
    };

    let object = match self.expr(false) {
      Ok(expr) => expr,
      Err(e) => return Err(e),
    };

    match self.ahead_is_symbol(Symbol::ParenR) {
      true => self.lexer.advance(),
      false => return Err(ParserError::at(&self.loc()).into()),
    };

    let body = match self.stmt() {
      Ok(stmt) => stmt,
      Err(e) => return Err(e),
    };

    loc.end = self.pos();
    let stmt = WithStmt { loc, object, body };
    Ok(stmt.into())
  }

  fn empty_stmt(&mut self) -> Result<Stmt, ParsingError> {
    let mut loc = self.loc();
    self.lexer.advance();
    loc.end = self.pos();
    Ok(EmptyStmt { loc }.into())
  }

  fn ret_stmt(&mut self) -> Result<Stmt, ParsingError> {
    let loc = self.loc();
    self.lexer.advance();

    let mut ret = ReturnStmt {
      loc,
      argument: None,
    };
    if self.lexer.next_is_line_terminator {
      if self.ahead_is_symbol(Symbol::Semi) {
        self.lexer.advance();
      }
      ret.loc.end = self.pos();
      return Ok(ret.into());
    }

    if !self.ahead_is_symbol(Symbol::BraceR) {
      if self.ahead_is_symbol(Symbol::Semi) {
        self.lexer.advance();
      } else {
        ret.argument = match self.expr(false) {
          Ok(expr) => Some(expr),
          Err(e) => return Err(e),
        };
      }
    }

    ret.loc.end = self.pos();
    Ok(ret.into())
  }

  fn break_stmt(&mut self) -> Result<Stmt, ParsingError> {
    let mut loc = self.loc();
    self.lexer.advance();
    loc.end = self.pos();
    if self.ahead_is_symbol(Symbol::Semi) {
      self.lexer.advance();
    }
    Ok(BreakStmt { loc }.into())
  }

  fn cont_stmt(&mut self) -> Result<Stmt, ParsingError> {
    let mut loc = self.loc();
    self.lexer.advance();
    loc.end = self.pos();
    if self.ahead_is_symbol(Symbol::Semi) {
      self.lexer.advance();
    }
    Ok(ContStmt { loc }.into())
  }

  fn while_stmt(&mut self) -> Result<Stmt, ParsingError> {
    let mut loc = self.loc();
    self.lexer.advance();

    match self.ahead_is_symbol(Symbol::ParenL) {
      true => self.lexer.advance(),
      false => return Err(ParserError::at(&self.loc()).into()),
    };

    let test = match self.expr(false) {
      Ok(expr) => expr,
      Err(e) => return Err(e),
    };

    match self.ahead_is_symbol(Symbol::ParenR) {
      true => self.lexer.advance(),
      false => return Err(ParserError::at(&self.loc()).into()),
    };

    let body = match self.stmt() {
      Ok(stmt) => stmt,
      Err(e) => return Err(e),
    };

    loc.end = self.pos();
    let stmt = WhileStmt { loc, test, body };
    Ok(stmt.into())
  }

  fn do_while(&mut self) -> Result<Stmt, ParsingError> {
    let mut loc = self.loc();
    self.lexer.advance();

    let body = match self.stmt() {
      Ok(stmt) => stmt,
      Err(e) => return Err(e),
    };

    match self.ahead_is_keyword(Keyword::While) {
      true => self.lexer.advance(),
      false => return Err(ParserError::at(&self.loc()).into()),
    };

    match self.ahead_is_symbol(Symbol::ParenL) {
      true => self.lexer.advance(),
      false => return Err(ParserError::at(&self.loc()).into()),
    };

    let test = match self.expr(false) {
      Ok(expr) => expr,
      Err(e) => return Err(e),
    };

    match self.ahead_is_symbol(Symbol::ParenR) {
      true => self.lexer.advance(),
      false => return Err(ParserError::at(&self.loc()).into()),
    };

    loc.end = self.pos();
    let stmt = DoWhileStmt { loc, test, body };
    Ok(stmt.into())
  }

  fn for_stmt(&mut self) -> Result<Stmt, ParsingError> {
    let mut loc = self.loc();
    self.lexer.advance();

    if !self.ahead_is_symbol(Symbol::ParenL) {
      return Err(ParserError::at(&self.loc()).into());
    }
    self.lexer.advance();

    let mut first = None;
    let mut left = None;
    if self.ahead_is_keyword(Keyword::Var) {
      first = match self.for_var() {
        Ok(expr) => Some(expr),
        Err(e) => return Err(e),
      };
    } else {
      left = match self.expr(true) {
        Ok(expr) => Some(expr),
        Err(e) => return Err(e),
      };
    }

    let is_for_in = self.ahead_is_keyword(Keyword::In);

    let for_in_left;
    if first.is_some() {
      let first = first.unwrap();
      if is_for_in && first.decs.len() > 1 {
        return Err(ParserError::at(&first.loc).into());
      }
      for_in_left = ForFirst::VarDec(first);
    } else if left.is_some() {
      for_in_left = ForFirst::Expr(left.unwrap());
    } else {
      return Err(ParserError::at(&self.loc()).into());
    }

    if is_for_in {
      self.lexer.advance();

      let right = match self.expr(false) {
        Ok(expr) => expr,
        Err(e) => return Err(e),
      };

      if !self.ahead_is_symbol(Symbol::ParenR) {
        return Err(ParserError::at(&self.loc()).into());
      }
      self.lexer.advance();

      let body = match self.stmt() {
        Ok(stmt) => stmt,
        Err(e) => return Err(e),
      };

      loc.end = self.pos();
      return Ok(
        ForInStmt {
          loc,
          left: for_in_left,
          right,
          body,
        }
        .into(),
      );
    }

    if !self.ahead_is_symbol(Symbol::Semi) {
      return Err(ParserError::at(&self.loc()).into());
    }
    self.lexer.advance();

    let test = match self.ahead_is_symbol(Symbol::Semi) {
      true => {
        self.lexer.advance();
        None
      }
      false => match self.expr(false) {
        Ok(expr) => Some(expr),
        Err(e) => return Err(e),
      },
    };

    let update = match self.ahead_is_symbol(Symbol::Semi) {
      true => {
        self.lexer.advance();
        None
      }
      false => match self.expr(false) {
        Ok(expr) => Some(expr),
        Err(e) => return Err(e),
      },
    };

    if !self.ahead_is_symbol(Symbol::ParenR) {
      return Err(ParserError::at(&self.loc()).into());
    }
    self.lexer.advance();

    let body = match self.stmt() {
      Ok(stmt) => stmt,
      Err(e) => return Err(e),
    };

    loc.end = self.pos();
    Ok(
      ForStmt {
        loc,
        init: Some(for_in_left),
        test,
        update,
        body,
      }
      .into(),
    )
  }

  fn for_var(&mut self) -> Result<Rc<VarDec>, ParsingError> {
    match self.var_dec_stmt(true, true) {
      Ok(stmt) => match stmt {
        Stmt::VarDec(stmt) => Ok(stmt.clone()),
        _ => panic!(), // unreachable
      },
      Err(e) => Err(e),
    }
  }

  fn if_stmt(&mut self) -> Result<Stmt, ParsingError> {
    let mut loc = self.loc();
    self.lexer.advance();

    if !self.ahead_is_symbol(Symbol::ParenL) {
      return Err(ParserError::at(&self.loc()).into());
    }
    self.lexer.advance();

    let test = match self.expr(false) {
      Ok(expr) => expr,
      Err(e) => return Err(e),
    };

    if !self.ahead_is_symbol(Symbol::ParenR) {
      return Err(ParserError::at(&self.loc()).into());
    }
    self.lexer.advance();

    let cons = match self.stmt() {
      Ok(stmt) => stmt,
      Err(e) => return Err(e),
    };

    let mut alt: Option<Stmt> = None;
    if self.ahead_is_keyword(Keyword::Else) {
      self.lexer.advance();
      alt = match self.stmt() {
        Ok(stmt) => Some(stmt),
        Err(e) => return Err(e),
      };
    }

    loc.end = self.pos();
    let s = IfStmt {
      loc,
      test,
      cons,
      alt,
    };
    Ok(s.into())
  }

  fn var_dec_stmt(&mut self, not_in: bool, leave_semi: bool) -> Result<Stmt, ParsingError> {
    let mut loc = self.loc();
    self.lexer.advance();
    let mut decs = vec![];
    loop {
      match self.var_decor(not_in) {
        Ok(dec) => decs.push(dec),
        Err(e) => return Err(e),
      }
      if self.ahead_is_symbol(Symbol::Comma) {
        self.lexer.advance();
        continue;
      }
      if self.ahead_is_symbol(Symbol::Semi) && !leave_semi {
        self.lexer.advance();
      }
      break;
    }
    loc.end = self.pos();
    let var_dec = VarDec { loc, decs };
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
    let mut loc = self.loc();
    self.lexer.advance();
    let mut body = vec![];
    loop {
      match self.lexer.peek() {
        Ok(tok) => {
          if tok.is_symbol_kind(Symbol::BraceR) {
            self.lexer.advance();
            break;
          }
          match self.stmt() {
            Ok(stmt) => body.push(stmt),
            Err(e) => return Err(e),
          }
        }
        Err(e) => return Err(e.into()),
      }
    }
    loc.end = self.pos();
    let b = BlockStmt { loc, body };
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
    let mut loc = self.loc();
    let first = match self.assign_expr(not_in, None) {
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
      let expr = match self.assign_expr(not_in, None) {
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
    loc.end = self.pos();
    let seq = SeqExpr { loc, exprs: seq };
    Ok(seq.into())
  }

  fn ahead_is_symbol_assign(&mut self) -> bool {
    match self.lexer.peek() {
      Err(_) => false,
      Ok(tok) => tok.is_symbol_assign(),
    }
  }

  fn assign_expr(&mut self, not_in: bool, left: Option<Expr>) -> Result<Expr, ParsingError> {
    let mut loc = self.loc();
    let lhs = match left {
      Some(expr) => expr,
      _ => match self.cond_expr(not_in) {
        Ok(expr) => expr,
        Err(e) => return Err(e),
      },
    };

    if !self.ahead_is_symbol_assign() {
      return Ok(lhs);
    }
    let op = self.lexer.next().ok().unwrap().deref().clone();

    let mut rhs = match self.cond_expr(not_in) {
      Ok(expr) => expr,
      Err(e) => return Err(e),
    };

    if self.ahead_is_symbol_assign() {
      rhs = match self.assign_expr(not_in, Some(rhs)) {
        Ok(expr) => expr,
        Err(e) => return Err(e),
      };
    }

    loc.end = self.pos();
    let assign = AssignExpr {
      loc,
      op,
      left: lhs,
      right: rhs,
    };
    Ok(assign.into())
  }

  fn cond_expr(&mut self, not_in: bool) -> Result<Expr, ParsingError> {
    let mut loc = self.loc();
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

    loc.end = self.pos();
    let cond = CondExpr {
      loc,
      test,
      cons,
      alt,
    };
    Ok(cond.into())
  }

  fn expr_op(
    &mut self,
    lhs: Option<Expr>,
    min_pcd: i32,
    not_in: bool,
  ) -> Result<Expr, ParsingError> {
    let mut loc = self.loc();
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
      loc.end = self.pos();
      let bin = BinaryExpr {
        loc,
        op,
        left: lhs,
        right: rhs,
      };
      lhs = bin.into();
    }
    Ok(lhs)
  }

  fn postfix_expr(&mut self) -> Result<Expr, ParsingError> {
    let mut loc = self.loc();
    match self.lhs_expr() {
      Ok(expr) => {
        if let Ok(tok) = self.lexer.peek() {
          if tok.is_symbol_kind(Symbol::Inc) || tok.is_symbol_kind(Symbol::Dec) {
            let tok = self.lexer.next().ok().unwrap();
            loc.end = self.pos();
            let expr: Expr = UnaryExpr {
              loc,
              op: tok.deref().clone(),
              argument: expr,
              prefix: false,
            }
            .into();
            return Ok(expr);
          }
        }
        Ok(expr)
      }
      Err(e) => Err(e),
    }
  }

  fn unary_expr(&mut self) -> Result<Expr, ParsingError> {
    let mut loc = self.loc();
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
      loc.end = self.pos();
      let expr = UnaryExpr {
        loc,
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
    let mut loc = self.loc();
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
    loc.end = self.pos();
    let expr = NewExpr {
      loc,
      callee,
      arguments,
    };
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
    let mut loc = self.loc();
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
    loc.end = self.pos();
    let mut expr: Expr = CallExpr {
      loc,
      callee,
      arguments,
    }
    .into();
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
    let mut loc = self.loc();
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
              loc.end = self.pos();
              obj = Expr::Member(Rc::new(MemberExpr {
                loc,
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
          loc.end = self.pos();
          obj = Expr::Member(Rc::new(MemberExpr {
            loc,
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
        } else if tok.is_undef() {
          Ok(UndefData::new(loc).into())
        } else if tok.is_str() {
          Ok(StringData::new(loc, tok.str_data().value.clone()).into())
        } else if tok.is_null() {
          Ok(NullData::new(loc).into())
        } else if tok.is_bool() {
          Ok(BoolData::new(loc, tok.bool_data().kind == BooleanLiteral::True).into())
        } else if tok.is_num() {
          Ok(NumericData::new(loc, tok.num_data().value.clone()).into())
        } else if tok.is_symbol_kind(Symbol::BraceL) {
          self.object_literal(&tok)
        } else if tok.is_symbol_kind(Symbol::BracketL) {
          self.array_literal(&tok)
        } else if tok.is_symbol_kind(Symbol::ParenL) {
          self.paren_expr(&tok)
        } else if tok.is_keyword_kind(Keyword::Function) {
          self.fn_expr(&tok)
        } else {
          Err(ParserError::at_tok(&tok).into())
        }
      }
      Err(e) => Err(e.into()),
    }
  }

  fn paren_expr(&mut self, open: &Token) -> Result<PrimaryExpr, ParsingError> {
    let mut loc = open.loc().to_owned();
    let value = match self.expr(false) {
      Ok(expr) => expr,
      Err(e) => return Err(e),
    };
    match self.ahead_is_symbol(Symbol::ParenR) {
      true => self.lexer.advance(),
      false => return Err(ParserError::at(&self.loc()).into()),
    }
    loc.end = self.pos();
    Ok(ParenData { loc, value }.into())
  }

  fn fn_expr(&mut self, open: &Token) -> Result<PrimaryExpr, ParsingError> {
    let mut loc = open.loc().to_owned();

    let mut id = None;
    if !self.ahead_is_symbol(Symbol::ParenL) {
      id = match self.primary_expr() {
        Ok(expr) => {
          if !expr.is_id() {
            return Err(ParserError::at(expr.loc()).into());
          }
          Some(expr)
        }
        Err(e) => return Err(e),
      }
    }

    match self.ahead_is_symbol(Symbol::ParenL) {
      true => self.lexer.advance(),
      false => return Err(ParserError::at(&self.loc()).into()),
    }

    let mut params = vec![];
    loop {
      if self.ahead_is_symbol(Symbol::ParenR) {
        self.lexer.advance();
        break;
      }
      match self.primary_expr() {
        Ok(e) => {
          if !e.is_id() {
            return Err(ParserError::at(e.loc()).into());
          }
          params.push(e);
        }
        Err(e) => return Err(e),
      }
      if self.ahead_is_symbol(Symbol::Comma) {
        self.lexer.advance();
      }
    }

    loc.end = self.pos();
    let body = match self.block_stmt() {
      Ok(stmt) => stmt,
      Err(e) => return Err(e),
    };

    let dec = FnDec {
      loc,
      id,
      params,
      body,
    };
    Ok(dec.into())
  }

  fn object_literal(&mut self, open: &Token) -> Result<PrimaryExpr, ParsingError> {
    let mut ret = ObjectData {
      loc: open.loc().to_owned(),
      properties: vec![],
    };
    loop {
      match self.lexer.peek() {
        Ok(tok) => {
          if tok.is_symbol_kind(Symbol::BraceR) {
            match self.lexer.next() {
              Err(e) => return Err(e.into()),
              _ => break,
            }
          } else if tok.is_symbol_kind(Symbol::Comma) {
            match self.lexer.next() {
              Err(e) => return Err(e.into()),
              _ => (),
            }
          }
          match self.object_prop() {
            Ok(prop) => ret.properties.push(prop),
            Err(e) => return Err(e),
          }
        }
        Err(e) => return Err(e.into()),
      }
    }
    ret.loc.end = self.pos();
    Ok(PrimaryExpr::ObjectLiteral(ret))
  }

  fn object_prop(&mut self) -> Result<ObjectProperty, ParsingError> {
    let mut loc = self.loc();
    let key = match self.lexer.next() {
      Ok(tok) => {
        if tok.is_str() {
          PrimaryExpr::Literal(Literal::String(StringData {
            loc: tok.loc().to_owned(),
            value: tok.str_data().value.clone(),
          }))
        } else if tok.is_id() {
          PrimaryExpr::Identifier(IdData {
            loc: tok.loc().to_owned(),
            name: tok.id_data().value.clone(),
          })
        } else {
          return Err(ParserError::at(tok.loc()).into());
        }
      }
      Err(e) => return Err(e.into()),
    };
    match self.lexer.next() {
      Ok(tok) => {
        if !tok.is_symbol_kind(Symbol::Colon) {
          return Err(ParserError::at(tok.loc()).into());
        }
      }
      Err(e) => return Err(e.into()),
    }
    let value = match self.assign_expr(false, None) {
      Ok(expr) => expr,
      Err(e) => return Err(e.into()),
    };
    loc.end = self.pos();
    Ok(ObjectProperty {
      loc,
      key: key.into(),
      value,
    })
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
              _ => {
                let expr =
                  Expr::Primary(Rc::new(PrimaryExpr::Literal(Literal::Undef(UndefData {
                    loc: tok.loc().clone(),
                  }))));
                ret.value.push(expr)
              }
            }
          } else {
            match self.assign_expr(false, None) {
              Ok(expr) => ret.value.push(expr),
              Err(e) => return Err(e.into()),
            };
            if self.ahead_is_symbol(Symbol::Comma) {
              self.lexer.advance();
            }
          }
        }
        Err(e) => return Err(e.into()),
      }
    }
    Ok(ret.into())
  }

  fn pos(&mut self) -> Position {
    self.lexer.pos()
  }

  fn loc(&mut self) -> SourceLoc {
    self.lexer.loc()
  }
}

#[derive(Debug)]
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

#[derive(Debug)]
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
mod parser_tests {
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

  #[test]
  fn if_stmt() {
    init_token_data();

    let code = String::from("if (true) 1 + 2 else { 3 }");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.stmt().ok().unwrap();
    assert!(node.is_if());
    let if_stmt = node.if_stmt();
    let cons = if_stmt.cons.expr().expr.bin_expr();
    assert!(cons.op.is_symbol_kind(Symbol::Add));
    let alt = if_stmt.alt.as_ref().unwrap();
    let alt = alt.block();
    let alt_body = &alt.body;
    assert_eq!(1, alt_body.len());
    assert_eq!("3", alt_body[0].expr().expr.primary().literal().num().value);
  }

  #[test]
  fn for_stmt() {
    init_token_data();

    let code = String::from("for(var a;b;) {}");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.stmt().ok().unwrap();
    assert!(node.is_for());
  }

  #[test]
  fn for_in_stmt() {
    init_token_data();

    let code = String::from("for(var a in b) {}");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.stmt().ok().unwrap();
    assert!(node.is_for_in());
  }

  #[test]
  fn do_while_stmt() {
    init_token_data();

    let code = String::from("do 1 + 2 while(true)");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.stmt().ok().unwrap();
    assert!(node.is_do_while());
  }

  #[test]
  fn while_stmt() {
    init_token_data();

    let code = String::from("while(true) 1 + 2");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.stmt().ok().unwrap();
    assert!(node.is_while_stmt());
  }

  #[test]
  fn cont_ret_break_stmt() {
    init_token_data();

    let code = String::from(
      "continue; return
    break",
    );
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.stmt().ok().unwrap();
    assert!(node.is_cont());

    let node = parser.stmt().ok().unwrap();
    assert!(node.is_ret());

    let node = parser.stmt().ok().unwrap();
    assert!(node.is_break());
  }

  #[test]
  fn ret_stmt() {
    init_token_data();

    let code = String::from(
      "return
      1;;",
    );
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.stmt().ok().unwrap();
    assert!(node.is_ret());

    let node = parser.stmt().ok().unwrap();
    assert!(node.is_expr());

    let node = parser.stmt().ok().unwrap();
    assert!(node.is_empty());
  }

  #[test]
  fn with_stmt() {
    init_token_data();

    let code = String::from("with(a) 1");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.stmt().ok().unwrap();
    assert!(node.is_with());
  }

  #[test]
  fn switch_stmt() {
    init_token_data();

    let code = String::from(
      "switch (c) {
  default: break
  case 1:
    1 + 2;
    3 + 3;
}",
    );
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.stmt().ok().unwrap();
    assert!(node.is_switch());
    let switch = node.switch_stmt();
    assert_eq!(2, switch.cases.len());
    let case = &switch.cases[1];
    assert_eq!(2, case.cons.len());
  }

  #[test]
  fn debug_stmt() {
    init_token_data();

    let code = String::from("debugger;");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.stmt().ok().unwrap();
    assert!(node.is_debug());
  }

  #[test]
  fn try_stmt() {
    init_token_data();

    let code = String::from("try{1}catch(e) {}");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.stmt().ok().unwrap();
    assert!(node.is_try());
  }

  #[test]
  fn throw_stmt() {
    init_token_data();

    let code = String::from("throw 1");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.stmt().ok().unwrap();
    assert!(node.is_throw());
  }

  #[test]
  fn fn_expr() {
    init_token_data();

    let code = String::from("(function f(a,b,c) {return 1;})()");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.stmt().ok().unwrap();
    assert!(node.is_expr());

    let call = node.expr().expr.call_expr();
    let callee = call.callee.primary().paren().value.primary();
    assert!(callee.is_fn());
  }

  #[test]
  fn obj_literal() {
    init_token_data();

    let code = String::from(
      "    
    var a = {
      b: 1,
      c: [
        {d: 2}
      ],
      e: { 'f': 3 }
    }",
    );
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.stmt().ok().unwrap();
  }

  #[test]
  fn prog() {
    init_token_data();

    let code = String::from(
      "function f() {}
    var a, b
    a + b
    ",
    );
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);

    let node = parser.prog().ok().unwrap();
    assert_eq!(3, node.body.len());
  }
}
