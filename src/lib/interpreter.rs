use crate::ast::*;
use crate::lexer::*;
use crate::object::*;
use crate::parser::*;
use crate::token::*;
use crate::visitor::AstVisitor;
use std::collections::HashMap;

pub struct Interpreter {}

pub struct RuntimeError {}

impl AstVisitor<Option<JSObjPtr>, RuntimeError> for Interpreter {
  fn prog(&mut self, prog: &Prog) -> Result<Option<JSObjPtr>, RuntimeError> {
    let mut ret = Ok(None);
    for stmt in &prog.body {
      ret = match self.stmt(stmt) {
        Err(e) => return Err(e),
        Ok(v) => Ok(v),
      }
    }
    ret
  }

  fn block_stmt(&mut self, stmt: &BlockStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn var_dec_stmt(&mut self, stmt: &VarDec) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn empty_stmt(&mut self, stmt: &EmptyStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn expr_stmt(&mut self, stmt: &ExprStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    self.expr(&stmt.expr)
  }

  fn if_stmt(&mut self, stmt: &IfStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn for_stmt(&mut self, stmt: &ForStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn for_in_stmt(&mut self, stmt: &ForInStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn do_while_stmt(&mut self, stmt: &DoWhileStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn while_stmt(&mut self, stmt: &WhileStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn cont_stmt(&mut self, stmt: &ContStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn break_stmt(&mut self, stmt: &BreakStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn ret_stmt(&mut self, stmt: &ReturnStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn with_stmt(&mut self, stmt: &WithStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn switch_stmt(&mut self, stmt: &SwitchStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn throw_stmt(&mut self, stmt: &ThrowStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn try_stmt(&mut self, stmt: &TryStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn debug_stmt(&mut self, stmt: &DebugStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn fn_stmt(&mut self, stmt: &FnDec) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn member_expr(&mut self, expr: &MemberExpr) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn new_expr(&mut self, expr: &NewExpr) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn call_expr(&mut self, expr: &CallExpr) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn unary_expr(&mut self, expr: &UnaryExpr) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn binary_expr(&mut self, expr: &BinaryExpr) -> Result<Option<JSObjPtr>, RuntimeError> {
    let lhs = match self.expr(&expr.left) {
      Ok(v) => v.unwrap(),
      Err(e) => return Err(e),
    };
    let rhs = match self.expr(&expr.right) {
      Ok(v) => v.unwrap(),
      Err(e) => return Err(e),
    };
    let op = expr.op.symbol_data();
    match op.kind {
      Symbol::Add => Ok(Some(JSObject::add(lhs, rhs))),
      _ => Ok(Some(js_undef())),
    }
  }

  fn assign_expr(&mut self, expr: &AssignExpr) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn cond_expr(&mut self, expr: &CondExpr) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn seq_expr(&mut self, expr: &SeqExpr) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn this_expr(&mut self, expr: &ThisExprData) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn id_expr(&mut self, expr: &IdData) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn array_literal(&mut self, expr: &ArrayData) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn object_literal(&mut self, expr: &ObjectData) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn paren_expr(&mut self, expr: &ParenData) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn fn_expr(&mut self, expr: &FnDec) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn regexp_expr(&mut self, expr: &RegExpData) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn null_expr(&mut self, expr: &NullData) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn str_expr(&mut self, expr: &StringData) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn bool_expr(&mut self, expr: &BoolData) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn num_expr(&mut self, expr: &NumericData) -> Result<Option<JSObjPtr>, RuntimeError> {
    let ret = JSNumber::from_str(&expr.value) as JSObjPtr;
    Ok(Some(ret))
  }
}

pub struct Env {
  outer: Option<Box<Env>>,
  inner: Vec<Env>,
  upvals: Vec<String>,
  maps: HashMap<String, JSObjPtr>,
}

#[cfg(test)]
mod interp_tests {
  use super::*;
  use crate::source::*;

  #[test]
  fn exec_test() {
    init_token_data();

    let code = String::from("1 + 2");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);
    let mut interp = Interpreter {};
    let node = parser.prog().ok().unwrap();
    let res = interp.prog(&node).ok().unwrap().unwrap();
    JSObject::dump(res);

    let v: f64 = "1e1".parse().unwrap();
    println!("{:#?}", v);
  }
}
