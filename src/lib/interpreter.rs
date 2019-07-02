use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::token::*;
use crate::visitor::AstVisitor;

pub struct Interpreter {}

pub struct RuntimeError {}

impl AstVisitor<(), RuntimeError> for Interpreter {
  fn prog(&mut self, prog: &Prog) -> Result<(), RuntimeError> {
    for stmt in &prog.body {
      match self.stmt(stmt) {
        Err(e) => return Err(e),
        _ => (),
      }
    }
    Ok(())
  }

  fn block_stmt(&mut self, stmt: &BlockStmt) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn var_dec_stmt(&mut self, stmt: &VarDec) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn empty_stmt(&mut self, stmt: &EmptyStmt) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn expr_stmt(&mut self, stmt: &ExprStmt) -> Result<(), RuntimeError> {
    self.expr(&stmt.expr)
  }

  fn if_stmt(&mut self, stmt: &IfStmt) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn for_stmt(&mut self, stmt: &ForStmt) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn for_in_stmt(&mut self, stmt: &ForInStmt) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn do_while_stmt(&mut self, stmt: &DoWhileStmt) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn while_stmt(&mut self, stmt: &WhileStmt) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn cont_stmt(&mut self, stmt: &ContStmt) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn break_stmt(&mut self, stmt: &BreakStmt) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn ret_stmt(&mut self, stmt: &ReturnStmt) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn with_stmt(&mut self, stmt: &WithStmt) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn switch_stmt(&mut self, stmt: &SwitchStmt) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn throw_stmt(&mut self, stmt: &ThrowStmt) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn try_stmt(&mut self, stmt: &TryStmt) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn debug_stmt(&mut self, stmt: &DebugStmt) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn fn_stmt(&mut self, stmt: &FnDec) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn member_expr(&mut self, expr: &MemberExpr) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn new_expr(&mut self, expr: &NewExpr) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn call_expr(&mut self, expr: &CallExpr) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn unary_expr(&mut self, expr: &UnaryExpr) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn binary_expr(&mut self, expr: &BinaryExpr) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn assign_expr(&mut self, expr: &AssignExpr) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn cond_expr(&mut self, expr: &CondExpr) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn seq_expr(&mut self, expr: &SeqExpr) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn this_expr(&mut self, expr: &ThisExprData) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn id_expr(&mut self, expr: &IdData) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn array_literal(&mut self, expr: &ArrayData) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn object_literal(&mut self, expr: &ObjectData) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn paren_expr(&mut self, expr: &ParenData) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn fn_expr(&mut self, expr: &FnDec) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn regexp_expr(&mut self, expr: &RegExpData) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn null_expr(&mut self, expr: &NullData) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn str_expr(&mut self, expr: &StringData) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn bool_expr(&mut self, expr: &BoolData) -> Result<(), RuntimeError> {
    unimplemented!()
  }

  fn num_expr(&mut self, expr: &NumericData) -> Result<(), RuntimeError> {
    println!("{:#?}", expr);
    Ok(())
  }
}

#[cfg(test)]
mod interp_tests {
  use super::*;
  use crate::source::*;

  #[test]
  fn exec_test() {
    init_token_data();

    let code = String::from("1");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);
    let mut interp = Interpreter {};
    let node = parser.prog().ok().unwrap();
    interp.prog(&node);
  }
}
