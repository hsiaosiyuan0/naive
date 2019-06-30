use crate::ast::*;

pub trait AstVisitor<T, E> {
  fn prog(&mut self, prog: &Prog) -> Result<T, E>;

  fn stmt(&mut self, stmt: &Stmt) -> Result<T, E> {
    match stmt {
      Stmt::Block(s) => self.block_stmt(s),
      Stmt::VarDec(s) => self.var_dec_stmt(s),
      Stmt::Empty(s) => self.empty_stmt(s),
      Stmt::Expr(s) => self.expr_stmt(s),
      Stmt::If(s) => self.if_stmt(s),
      Stmt::For(s) => self.for_stmt(s),
      Stmt::ForIn(s) => self.for_in_stmt(s),
      Stmt::DoWhile(s) => self.do_while_stmt(s),
      Stmt::While(s) => self.while_stmt(s),
      Stmt::Cont(s) => self.cont_stmt(s),
      Stmt::Break(s) => self.break_stmt(s),
      Stmt::Return(s) => self.ret_stmt(s),
      Stmt::With(s) => self.with_stmt(s),
      Stmt::Switch(s) => self.switch_stmt(s),
      Stmt::Throw(s) => self.throw_stmt(s),
      Stmt::Try(s) => self.try_stmt(s),
      Stmt::Debugger(s) => self.debug_stmt(s),
      Stmt::Function(s) => self.fn_stmt(s),
    }
  }

  fn block_stmt(&mut self, stmt: &BlockStmt) -> Result<T, E>;
  fn var_dec_stmt(&mut self, stmt: &VarDec) -> Result<T, E>;
  fn empty_stmt(&mut self, stmt: &EmptyStmt) -> Result<T, E>;
  fn expr_stmt(&mut self, stmt: &ExprStmt) -> Result<T, E>;
  fn if_stmt(&mut self, stmt: &IfStmt) -> Result<T, E>;
  fn for_stmt(&mut self, stmt: &ForStmt) -> Result<T, E>;
  fn for_in_stmt(&mut self, stmt: &ForInStmt) -> Result<T, E>;
  fn do_while_stmt(&mut self, stmt: &DoWhileStmt) -> Result<T, E>;
  fn while_stmt(&mut self, stmt: &WhileStmt) -> Result<T, E>;
  fn cont_stmt(&mut self, stmt: &ContStmt) -> Result<T, E>;
  fn break_stmt(&mut self, stmt: &BreakStmt) -> Result<T, E>;
  fn ret_stmt(&mut self, stmt: &ReturnStmt) -> Result<T, E>;
  fn with_stmt(&mut self, stmt: &WithStmt) -> Result<T, E>;
  fn switch_stmt(&mut self, stmt: &SwitchStmt) -> Result<T, E>;
  fn throw_stmt(&mut self, stmt: &ThrowStmt) -> Result<T, E>;
  fn try_stmt(&mut self, stmt: &TryStmt) -> Result<T, E>;
  fn debug_stmt(&mut self, stmt: &DebugStmt) -> Result<T, E>;
  fn fn_stmt(&mut self, stmt: &FnDec) -> Result<T, E>;

  fn member_expr(&mut self, expr: &MemberExpr) -> Result<T, E>;
  fn new_expr(&mut self, expr: &NewExpr) -> Result<T, E>;
  fn call_expr(&mut self, expr: &CallExpr) -> Result<T, E>;
  fn unary_expr(&mut self, expr: &UnaryExpr) -> Result<T, E>;
  fn binary_expr(&mut self, expr: &BinaryExpr) -> Result<T, E>;
  fn assign_expr(&mut self, expr: &AssignExpr) -> Result<T, E>;
  fn cond_expr(&mut self, expr: &CondExpr) -> Result<T, E>;
  fn seq_expr(&mut self, expr: &SeqExpr) -> Result<T, E>;
  fn primary_expr(&mut self, expr: &PrimaryExpr) -> Result<T, E> {
    match expr {
      PrimaryExpr::This(ex) => self.this_expr(ex),
      PrimaryExpr::Identifier(ex) => self.id_expr(ex),
      PrimaryExpr::Literal(ex) => self.literal(ex),
      PrimaryExpr::ArrayLiteral(ex) => self.array_literal(ex),
      PrimaryExpr::ObjectLiteral(ex) => self.object_literal(ex),
      PrimaryExpr::Parenthesized(ex) => self.paren_expr(ex),
      PrimaryExpr::Function(ex) => self.fn_expr(ex),
    }
  }

  fn this_expr(&mut self, expr: &ThisExprData) -> Result<T, E>;
  fn id_expr(&mut self, expr: &IdData) -> Result<T, E>;
  fn array_literal(&mut self, expr: &ArrayData) -> Result<T, E>;
  fn object_literal(&mut self, expr: &ObjectData) -> Result<T, E>;
  fn paren_expr(&mut self, expr: &ParenData) -> Result<T, E>;
  fn fn_expr(&mut self, expr: &FnDec) -> Result<T, E>;
  fn literal(&mut self, expr: &Literal) -> Result<T, E> {
    match expr {
      Literal::RegExp(ex) => self.regexp_expr(ex),
      Literal::Null(ex) => self.null_expr(ex),
      Literal::String(ex) => self.str_expr(ex),
      Literal::Bool(ex) => self.bool_expr(ex),
      Literal::Numeric(ex) => self.num_expr(ex),
    }
  }

  fn regexp_expr(&mut self, expr: &RegExpData) -> Result<T, E>;
  fn null_expr(&mut self, expr: &NullData) -> Result<T, E>;
  fn str_expr(&mut self, expr: &StringData) -> Result<T, E>;
  fn bool_expr(&mut self, expr: &BoolData) -> Result<T, E>;
  fn num_expr(&mut self, expr: &NumericData) -> Result<T, E>;
}
