use crate::ast::*;
use crate::visitor::AstVisitor;
use std::collections::{HashMap, HashSet};
use std::ptr::{drop_in_place, null_mut};

type ScopePtr = *mut Scope;

pub fn as_scope(ptr: ScopePtr) -> &'static mut Scope {
  unsafe { &mut (*ptr) }
}

#[derive(Debug)]
pub struct Scope {
  id: usize,
  parent: ScopePtr,
  subs: Vec<ScopePtr>,
  bindings: HashSet<String>,
}

impl Scope {
  fn new(id: usize) -> ScopePtr {
    Box::into_raw(Box::new(Scope {
      id,
      parent: null_mut(),
      subs: vec![],
      bindings: HashSet::new(),
    }))
  }

  pub fn add_binding(&mut self, n: &str) {
    self.bindings.insert(n.to_string());
  }

  pub fn has_binding(&self, n: &str) -> bool {
    self.bindings.contains(n)
  }
}

#[derive(Debug)]
pub struct SymTab {
  i: usize,
  scopes: HashMap<usize, ScopePtr>,
  s: ScopePtr,
}

impl SymTab {
  pub fn new() -> SymTab {
    let s = Scope::new(0);
    let mut scopes = HashMap::new();
    scopes.insert(as_scope(s).id, s);
    SymTab { i: 1, scopes, s }
  }

  pub fn enter_scope(&mut self) {
    let s = Scope::new(self.i);
    self.scopes.insert(self.i, s);
    self.i += 1;
    as_scope(s).parent = self.s;
    as_scope(self.s).subs.push(s);
    self.s = s;
  }

  pub fn leave_scope(&mut self) {
    self.s = as_scope(self.s).parent;
  }

  fn add_binding(&mut self, n: &str) {
    as_scope(self.s).add_binding(n)
  }
}

impl Drop for SymTab {
  fn drop(&mut self) {
    self
      .scopes
      .values()
      .for_each(|s| unsafe { drop_in_place(*s) })
  }
}

impl AstVisitor<(), ()> for SymTab {
  fn prog(&mut self, prog: &Prog) -> Result<(), ()> {
    prog.body.iter().for_each(|s| self.stmt(s).unwrap());
    Ok(())
  }

  fn block_stmt(&mut self, stmt: &BlockStmt) -> Result<(), ()> {
    stmt.body.iter().for_each(|s| self.stmt(s).unwrap());
    Ok(())
  }

  fn var_dec_stmt(&mut self, stmt: &VarDec) -> Result<(), ()> {
    stmt
      .decs
      .iter()
      .for_each(|dec| self.add_binding(dec.id.id().name.as_str()));
    Ok(())
  }

  fn empty_stmt(&mut self, stmt: &EmptyStmt) -> Result<(), ()> {
    Ok(())
  }

  fn expr_stmt(&mut self, stmt: &ExprStmt) -> Result<(), ()> {
    Ok(())
  }

  fn if_stmt(&mut self, stmt: &IfStmt) -> Result<(), ()> {
    self.stmt(&stmt.cons);
    if let Some(s) = &stmt.alt {
      self.stmt(s);
    }
    Ok(())
  }

  fn for_stmt(&mut self, stmt: &ForStmt) -> Result<(), ()> {
    if let Some(init) = &stmt.init {
      match init {
        ForFirst::VarDec(dec) => self.var_dec_stmt(dec).unwrap(),
        _ => (),
      }
    }
    self.stmt(&stmt.body);
    Ok(())
  }

  fn for_in_stmt(&mut self, stmt: &ForInStmt) -> Result<(), ()> {
    match &stmt.left {
      ForFirst::VarDec(dec) => self.var_dec_stmt(dec).unwrap(),
      _ => (),
    }
    self.stmt(&stmt.body);
    Ok(())
  }

  fn do_while_stmt(&mut self, stmt: &DoWhileStmt) -> Result<(), ()> {
    self.stmt(&stmt.body);
    Ok(())
  }

  fn while_stmt(&mut self, stmt: &WhileStmt) -> Result<(), ()> {
    self.stmt(&stmt.body);
    Ok(())
  }

  fn cont_stmt(&mut self, stmt: &ContStmt) -> Result<(), ()> {
    Ok(())
  }

  fn break_stmt(&mut self, stmt: &BreakStmt) -> Result<(), ()> {
    Ok(())
  }

  fn ret_stmt(&mut self, stmt: &ReturnStmt) -> Result<(), ()> {
    if let Some(s) = &stmt.argument {
      self.expr(s);
    }
    Ok(())
  }

  fn with_stmt(&mut self, stmt: &WithStmt) -> Result<(), ()> {
    Ok(())
  }

  fn switch_stmt(&mut self, stmt: &SwitchStmt) -> Result<(), ()> {
    stmt
      .cases
      .iter()
      .for_each(|case| case.cons.iter().for_each(|s| self.stmt(s).unwrap()));
    Ok(())
  }

  fn throw_stmt(&mut self, stmt: &ThrowStmt) -> Result<(), ()> {
    Ok(())
  }

  fn try_stmt(&mut self, stmt: &TryStmt) -> Result<(), ()> {
    self.stmt(&stmt.block);
    if let Some(h) = &stmt.handler {
      self.stmt(&h.body);
    }
    if let Some(f) = &stmt.finalizer {
      self.stmt(&f);
    }
    Ok(())
  }

  fn debug_stmt(&mut self, stmt: &DebugStmt) -> Result<(), ()> {
    Ok(())
  }

  fn fn_stmt(&mut self, stmt: &FnDec) -> Result<(), ()> {
    if let Some(id) = &stmt.id {
      self.add_binding(id.id().name.as_str());
    }
    self.enter_scope();
    stmt
      .params
      .iter()
      .for_each(|p| self.add_binding(p.id().name.as_str()));
    self.stmt(&stmt.body);
    self.leave_scope();
    Ok(())
  }

  fn member_expr(&mut self, expr: &MemberExpr) -> Result<(), ()> {
    self.expr(&expr.object);
    self.expr(&expr.property);
    Ok(())
  }

  fn new_expr(&mut self, expr: &NewExpr) -> Result<(), ()> {
    self.expr(&expr.callee);
    expr
      .arguments
      .iter()
      .for_each(|arg| self.expr(arg).unwrap());
    Ok(())
  }

  fn call_expr(&mut self, expr: &CallExpr) -> Result<(), ()> {
    self.expr(&expr.callee);
    expr
      .arguments
      .iter()
      .for_each(|arg| self.expr(arg).unwrap());
    Ok(())
  }

  fn unary_expr(&mut self, expr: &UnaryExpr) -> Result<(), ()> {
    self.expr(&expr.argument);
    Ok(())
  }

  fn binary_expr(&mut self, expr: &BinaryExpr) -> Result<(), ()> {
    self.expr(&expr.left);
    self.expr(&expr.right);
    Ok(())
  }

  fn assign_expr(&mut self, expr: &AssignExpr) -> Result<(), ()> {
    self.expr(&expr.left);
    self.expr(&expr.right);
    Ok(())
  }

  fn cond_expr(&mut self, expr: &CondExpr) -> Result<(), ()> {
    self.expr(&expr.test);
    self.expr(&expr.cons);
    self.expr(&expr.alt);
    Ok(())
  }

  fn seq_expr(&mut self, expr: &SeqExpr) -> Result<(), ()> {
    expr.exprs.iter().for_each(|expr| self.expr(expr).unwrap());
    Ok(())
  }

  fn this_expr(&mut self, expr: &ThisExprData) -> Result<(), ()> {
    Ok(())
  }

  fn id_expr(&mut self, expr: &IdData) -> Result<(), ()> {
    Ok(())
  }

  fn array_literal(&mut self, expr: &ArrayData) -> Result<(), ()> {
    expr.value.iter().for_each(|expr| self.expr(expr).unwrap());
    Ok(())
  }

  fn object_literal(&mut self, expr: &ObjectData) -> Result<(), ()> {
    expr.properties.iter().for_each(|p| {
      self.expr(&p.key).unwrap();
      self.expr(&p.value).unwrap();
    });
    Ok(())
  }

  fn paren_expr(&mut self, expr: &ParenData) -> Result<(), ()> {
    self.expr(&expr.value);
    Ok(())
  }

  fn fn_expr(&mut self, expr: &FnDec) -> Result<(), ()> {
    if let Some(id) = &expr.id {
      self.add_binding(id.id().name.as_str());
    }
    self.enter_scope();
    expr
      .params
      .iter()
      .for_each(|p| self.add_binding(p.id().name.as_str()));
    self.stmt(&expr.body);
    self.leave_scope();
    Ok(())
  }

  fn regexp_expr(&mut self, expr: &RegExpData) -> Result<(), ()> {
    Ok(())
  }

  fn null_expr(&mut self, expr: &NullData) -> Result<(), ()> {
    Ok(())
  }

  fn str_expr(&mut self, expr: &StringData) -> Result<(), ()> {
    Ok(())
  }

  fn bool_expr(&mut self, expr: &BoolData) -> Result<(), ()> {
    Ok(())
  }

  fn num_expr(&mut self, expr: &NumericData) -> Result<(), ()> {
    Ok(())
  }
}

#[cfg(test)]
mod symtab_tests {
  use super::*;
  use crate::lexer::*;
  use crate::parser::*;
  use crate::source::*;
  use crate::token::*;

  #[test]
  fn scope() {
    init_token_data();

    let code = String::from("var a; function f(b) {var a; return function(c) {var d}}");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);
    let mut symtab = SymTab::new();
    let ast = parser.prog().ok().unwrap();
    symtab.prog(&ast).unwrap();

    println!("{:#?}", symtab);

    let mut i = 0;
    let s0 = as_scope(*symtab.scopes.get(&i).unwrap());
    assert!(s0.has_binding("a"));

    i = 1;
    let s1 = as_scope(*symtab.scopes.get(&i).unwrap());
    assert!(s1.has_binding("a"));
    assert!(s1.has_binding("b"));

    i = 2;
    let s1 = as_scope(*symtab.scopes.get(&i).unwrap());
    assert!(s1.has_binding("c"));
    assert!(s1.has_binding("d"));
  }
}
