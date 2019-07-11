use crate::asm::chunk::*;
use crate::ast::*;
use crate::visitor::AstVisitor;
use std::collections::HashMap;
use std::mem::forget;
use std::ptr::{drop_in_place, null_mut};

pub type FnStatePtr = *mut FnState;

pub fn as_fn_state(ptr: FnStatePtr) -> &'static mut FnState {
  unsafe { &mut (*ptr) }
}

pub const ENV_NAME: &'static str = "__ENV__";

pub struct FnState {
  tpl: FunTpl,
  parent: FnStatePtr,
  idx_in_parent: usize,
  local_reg_map: HashMap<String, u64>,
  subs: Vec<FnStatePtr>,
  free_reg: usize,
}

impl FnState {
  pub fn new() -> FnStatePtr {
    Box::into_raw(Box::new(FnState {
      tpl: FunTpl::new(),
      parent: null_mut(),
      idx_in_parent: 0,
      local_reg_map: HashMap::new(),
      subs: vec![],
      free_reg: 0,
    }))
  }

  pub fn take_reg(&mut self) -> usize {
    let r = self.free_reg;
    self.free_reg += 1;
    r
  }

  pub fn has_local(&self, n: &str) -> bool {
    for v in &self.tpl.locals {
      if v.name.eq(n) {
        return true;
      }
    }
    false
  }

  pub fn def_local(&mut self, n: &str, reg: u64) {
    self.tpl.locals.push(Local {
      name: n.to_string(),
    });
    self.local_reg_map.insert(n.to_string(), reg);
  }

  pub fn local2reg(&self, n: &str) -> u64 {
    *self.local_reg_map.get(n).unwrap()
  }

  pub fn has_upval(&self, n: &str) -> bool {
    for uv in &self.tpl.upvals {
      if uv.name.eq(n) {
        return true;
      }
    }
    false
  }

  pub fn get_upval(&self, n: &str) -> Option<&Upval> {
    for uv in &self.tpl.upvals {
      if uv.name.eq(n) {
        return Some(uv);
      }
    }
    None
  }

  pub fn get_upval_idx(&self, n: &str) -> usize {
    self.tpl.upvals.iter().position(|uv| uv.name.eq(n)).unwrap()
  }

  pub fn add_upval(&mut self, n: &str) -> bool {
    if self.has_upval(n) {
      return true;
    }
    let mut spans: Vec<FnStatePtr> = vec![self as FnStatePtr];
    let mut parent = self.parent;
    let mut uv: Option<Upval> = None;
    while !parent.is_null() {
      let pfs = as_fn_state(parent);
      if pfs.has_local(n) {
        let immediate = spans.pop().unwrap();
        let immediate = as_fn_state(immediate);
        let mut v = Upval {
          name: n.to_string(),
          in_stack: true,
          idx: pfs.local2reg(n),
        };
        immediate.tpl.upvals.push(v.clone());
        v.in_stack = false;
        uv = Some(v);
        break;
      } else if pfs.has_upval(n) {
        let mut v = pfs.get_upval(n).unwrap().clone();
        v.in_stack = false;
        uv = Some(v);
        break;
      }
      parent = pfs.parent;
    }
    if uv.is_none() {
      if let Some(last) = spans.pop() {
        let fs = as_fn_state(last);
        if fs.parent.is_null() && !fs.has_upval(ENV_NAME) {
          fs.tpl.upvals.push(Upval {
            name: ENV_NAME.to_string(),
            in_stack: true,
            idx: 0,
          });
        }
      }
      spans.iter().for_each(|fs| {
        let fs = as_fn_state(*fs);
        fs.tpl.upvals.push(Upval {
          name: ENV_NAME.to_string(),
          in_stack: false,
          idx: 0,
        });
      });
      return false;
    }
    let uv = uv.unwrap();
    for span in spans {
      let fs = as_fn_state(span);
      fs.tpl.upvals.push(uv.clone());
    }
    true
  }

  pub fn has_const(&self, c: &Const) -> bool {
    for c1 in &self.tpl.consts {
      if c1.eq(c) {
        return true;
      }
    }
    false
  }

  pub fn add_const(&mut self, c: &Const) {
    if self.has_const(c) {
      return;
    }
    self.tpl.consts.push(c.clone());
  }

  pub fn const2idx(&self, c: &Const) -> u64 {
    self.tpl.consts.iter().position(|c1| c1.eq(c)).unwrap() as u64
  }

  pub fn append_inst(&mut self, inst: Inst) {
    self.tpl.code.push(inst);
  }
}

// convert const idx to RK
pub fn kst_id_rk(id: usize) -> u16 {
  assert!(id < 256);
  (id | (1 << 8)) as u16
}

impl Drop for FnState {
  fn drop(&mut self) {
    for sub in &self.subs {
      unsafe {
        drop_in_place(*sub);
      }
    }
  }
}

pub struct CodegenError {
  msg: String,
}

impl CodegenError {
  fn new(msg: &str) -> Self {
    CodegenError {
      msg: msg.to_string(),
    }
  }
}

pub struct Codegen {
  fs: FnStatePtr,
}

impl Codegen {
  pub fn new() -> Self {
    let fs = FnState::new();
    Codegen { fs }
  }

  pub fn enter_fn_state(&mut self) {
    let fs = FnState::new();
    as_fn_state(fs).parent = self.fs;
    as_fn_state(self.fs).subs.push(fs);
    self.fs = fs;
  }

  pub fn leave_fn_state(&mut self) {
    self.fs = as_fn_state(self.fs).parent;
  }
}

impl Drop for Codegen {
  fn drop(&mut self) {
    unsafe {
      drop_in_place(self.fs);
    }
  }
}

impl AstVisitor<(), CodegenError> for Codegen {
  fn prog(&mut self, prog: &Prog) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn block_stmt(&mut self, stmt: &BlockStmt) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn var_dec_stmt(&mut self, stmt: &VarDec) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn empty_stmt(&mut self, stmt: &EmptyStmt) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn expr_stmt(&mut self, stmt: &ExprStmt) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn if_stmt(&mut self, stmt: &IfStmt) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn for_stmt(&mut self, stmt: &ForStmt) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn for_in_stmt(&mut self, stmt: &ForInStmt) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn do_while_stmt(&mut self, stmt: &DoWhileStmt) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn while_stmt(&mut self, stmt: &WhileStmt) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn cont_stmt(&mut self, stmt: &ContStmt) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn break_stmt(&mut self, stmt: &BreakStmt) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn ret_stmt(&mut self, stmt: &ReturnStmt) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn with_stmt(&mut self, stmt: &WithStmt) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn switch_stmt(&mut self, stmt: &SwitchStmt) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn throw_stmt(&mut self, stmt: &ThrowStmt) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn try_stmt(&mut self, stmt: &TryStmt) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn debug_stmt(&mut self, stmt: &DebugStmt) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn fn_stmt(&mut self, stmt: &FnDec) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn member_expr(&mut self, expr: &MemberExpr) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn new_expr(&mut self, expr: &NewExpr) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn call_expr(&mut self, expr: &CallExpr) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn unary_expr(&mut self, expr: &UnaryExpr) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn binary_expr(&mut self, expr: &BinaryExpr) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn assign_expr(&mut self, expr: &AssignExpr) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn cond_expr(&mut self, expr: &CondExpr) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn seq_expr(&mut self, expr: &SeqExpr) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn this_expr(&mut self, expr: &ThisExprData) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn id_expr(&mut self, expr: &IdData) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn array_literal(&mut self, expr: &ArrayData) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn object_literal(&mut self, expr: &ObjectData) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn paren_expr(&mut self, expr: &ParenData) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn fn_expr(&mut self, expr: &FnDec) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn regexp_expr(&mut self, expr: &RegExpData) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn null_expr(&mut self, expr: &NullData) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn str_expr(&mut self, expr: &StringData) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn bool_expr(&mut self, expr: &BoolData) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn num_expr(&mut self, expr: &NumericData) -> Result<(), CodegenError> {
    unimplemented!()
  }
}

#[cfg(test)]
mod codegen_tests {
  use super::*;

  #[test]
  fn test_drop() {
    let mut cg = Codegen::new();
    cg.enter_fn_state();
    cg.enter_fn_state();
    cg.leave_fn_state();
    cg.leave_fn_state();
  }
}
