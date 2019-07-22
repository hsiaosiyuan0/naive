use crate::asm::chunk::*;
use crate::asm::symtab::*;
use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::source::*;
use crate::token::*;
use crate::visitor::AstVisitor;
use linked_hash_set::LinkedHashSet;
use std::cmp::min;
use std::collections::HashMap;
use std::ptr::{drop_in_place, null_mut};
use std::sync::Once;

pub type FnStatePtr = *mut FnState;

pub fn as_fn_state(ptr: FnStatePtr) -> &'static mut FnState {
  unsafe { &mut (*ptr) }
}

pub const ENV_NAME: &'static str = "__ENV__";

#[derive(Debug)]
pub struct LoopInfo {
  start: i32,
  end: i32,
  // index of break instructions in this loop
  brk: Vec<i32>,
  // index of continue instructions in this loop
  cont: Vec<i32>,
}

#[derive(Debug)]
pub struct FnState {
  id: usize,
  tpl: FnTpl,
  parent: FnStatePtr,
  idx_in_parent: u32,
  local_reg_map: HashMap<String, u32>,
  subs: Vec<FnStatePtr>,
  free_reg: u32,
  res_reg: Vec<u32>,
  loop_stack: Vec<LoopInfo>,
}

fn fn_state_to_tpl(s: &FnState) -> FnTpl {
  let mut tpl = s.tpl.clone();
  s.subs.iter().for_each(|sp| {
    let st = fn_state_to_tpl(as_fn_state(*sp));
    tpl.fun_tpls.push(st);
  });
  tpl
}

impl FnState {
  pub fn new(id: usize) -> FnStatePtr {
    Box::into_raw(Box::new(FnState {
      id,
      tpl: FnTpl::new(),
      parent: null_mut(),
      idx_in_parent: 0,
      local_reg_map: HashMap::new(),
      subs: vec![],
      free_reg: 0,
      res_reg: vec![],
      loop_stack: vec![],
    }))
  }

  pub fn take_reg(&mut self) -> u32 {
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

  pub fn def_local(&mut self, n: &str, reg: u32) {
    self.tpl.locals.push(Local {
      name: n.to_string(),
    });
    self.local_reg_map.insert(n.to_string(), reg);
  }

  pub fn local2reg(&self, n: &str) -> u32 {
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

  pub fn get_upval(&self, n: &str) -> Option<&UpvalDesc> {
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
    let parent = self.parent;
    if parent.is_null() {
      if !self.has_upval(ENV_NAME) {
        self.tpl.upvals.push(UpvalDesc {
          name: ENV_NAME.to_string(),
          in_stack: true,
          idx: 0,
        });
      }
      return false;
    } else {
      let parent = as_fn_state(parent);
      if parent.has_local(n) {
        self.tpl.upvals.push(UpvalDesc {
          name: n.to_string(),
          in_stack: true,
          idx: parent.local2reg(n),
        });
        return true;
      } else {
        let is_up = parent.add_upval(n);
        if is_up {
          self.tpl.upvals.push(UpvalDesc {
            name: n.to_string(),
            in_stack: false,
            idx: parent.get_upval_idx(n) as u32,
          });
          return true;
        } else {
          self.add_upval(ENV_NAME);
          return false;
        }
      }
    }
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

  pub fn const2idx(&self, c: &Const) -> usize {
    self.tpl.consts.iter().position(|c1| c1.eq(c)).unwrap()
  }

  pub fn append_inst(&mut self, inst: Inst) {
    self.tpl.code.push(inst);
  }

  fn push_res_reg(&mut self, r: u32) {
    self.res_reg.push(r);
  }

  fn pop_res_reg(&mut self) -> (u32, bool) {
    match self.res_reg.pop() {
      Some(r) => (r, false),
      None => (self.take_reg(), true),
    }
  }

  fn push_inst(&mut self, c: Inst) {
    self.tpl.code.push(c);
  }

  fn code_len(&self) -> i32 {
    self.tpl.code.len() as i32
  }

  fn push_jmp(&mut self) -> i32 {
    let mut jmp = Inst::new();
    jmp.set_op(OpCode::JMP);
    jmp.set_sbx(self.code_len() + 1);
    self.push_inst(jmp);
    self.code_len() - 1
  }

  fn fin_jmp(&mut self, idx: i32) {
    let cl = self.code_len();
    let jmp = self.tpl.code.get_mut(idx as usize).unwrap();
    jmp.set_sbx(cl - jmp.sbx());
  }

  fn fin_jmp_sbx(&mut self, idx: i32, sbx: i32) {
    let jmp = self.tpl.code.get_mut(idx as usize).unwrap();
    jmp.set_sbx(sbx);
  }

  fn get_inst(&self, idx: i32) -> &Inst {
    self.tpl.code.get(idx as usize).unwrap()
  }

  fn free_reg_to(&mut self, r: u32) {
    self.free_reg = r;
  }

  fn free_regs(&mut self, rs: &[u32]) {
    if rs.len() > 0 {
      let mut mr = std::u32::MAX;
      for r in rs {
        mr = min(mr, *r);
      }
      self.free_reg_to(mr);
    }
  }

  fn get_sub(&mut self, idx: usize) -> &'static FnState {
    as_fn_state(self.subs[0])
  }

  fn enter_loop(&mut self) {
    self.loop_stack.push(LoopInfo {
      start: self.code_len(),
      end: 0,
      brk: vec![],
      cont: vec![],
    })
  }

  fn fin_brk(&mut self) {
    if self.loop_stack.last().is_none() {
      return;
    }
    let info = self.loop_stack.last().unwrap();
    let end = info.end;
    for brk_idx in &info.brk {
      let jmp = self.get_inst(*brk_idx);
      let jmp_pc = jmp.sbx();
      let ptr = jmp as *const Inst as *mut Inst;
      unsafe {
        (*ptr).set_sbx(end - jmp_pc);
      }
    }
  }

  fn fin_cont(&mut self) {
    if self.loop_stack.last().is_none() {
      return;
    }
    let info = self.loop_stack.last().unwrap();
    let start = info.start;
    for cont_idx in &info.cont {
      let jmp = self.get_inst(*cont_idx);
      let jmp_pc = jmp.sbx();
      let ptr = jmp as *const Inst as *mut Inst;
      unsafe {
        (*ptr).set_sbx(start - jmp_pc);
      }
    }
  }

  fn leave_loop(&mut self) {
    self.loop_stack.last_mut().unwrap().end = self.code_len();
    self.fin_brk();
    self.fin_cont();
    self.loop_stack.pop();
  }
}

// convert const idx to RK
pub fn kst_id_rk(id: usize) -> u32 {
  assert!(id < 256);
  (id | (1 << 8)) as u32
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

#[derive(Debug)]
pub struct Codegen {
  scope_id_seed: usize,
  fs: FnStatePtr,
  symtab: SymTab,
}

impl Codegen {
  pub fn new(symtab: SymTab) -> Self {
    let fs = FnState::new(0);
    Codegen {
      scope_id_seed: 1,
      fs,
      symtab,
    }
  }

  pub fn gen(code: &str) -> Chunk {
    init_codegen_data();

    let code = String::from(code);
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);
    let ast = parser.prog().ok().unwrap();

    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let chk = Chunk {
      sig: "naive",
      ver: 0x0001,
      upval_cnt: 0,
      fun_tpl: fn_state_to_tpl(codegen.fs_ref()),
    };

    chk
  }

  fn enter_fn_state(&mut self) {
    let cfs = self.fs_ref();
    let fs = as_fn_state(FnState::new(self.scope_id_seed));
    self.scope_id_seed += 1;
    fs.parent = self.fs;
    fs.idx_in_parent = cfs.subs.len() as u32;
    cfs.subs.push(fs);
    self.fs = fs;
  }

  fn leave_fn_state(&mut self) {
    self.fs = as_fn_state(self.fs).parent;
  }

  fn fs_ref(&self) -> &'static mut FnState {
    as_fn_state(self.fs)
  }

  fn symtab_scope(&self) -> &'static mut Scope {
    let fs = as_fn_state(self.fs);
    as_scope(self.symtab.get_scope(fs.id))
  }

  fn is_root_scope(&self) -> bool {
    self.symtab_scope().id == 0
  }

  fn has_binding(&self, n: &str) -> bool {
    self.symtab_scope().has_binding(n)
  }

  fn bindings(&self) -> &'static LinkedHashSet<String> {
    &self.symtab_scope().bindings
  }

  fn declare_bindings(&mut self) {
    let fs = self.fs_ref();

    let bindings = self.bindings();
    if bindings.len() > 0 {
      let a = fs.take_reg();
      let mut b = a;
      bindings.iter().for_each(|name| {
        fs.def_local(name.as_str(), b);
        b = fs.take_reg();
      });
      let mut inst = Inst::new();
      inst.set_op(OpCode::LOADUNDEF);
      inst.set_a(a);
      inst.set_b(b - 1);
      fs.push_inst(inst);
      fs.free_reg_to(b);
    }
  }

  fn stmts(&mut self, stmts: &Vec<Stmt>) -> Result<(), CodegenError> {
    for stmt in stmts {
      match self.stmt(stmt) {
        Err(e) => return Err(e),
        _ => (),
      }
    }
    Ok(())
  }

  fn append_ret_inst(&mut self) {
    let mut ret = Inst::new();
    ret.set_op(OpCode::RETURN);
    ret.set_a(0);
    ret.set_b(1);
    self.fs_ref().push_inst(ret);
  }
}

impl Drop for Codegen {
  fn drop(&mut self) {
    unsafe {
      drop_in_place(self.fs);
    }
  }
}

fn new_set_global_inst(fs: &mut FnState, field_name: &str, val_reg: u32) -> Inst {
  let mut inst = Inst::new();
  inst.set_op(OpCode::SETTABUP);
  inst.set_a(fs.get_upval(ENV_NAME).unwrap().idx);
  let kst = Const::String(field_name.to_string());
  fs.add_const(&kst);
  inst.set_b(kst_id_rk(fs.const2idx(&kst)));
  inst.set_c(val_reg);
  inst
}

fn assign_upval(fs: &mut FnState, from_reg: u32, to_name: &str) -> Inst {
  let has_def = fs.add_upval(to_name);
  if has_def {
    let mut inst = Inst::new();
    inst.set_op(OpCode::SETUPVAL);
    inst.set_b(fs.get_upval(to_name).unwrap().idx);
    inst.set_a(from_reg);
    inst
  } else {
    let mut inst = Inst::new();
    inst.set_op(OpCode::SETTABUP);
    inst.set_a(fs.get_upval(ENV_NAME).unwrap().idx);
    let kst = Const::String(to_name.to_string());
    fs.add_const(&kst);
    inst.set_b(kst_id_rk(fs.const2idx(&kst)));
    inst.set_c(from_reg);
    inst
  }
}

//fn load_upval(fs: &mut FnState, from_name: &str, to_reg: u32) -> Inst {
//  let has_def = fs.add_upval(from_name);
//  if has_def {
//    let mut inst = Inst::new();
//    inst.set_op(OpCode::GETUPVAL);
//    inst.set_a(to_reg);
//    inst.set_b(fs.get_upval(from_name).unwrap().idx);
//    inst
//  } else {
//    let mut inst = Inst::new();
//    inst.set_op(OpCode::GETTABUP);
//    inst.set_a(to_reg);
//    inst.set_b(fs.get_upval(ENV_NAME).unwrap().idx);
//    let kst = Const::String(from_name.to_string());
//    fs.add_const(&kst);
//    inst.set_c(kst_id_rk(fs.const2idx(&kst)));
//    inst
//  }
//}

fn pop_last_loadk(fs: &mut FnState) -> Option<u32> {
  let last_inst = fs.tpl.code.last().unwrap();
  let last_op = OpCode::from_u32(last_inst.op());
  if last_op == OpCode::LOADK {
    let last_inst = fs.tpl.code.pop().unwrap();
    Some(last_inst.bx())
  } else {
    None
  }
}

// sort statements, move function statements to the top of the others
// does not hoist variable declarations, since they are hoisted via
// call `declare_bindings` at the entry of each scope
fn hoist(stmts: &Vec<Stmt>) -> Vec<Stmt> {
  let mut not_fn = vec![];
  let mut fns = vec![];
  stmts.iter().for_each(|stmt| {
    if stmt.is_fn() {
      fns.push(stmt.clone());
    } else {
      not_fn.push(stmt.clone());
    }
  });
  fns.append(&mut not_fn);
  fns
}

impl AstVisitor<(), CodegenError> for Codegen {
  fn prog(&mut self, prog: &Prog) -> Result<(), CodegenError> {
    unsafe {
      if !IS_MOD_INITIALIZED {
        return Err(CodegenError::new(
          "`init_codegen_data` should be called before this method",
        ));
      }
    }
    // we don't use `self.declare_bindings()` here since we cannot
    // declare values in root scope, the values in root scope just
    // alias of the fields of the global object
    let stmts = hoist(&prog.body);
    match self.stmts(&stmts) {
      Err(e) => return Err(e),
      _ => (),
    }
    self.append_ret_inst();
    Ok(())
  }

  fn block_stmt(&mut self, stmt: &BlockStmt) -> Result<(), CodegenError> {
    let stmts = hoist(&stmt.body);
    self.stmts(&stmts)
  }

  fn var_dec_stmt(&mut self, stmt: &VarDec) -> Result<(), CodegenError> {
    let fs = self.fs_ref();
    stmt.decs.iter().for_each(|dec| {
      if let Some(init) = &dec.init {
        let n = dec.id.id().name.as_str();
        if fs.has_local(n) {
          fs.push_res_reg(fs.local2reg(n));
          self.expr(&init).ok();
        } else {
          let mut tmp_regs = vec![];
          let tmp_reg = fs.take_reg();
          fs.push_res_reg(tmp_reg);
          self.expr(&init).ok();
          let rk = if let Some(r) = pop_last_loadk(fs) {
            fs.free_reg_to(tmp_reg);
            r
          } else {
            tmp_regs.push(tmp_reg);
            tmp_reg
          };

          let inst = assign_upval(fs, rk, n);
          fs.push_inst(inst);
          fs.free_regs(&tmp_regs);
        }
      }
    });
    Ok(())
  }

  fn empty_stmt(&mut self, stmt: &EmptyStmt) -> Result<(), CodegenError> {
    Ok(())
  }

  fn expr_stmt(&mut self, stmt: &ExprStmt) -> Result<(), CodegenError> {
    self.expr(&stmt.expr).ok();
    Ok(())
  }

  fn if_stmt(&mut self, stmt: &IfStmt) -> Result<(), CodegenError> {
    let fs = self.fs_ref();
    let tr = fs.take_reg();
    fs.push_res_reg(tr);
    self.expr(&stmt.test).ok();

    let mut test = Inst::new();
    test.set_op(OpCode::TEST);
    test.set_a(tr);
    test.set_c(0);
    fs.push_inst(test);
    fs.free_reg_to(tr);

    // jmp to false
    let jmp1 = fs.push_jmp();
    self.stmt(&stmt.cons).ok();

    if let Some(alt) = &stmt.alt {
      // jmp over false
      let jmp2 = fs.push_jmp();
      fs.fin_jmp(jmp1);
      self.stmt(&alt).ok();
      fs.fin_jmp(jmp2);
    } else {
      fs.fin_jmp(jmp1);
    }

    Ok(())
  }

  fn for_stmt(&mut self, stmt: &ForStmt) -> Result<(), CodegenError> {
    let fs = self.fs_ref();
    fs.enter_loop();

    if let Some(init) = &stmt.init {
      match init {
        ForFirst::VarDec(dec) => self.var_dec_stmt(dec).ok(),
        ForFirst::Expr(expr) => self.expr(expr).ok(),
      };
    }

    let s_len = fs.code_len();
    fs.loop_stack.last_mut().unwrap().start = s_len;

    let tr = fs.take_reg();
    if let Some(test) = &stmt.test {
      fs.push_res_reg(tr);
      self.expr(test).ok();
    } else {
      let mut b = Inst::new();
      b.set_op(OpCode::LOADBOO);
      b.set_a(tr);
      b.set_b(1);
      fs.push_inst(b);
    }

    let mut t = Inst::new();
    t.set_op(OpCode::TEST);
    t.set_a(tr);
    t.set_c(0);
    fs.push_inst(t);
    fs.free_reg_to(tr);

    // jmp out of loop
    let jmp1 = fs.push_jmp();

    if let Some(update) = &stmt.update {
      self.expr(update).ok();
    }

    self.stmt(&stmt.body).ok();

    // jmp to the loop start
    let jmp2 = fs.push_jmp();
    fs.fin_jmp(jmp1);

    let e_len = fs.code_len();
    fs.fin_jmp_sbx(jmp2, s_len - e_len);
    fs.leave_loop();
    Ok(())
  }

  fn for_in_stmt(&mut self, stmt: &ForInStmt) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn do_while_stmt(&mut self, stmt: &DoWhileStmt) -> Result<(), CodegenError> {
    let fs = self.fs_ref();
    fs.enter_loop();

    let s_len = fs.code_len();
    self.stmt(&stmt.body).ok();

    let tr = fs.take_reg();
    fs.push_res_reg(tr);
    self.expr(&stmt.test).ok();
    fs.free_reg_to(tr);

    let mut t = Inst::new();
    t.set_op(OpCode::TEST);
    t.set_a(tr);
    t.set_c(0);
    fs.push_inst(t);

    // jmp out of loop
    let jmp1 = fs.push_jmp();
    let e_len = fs.code_len();
    fs.fin_jmp_sbx(jmp1, s_len - e_len);
    fs.leave_loop();
    Ok(())
  }

  fn while_stmt(&mut self, stmt: &WhileStmt) -> Result<(), CodegenError> {
    let fs = self.fs_ref();
    fs.enter_loop();

    let s_len = fs.code_len();

    let tr = fs.take_reg();
    fs.push_res_reg(tr);
    self.expr(&stmt.test).ok();
    fs.free_reg_to(tr);

    let mut t = Inst::new();
    t.set_op(OpCode::TEST);
    t.set_a(tr);
    t.set_c(0);
    fs.push_inst(t);

    // jmp out of loop
    let jmp1 = fs.push_jmp();
    self.stmt(&stmt.body).ok();

    // jmp to the loop start
    let jmp2 = fs.push_jmp();
    fs.fin_jmp(jmp1);

    let e_len = fs.code_len();
    fs.fin_jmp_sbx(jmp2, s_len - e_len);
    fs.leave_loop();
    Ok(())
  }

  fn cont_stmt(&mut self, stmt: &ContStmt) -> Result<(), CodegenError> {
    let fs = self.fs_ref();
    let len = fs.code_len();
    let mut inst = Inst::new();
    inst.set_op(OpCode::JMP);
    inst.set_sbx(len + 1);
    fs.push_inst(inst);
    fs.loop_stack.last_mut().unwrap().cont.push(len);
    Ok(())
  }

  fn break_stmt(&mut self, stmt: &BreakStmt) -> Result<(), CodegenError> {
    let fs = self.fs_ref();
    let len = fs.code_len();
    let mut inst = Inst::new();
    inst.set_op(OpCode::JMP);
    inst.set_sbx(len + 1);
    fs.push_inst(inst);
    fs.loop_stack.last_mut().unwrap().brk.push(len);
    Ok(())
  }

  fn ret_stmt(&mut self, stmt: &ReturnStmt) -> Result<(), CodegenError> {
    let fs = self.fs_ref();

    let mut ret = Inst::new();
    ret.set_op(OpCode::RETURN);

    if let Some(arg) = &stmt.argument {
      let tr = fs.take_reg();
      fs.push_res_reg(tr);
      self.expr(arg).ok();
      ret.set_a(tr);
      ret.set_b(2);
      fs.free_reg_to(tr);
    }

    fs.push_inst(ret);
    Ok(())
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
    // unlike function expr, function with none id is meaningless since it cannot be called later
    // so in this situation we can skip to generate it's instructions
    if stmt.id.is_none() {
      return Ok(());
    }

    let id = stmt.id.as_ref().unwrap().id().name.as_str();
    let pfs = self.fs_ref();

    self.enter_fn_state();
    let cfs = self.fs_ref();
    let ra = if pfs.has_local(id) {
      pfs.local2reg(id)
    } else {
      pfs.take_reg()
    };

    let mut inst = Inst::new();
    inst.set_op(OpCode::CLOSURE);
    inst.set_a(ra);
    inst.set_bx(cfs.idx_in_parent);
    pfs.push_inst(inst);

    if !pfs.has_local(id) {
      let inst = assign_upval(pfs, ra, id);
      pfs.push_inst(inst);
      pfs.free_reg_to(ra);
    }

    self.declare_bindings();
    self.stmt(&stmt.body).ok();
    self.append_ret_inst();

    self.leave_fn_state();
    Ok(())
  }

  fn member_expr(&mut self, expr: &MemberExpr) -> Result<(), CodegenError> {
    let fs = self.fs_ref();
    let mut tmp_regs = vec![];
    let (res_reg, is_tmp) = fs.pop_res_reg();
    if is_tmp {
      tmp_regs.push(res_reg)
    }

    let mut inst = Inst::new();
    let tr = fs.take_reg();
    fs.push_res_reg(tr);
    self.expr(&expr.object).ok();
    tmp_regs.push(tr);

    inst.set_op(OpCode::GETTABLE);
    inst.set_a(res_reg);
    inst.set_b(tr);

    if expr.computed {
      let tr = fs.take_reg();
      fs.push_res_reg(tr);
      self.expr(&expr.property).ok();
      tmp_regs.push(tr);
      inst.set_c(tr);
    } else {
      let n = expr.property.primary().id().name.as_str();
      let kst = Const::new_str(n);
      fs.add_const(&kst);
      inst.set_c(kst_id_rk(fs.const2idx(&kst)));
    }

    fs.push_inst(inst);
    fs.free_regs(&tmp_regs);
    Ok(())
  }

  fn new_expr(&mut self, expr: &NewExpr) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn call_expr(&mut self, expr: &CallExpr) -> Result<(), CodegenError> {
    // As opposed to Lua, JS does not support multiple return values and does not have VARARG(until
    // ES5, in ES6 Rest Parameters equals VARARG in Lua).
    // However this does not mean the B consist in CALL cannot be 0 in ES5, since we should keep
    // this ability to implement Function.prototype.apply

    let fs = self.fs_ref();
    let mut tmp_regs = vec![];
    let (res_reg, is_tmp) = fs.pop_res_reg();
    if is_tmp {
      tmp_regs.push(res_reg)
    }

    let a = fs.take_reg();
    fs.push_res_reg(a);
    self.expr(&expr.callee).ok();
    tmp_regs.push(a);

    expr.arguments.iter().for_each(|arg| {
      let r = fs.take_reg();
      fs.push_res_reg(r);
      self.expr(arg).ok();
      tmp_regs.push(r);
    });

    let mut call = Inst::new();
    call.set_op(OpCode::CALL);
    call.set_a(a);
    call.set_b((expr.arguments.len() + 1) as u32);

    let ret_num = if is_tmp { 1 } else { 2 };
    call.set_c(ret_num);
    fs.push_inst(call);

    if !is_tmp && a != res_reg {
      let mut mov = Inst::new();
      mov.set_op(OpCode::MOVE);
      mov.set_a(res_reg);
      mov.set_b(a);
      fs.push_inst(mov);
    }
    fs.free_regs(&tmp_regs);
    Ok(())
  }

  fn unary_expr(&mut self, expr: &UnaryExpr) -> Result<(), CodegenError> {
    let fs = self.fs_ref();

    let mut tmp_regs = vec![];
    let (res_reg, is_tmp) = fs.pop_res_reg();
    if is_tmp {
      tmp_regs.push(res_reg)
    }

    let sym = expr.op.symbol_data().kind;
    match sym {
      Symbol::Sub | Symbol::Not | Symbol::BitNot => {
        let op = match sym {
          Symbol::Sub => OpCode::UNM,
          Symbol::Not => OpCode::NOT,
          Symbol::BitNot => OpCode::BITNOT,
          _ => panic!(),
        };

        let mut inst = Inst::new();
        inst.set_op(op);
        inst.set_a(res_reg);

        let tr = fs.take_reg();
        fs.push_res_reg(tr);
        self.expr(&expr.argument).ok();
        if let Some(r) = pop_last_loadk(fs) {
          inst.set_b(r);
        } else {
          inst.set_b(tr);
        }
        fs.push_inst(inst);
        fs.free_regs(&tmp_regs);
      }
      Symbol::Inc | Symbol::Dec => {
        let op_tok = if sym == Symbol::Inc {
          Token::Symbol(SymbolData {
            kind: Symbol::AssignAdd,
            loc: SourceLoc::new(),
          })
        } else {
          Token::Symbol(SymbolData {
            kind: Symbol::AssignSub,
            loc: SourceLoc::new(),
          })
        };

        if !expr.prefix {
          // if expr is postfix we should compute the value of
          // argument and move the compute result to the result register
          let m_b = fs.take_reg();
          fs.push_res_reg(m_b);
          self.expr(&expr.argument).ok();
          tmp_regs.push(m_b);

          if !is_tmp {
            let mut mov = Inst::new();
            mov.set_op(OpCode::MOVE);
            mov.set_a(res_reg);
            mov.set_b(m_b);
            fs.push_inst(mov);
          }
        }

        let assign: Expr = AssignExpr {
          loc: SourceLoc::new(),
          op: op_tok,
          left: expr.argument.clone(),
          right: PrimaryExpr::Literal(Literal::Numeric(NumericData::new(
            SourceLoc::new(),
            "1".to_owned(),
          )))
          .into(),
        }
        .into();

        // if expr is prefix, we should push result register into res_reg_stack
        // to let the anonymous assign to put its result in it
        if expr.prefix {
          fs.push_res_reg(res_reg);
        }
        self.expr(&assign).ok();
        fs.free_regs(&tmp_regs);
      }
      _ => panic!(),
    }
    Ok(())
  }

  fn binary_expr(&mut self, expr: &BinaryExpr) -> Result<(), CodegenError> {
    let fs = self.fs_ref();
    let op_s = expr.op.symbol_data().kind;

    let mut inst = Inst::new();

    let mut tmp_regs = vec![];
    let (res_reg, is_tmp) = fs.pop_res_reg();
    if is_tmp {
      tmp_regs.push(res_reg)
    }

    inst.set_a(res_reg);
    match symbol_to_opcode(op_s) {
      Some(op) => inst.set_op(*op),
      _ => unimplemented!(),
    }

    let r_lhs = fs.take_reg();
    fs.push_res_reg(r_lhs);
    self.expr(&expr.left).ok();
    if let Some(rb) = pop_last_loadk(fs) {
      inst.set_b(rb);
      fs.free_reg_to(r_lhs);
    } else {
      inst.set_b(r_lhs);
      tmp_regs.push(r_lhs);
    }

    let r_rhs = fs.take_reg();
    fs.push_res_reg(r_rhs);
    self.expr(&expr.right).ok();
    if let Some(rc) = pop_last_loadk(fs) {
      inst.set_c(rc);
      fs.free_reg_to(r_rhs);
    } else {
      inst.set_c(r_rhs);
      tmp_regs.push(r_rhs);
    }

    fs.push_inst(inst);
    fs.free_regs(&tmp_regs);

    match op_s {
      Symbol::LT
      | Symbol::LE
      | Symbol::Eq
      | Symbol::EqStrict
      | Symbol::GT
      | Symbol::GE
      | Symbol::NotEq
      | Symbol::NotEqStrict => {
        // People who are familiar with the CLua implementation may notice that here we don't use
        // JMP, the reason of why CLua uses JMP with EQ|LT|LE is that it treats the comparisons
        // which their results are required as using them in the test part of the `if-else` statement,
        // eg: it treats all `var a = b > c` as
        // `var a = if b > c then return true else return false`
        // We use JMP with EQ|LT|LE in `if-else` statement to eliminate a duplicated TEST, otherwise
        // we should firstly use a temp register to save the comparison result and then use TEST to
        // test that result to perform the conditional jump
        // However this way(treat all comparisons as using them in `if-else`) cause a redundant JMP is
        // produced when we just use them like `var a = b > c`, in this case the instructions can be
        // generated like below, there is no JUMP and just three instructions are used.
        // For the situation whenever JUMP is required will be properly handled in the outer `if-else`
        // routine
        fs.tpl.code.last_mut().unwrap().set_a(1);

        let load_true_first = match op_s {
          Symbol::LT | Symbol::LE | Symbol::Eq | Symbol::EqStrict => true,
          _ => false,
        };

        let mut lb1 = Inst::new();
        lb1.set_op(OpCode::LOADBOO);
        lb1.set_a(res_reg);
        lb1.set_b(if load_true_first { 1 } else { 0 });
        lb1.set_c(1);
        fs.push_inst(lb1);

        let mut lb2 = Inst::new();
        lb2.set_op(OpCode::LOADBOO);
        lb2.set_a(res_reg);
        lb2.set_b(if load_true_first { 0 } else { 1 });
        fs.push_inst(lb2);
      }
      Symbol::And | Symbol::Or => {
        let test_set = fs.tpl.code.last_mut().unwrap();
        let lhs_reg = test_set.b();
        let rhs_reg = test_set.c();
        test_set.set_c(if op_s == Symbol::And { 1 } else { 0 });

        let mut jmp = Inst::new();
        jmp.set_op(OpCode::JMP);
        jmp.set_sbx(1);
        fs.push_inst(jmp);

        let mut mov = Inst::new();
        mov.set_op(OpCode::MOVE);
        mov.set_a(res_reg);
        mov.set_b(rhs_reg);
        fs.push_inst(mov);
      }
      _ => (),
    }

    Ok(())
  }

  fn assign_expr(&mut self, expr: &AssignExpr) -> Result<(), CodegenError> {
    let fs = self.fs_ref();

    let mut tmp_regs = vec![];
    let (res_reg, is_res_tmp) = fs.pop_res_reg();
    if is_res_tmp {
      tmp_regs.push(res_reg)
    }

    let mut rc = fs.take_reg();
    fs.push_res_reg(rc);
    self.expr(&expr.right).ok();
    if let Some(r) = pop_last_loadk(fs) {
      fs.free_reg_to(rc);
      rc = r;
    } else {
      tmp_regs.push(rc);
    }

    let mut inst = Inst::new();
    match &expr.left {
      Expr::Primary(pri) => {
        if !pri.is_id() {
          return Err(CodegenError::new("lhs must be LVal"));
        }
        let id = pri.id().name.as_str();
        if fs.has_local(id) {
          inst.set_op(OpCode::MOVE);
          inst.set_a(fs.local2reg(id));
        } else {
          inst = assign_upval(fs, 0, id);
        }
      }
      Expr::Member(m) => {
        inst.set_op(OpCode::SETTABLE);
        let tr = fs.take_reg();
        fs.push_res_reg(tr);
        self.expr(&m.object).ok();
        inst.set_a(tr);
        tmp_regs.push(tr);

        if m.computed {
          let rb = fs.take_reg();
          fs.push_res_reg(rb);
          self.expr(&m.property).ok();
          tmp_regs.push(rb);
          inst.set_b(rb);
        } else {
          let n = m.property.primary().id().name.as_str();
          let kst = Const::new_str(n);
          fs.add_const(&kst);
          inst.set_b(kst_id_rk(fs.const2idx(&kst)));
        }
      }
      _ => return Err(CodegenError::new("lhs must be LVal")),
    }

    // is `operator=`
    if !expr.op.is_symbol_kind(Symbol::Assign) {
      let rb = fs.take_reg();
      fs.push_res_reg(rb);
      self.expr(&expr.left).ok();
      tmp_regs.push(rb);

      let ra = fs.take_reg();
      let mut binop = Inst::new();
      binop.set_op(*symbol_to_opcode(expr.op.symbol_data().kind).unwrap());
      binop.set_a(ra);
      binop.set_b(rb);
      binop.set_c(rc);
      fs.push_inst(binop);
      tmp_regs.push(ra);
      rc = ra;
    }

    let op = OpCode::from_u32(inst.op());
    match op {
      OpCode::MOVE => inst.set_b(rc),
      OpCode::SETUPVAL => inst.set_a(rc),
      OpCode::SETTABLE | OpCode::SETTABUP => inst.set_c(rc),
      _ => panic!(),
    }
    fs.push_inst(inst);

    if !is_res_tmp {
      let mut mov = Inst::new();
      mov.set_op(OpCode::MOVE);
      mov.set_a(res_reg);
      mov.set_b(rc);
      fs.push_inst(mov);
    }
    fs.free_regs(&tmp_regs);
    Ok(())
  }

  fn cond_expr(&mut self, expr: &CondExpr) -> Result<(), CodegenError> {
    let fs = self.fs_ref();

    let mut tmp_regs = vec![];
    let (res_reg, is_tmp) = fs.pop_res_reg();
    if is_tmp {
      tmp_regs.push(res_reg)
    }

    let tr = fs.take_reg();
    fs.push_res_reg(tr);
    self.expr(&expr.test).ok();
    tmp_regs.push(tr);

    let mut test = Inst::new();
    test.set_op(OpCode::TESTSET);
    test.set_a(tr);
    test.set_c(0);
    fs.push_inst(test);

    let jmp1 = fs.push_jmp();
    fs.push_res_reg(res_reg);
    self.expr(&expr.cons).ok();

    let jmp2 = fs.push_jmp();
    fs.fin_jmp(jmp1);

    fs.push_res_reg(res_reg);
    self.expr(&expr.alt).ok();
    fs.fin_jmp(jmp2);

    fs.free_regs(&tmp_regs);
    Ok(())
  }

  fn seq_expr(&mut self, expr: &SeqExpr) -> Result<(), CodegenError> {
    let fs = self.fs_ref();

    let mut tmp_regs = vec![];
    let (res_reg, is_tmp) = fs.pop_res_reg();
    if is_tmp {
      tmp_regs.push(res_reg)
    }

    let len = expr.exprs.len();
    expr.exprs.iter().enumerate().for_each(|(i, expr)| {
      if i == len - 1 {
        fs.push_res_reg(res_reg);
      } else {
        let tmp_reg = fs.take_reg();
        fs.push_res_reg(tmp_reg);
        tmp_regs.push(tmp_reg);
      }
      self.expr(expr).ok();
    });
    fs.free_regs(&tmp_regs);
    Ok(())
  }

  fn this_expr(&mut self, expr: &ThisExprData) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn id_expr(&mut self, expr: &IdData) -> Result<(), CodegenError> {
    let fs = self.fs_ref();
    let n = expr.name.as_str();

    let mut tmp_regs = vec![];
    let (res_reg, is_tmp) = fs.pop_res_reg();
    if is_tmp {
      tmp_regs.push(res_reg)
    }

    if fs.has_local(n) {
      let mut inst = Inst::new();
      inst.set_op(OpCode::MOVE);
      inst.set_a(res_reg);
      inst.set_b(fs.local2reg(n));
      fs.push_inst(inst);
    } else {
      let has_def = fs.add_upval(n);
      if has_def {
        let mut inst = Inst::new();
        inst.set_op(OpCode::GETUPVAL);
        inst.set_a(res_reg);
        inst.set_b(fs.get_upval_idx(n) as u32);
        fs.push_inst(inst);
      } else {
        let kst = Const::String(n.to_string());
        fs.add_const(&kst);
        let mut inst = Inst::new();
        inst.set_op(OpCode::GETTABUP);
        inst.set_a(res_reg);
        inst.set_b(fs.get_upval(ENV_NAME).unwrap().idx);
        inst.set_c(kst_id_rk(fs.const2idx(&kst)));
        fs.push_inst(inst);
      }
    }

    fs.free_regs(&tmp_regs);
    Ok(())
  }

  fn array_literal(&mut self, expr: &ArrayData) -> Result<(), CodegenError> {
    let fs = self.fs_ref();
    let mut inst = Inst::new();
    inst.set_op(OpCode::NEWARRAY);

    let mut tmp_regs = vec![];
    let (res_reg, is_tmp) = fs.pop_res_reg();
    if is_tmp {
      tmp_regs.push(res_reg)
    }

    inst.set_a(res_reg);
    fs.push_inst(inst);

    if expr.value.len() > 0 {
      let mut i = 0;
      expr.value.iter().for_each(|expr| {
        let r = fs.take_reg();
        tmp_regs.push(r);
        fs.push_res_reg(r);
        self.expr(expr).ok();
        i += 1;
      });
      let mut inst = Inst::new();
      inst.set_op(OpCode::INITARRAY);
      inst.set_a(res_reg);
      inst.set_b(i);
      fs.push_inst(inst);
    }

    fs.free_regs(&tmp_regs);
    Ok(())
  }

  fn object_literal(&mut self, expr: &ObjectData) -> Result<(), CodegenError> {
    let fs = self.fs_ref();
    let mut inst = Inst::new();
    inst.set_op(OpCode::NEWTABLE);

    let mut tmp_regs = vec![];
    let (res_reg, is_tmp) = fs.pop_res_reg();
    if is_tmp {
      tmp_regs.push(res_reg)
    }

    inst.set_a(res_reg);
    fs.push_inst(inst);

    if expr.properties.len() > 0 {
      expr.properties.iter().for_each(|prop| {
        let kst = if prop.key.primary().is_id() {
          Const::String(prop.key.primary().id().name.clone())
        } else {
          Const::String(prop.key.primary().literal().str().value.clone())
        };
        fs.add_const(&kst);

        let mut inst = Inst::new();
        inst.set_op(OpCode::SETTABLE);
        inst.set_a(res_reg);
        inst.set_b(kst_id_rk(fs.const2idx(&kst)));

        let r = fs.take_reg();
        tmp_regs.push(r);
        fs.push_res_reg(r);
        inst.set_c(r);

        self.expr(&prop.value).ok();
        fs.push_inst(inst);
      });
    }

    fs.free_regs(&tmp_regs);
    Ok(())
  }

  fn paren_expr(&mut self, expr: &ParenData) -> Result<(), CodegenError> {
    self.expr(&expr.value).ok();
    Ok(())
  }

  fn fn_expr(&mut self, expr: &FnDec) -> Result<(), CodegenError> {
    let pfs = self.fs_ref();
    let (res_reg, is_tmp) = pfs.pop_res_reg();
    assert!(!is_tmp);

    self.enter_fn_state();
    let cfs = self.fs_ref();

    let mut inst = Inst::new();
    inst.set_op(OpCode::CLOSURE);
    inst.set_a(res_reg);
    inst.set_bx(cfs.idx_in_parent);
    pfs.push_inst(inst);

    self.declare_bindings();
    self.stmt(&expr.body).ok();
    self.append_ret_inst();

    self.leave_fn_state();
    Ok(())
  }

  fn regexp_expr(&mut self, expr: &RegExpData) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn null_expr(&mut self, expr: &NullData) -> Result<(), CodegenError> {
    let fs = self.fs_ref();

    let mut tmp_regs = vec![];
    let (res_reg, is_tmp) = fs.pop_res_reg();
    if is_tmp {
      tmp_regs.push(res_reg)
    }

    let mut inst = Inst::new();
    inst.set_op(OpCode::LOADNUL);
    inst.set_a(res_reg);
    fs.push_inst(inst);
    fs.free_regs(&tmp_regs);
    Ok(())
  }

  fn undef_expr(&mut self, expr: &UndefData) -> Result<(), CodegenError> {
    let fs = self.fs_ref();

    let mut tmp_regs = vec![];
    let (res_reg, is_tmp) = fs.pop_res_reg();
    if is_tmp {
      tmp_regs.push(res_reg)
    }

    let mut inst = Inst::new();
    inst.set_op(OpCode::LOADUNDEF);
    inst.set_a(res_reg);
    fs.push_inst(inst);
    fs.free_regs(&tmp_regs);
    Ok(())
  }

  fn str_expr(&mut self, expr: &StringData) -> Result<(), CodegenError> {
    let fs = self.fs_ref();

    let mut tmp_regs = vec![];
    let (res_reg, is_tmp) = fs.pop_res_reg();
    if is_tmp {
      tmp_regs.push(res_reg)
    }

    let kst = Const::String(expr.value.clone());
    fs.add_const(&kst);
    let mut inst = Inst::new();
    inst.set_op(OpCode::LOADK);
    inst.set_a(res_reg);
    inst.set_bx(kst_id_rk(fs.const2idx(&kst)));
    fs.push_inst(inst);
    fs.free_regs(&tmp_regs);
    Ok(())
  }

  fn bool_expr(&mut self, expr: &BoolData) -> Result<(), CodegenError> {
    let fs = self.fs_ref();

    let mut tmp_regs = vec![];
    let (res_reg, is_tmp) = fs.pop_res_reg();
    if is_tmp {
      tmp_regs.push(res_reg)
    }

    let mut inst = Inst::new();
    inst.set_op(OpCode::LOADBOO);
    inst.set_a(res_reg);
    inst.set_b(if expr.value { 1 } else { 0 });
    fs.push_inst(inst);
    fs.free_regs(&tmp_regs);
    Ok(())
  }

  fn num_expr(&mut self, expr: &NumericData) -> Result<(), CodegenError> {
    let fs = self.fs_ref();

    let mut tmp_regs = vec![];
    let (res_reg, is_tmp) = fs.pop_res_reg();
    if is_tmp {
      tmp_regs.push(res_reg)
    }

    let kst = Const::Number(expr.value.parse().ok().unwrap());
    fs.add_const(&kst);
    let mut inst = Inst::new();
    inst.set_op(OpCode::LOADK);
    inst.set_a(res_reg);
    inst.set_bx(kst_id_rk(fs.const2idx(&kst)));
    fs.push_inst(inst);
    fs.free_regs(&tmp_regs);
    Ok(())
  }
}

static mut SYMBOL_OPCODE_MAP: Option<HashMap<Symbol, OpCode>> = None;

fn symbol_to_opcode(s: Symbol) -> Option<&'static OpCode> {
  unsafe { SYMBOL_OPCODE_MAP.as_ref().unwrap().get(&s) }
}

fn init_symbol_opcode_map() {
  let mut map = HashMap::new();
  map.insert(Symbol::Add, OpCode::ADD);
  map.insert(Symbol::Sub, OpCode::SUB);
  map.insert(Symbol::Mul, OpCode::MUL);
  map.insert(Symbol::Mod, OpCode::MOD);
  map.insert(Symbol::Div, OpCode::DIV);
  map.insert(Symbol::LT, OpCode::LT);
  map.insert(Symbol::GT, OpCode::LT);
  map.insert(Symbol::LE, OpCode::LE);
  map.insert(Symbol::GE, OpCode::LE);
  map.insert(Symbol::Eq, OpCode::EQ);
  map.insert(Symbol::NotEq, OpCode::EQ);
  map.insert(Symbol::EqStrict, OpCode::EQS);
  map.insert(Symbol::NotEqStrict, OpCode::EQS);
  map.insert(Symbol::BitAnd, OpCode::BITAND);
  map.insert(Symbol::BitOr, OpCode::BITOR);
  map.insert(Symbol::BitNot, OpCode::BITNOT);
  map.insert(Symbol::SHL, OpCode::SHL);
  map.insert(Symbol::SAR, OpCode::SAR);
  map.insert(Symbol::SHR, OpCode::SHR);
  map.insert(Symbol::And, OpCode::TESTSET);
  map.insert(Symbol::Or, OpCode::TESTSET);
  map.insert(Symbol::AssignAdd, OpCode::ADD);
  map.insert(Symbol::AssignSub, OpCode::SUB);
  map.insert(Symbol::AssignMul, OpCode::MUL);
  map.insert(Symbol::AssignDiv, OpCode::DIV);
  map.insert(Symbol::AssignMod, OpCode::MOD);
  map.insert(Symbol::AssignSHL, OpCode::SHL);
  map.insert(Symbol::AssignSAR, OpCode::SAR);
  map.insert(Symbol::AssignSHR, OpCode::SHR);
  map.insert(Symbol::AssignBitAnd, OpCode::BITAND);
  map.insert(Symbol::AssignBitOr, OpCode::BITOR);
  map.insert(Symbol::AssignBitXor, OpCode::BITXOR);
  unsafe {
    SYMBOL_OPCODE_MAP = Some(map);
  }
}

static mut IS_MOD_INITIALIZED: bool = false;

static INIT_CODEGEN_DATA_ONCE: Once = Once::new();
pub fn init_codegen_data() {
  INIT_CODEGEN_DATA_ONCE.call_once(|| {
    init_token_data();
    init_opcode_data();
    init_symbol_opcode_map();
    unsafe {
      IS_MOD_INITIALIZED = true;
    }
  });
}

#[cfg(test)]
mod codegen_tests {
  use super::*;

  fn parse(code: &str) -> Prog {
    init_codegen_data();

    let code = String::from(code);
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);
    let ast = parser.prog().ok().unwrap();
    ast
  }

  fn assert_code_eq(asert: &str, test: &Vec<Inst>) {
    let mut t = String::new();
    test.iter().for_each(|inst| {
      t.push_str("    ");
      t.push_str(format!("{:#?}", inst).as_str());
      t.push_str(",\n");
    });
    assert_eq!(asert.trim(), t.trim());
  }

  #[test]
  fn gen_test() {
    let ast = parse("var a; if(1) var b else var c");
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();
  }

  #[test]
  fn kst_test() {
    let ast = parse(
      "
    var a = 'hello world', 
        b = 1, 
        c = true,
        e = null
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();
  }

  #[test]
  fn id_test() {
    let ast = parse(
      "
    var a = 'hello world'
    var b = a
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();
  }

  #[test]
  fn arr_literal_test() {
    let ast = parse(
      "
    var a = [1, 2, 3, [4, 5]]
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();
  }

  #[test]
  fn tbl_literal_test() {
    let ast = parse(
      "
        var a = {
          b: 1,
          c: [ {d: 2}, , undefined, null ],
          e: { 'f': 3 }
        }
        ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();
  }

  #[test]
  fn binop_kst() {
    let ast = parse(
      "
        var a = 1
        var b = a
        var c = b + 2
        ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "
    SETTABUP{ A: 0, B: 257, C: 256 },
    GETTABUP{ A: 0, B: 0, C: 257 },
    SETTABUP{ A: 0, B: 258, C: 0 },
    GETTABUP{ A: 1, B: 0, C: 258 },
    ADD{ A: 0, B: 1, C: 259 },
    SETTABUP{ A: 0, B: 260, C: 0 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);
  }

  #[test]
  fn binop_test() {
    let ast = parse(
      "
        var a,b;
        var c = a + 2 * b / 3
        ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();
  }

  #[test]
  fn seq_test() {
    let ast = parse(
      "
        var a,b;
        var c = (1,2,a+b,b)
        ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();
  }

  #[test]
  fn closure_test() {
    let ast = parse(
      "
    function a() {
      var a = b;
      function b() {}
    }
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();
  }

  #[test]
  fn relation_test() {
    let ast = parse(
      "
    var c = a == b
    var d = c
    var e = d != f
    var f = a < b
    var f = a > b
    var f = a >> 1
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();
  }

  #[test]
  fn and_or_test() {
    let ast = parse(
      "
    var c = a && b
    var d = e || f
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "
    GETTABUP{ A: 1, B: 0, C: 256 },
    GETTABUP{ A: 2, B: 0, C: 257 },
    TESTSET{ A: 0, B: 1, C: 1 },
    JMP{ A: 0, sBx: 1 },
    MOVE{ A: 0, B: 2, C: 0 },
    SETTABUP{ A: 0, B: 258, C: 0 },
    GETTABUP{ A: 1, B: 0, C: 259 },
    GETTABUP{ A: 2, B: 0, C: 260 },
    TESTSET{ A: 0, B: 1, C: 0 },
    JMP{ A: 0, sBx: 1 },
    MOVE{ A: 0, B: 2, C: 0 },
    SETTABUP{ A: 0, B: 261, C: 0 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);
  }

  #[test]
  fn assign_test() {
    let ast = parse(
      "
    c = a + b
    e.f = 1 + 2
    a = b = 1
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "
    GETTABUP{ A: 2, B: 0, C: 256 },
    GETTABUP{ A: 3, B: 0, C: 257 },
    ADD{ A: 1, B: 2, C: 3 },
    SETTABUP{ A: 0, B: 258, C: 1 },
    ADD{ A: 1, B: 259, C: 260 },
    GETTABUP{ A: 2, B: 0, C: 261 },
    SETTABLE{ A: 2, B: 262, C: 1 },
    SETTABUP{ A: 0, B: 257, C: 259 },
    MOVE{ A: 1, B: 259, C: 0 },
    SETTABUP{ A: 0, B: 256, C: 1 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);
  }

  #[test]
  fn binop_assign_test() {
    let ast = parse(
      "
    a.b += c + 2
    e += 1
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "            
    GETTABUP{ A: 2, B: 0, C: 256 },
    ADD{ A: 1, B: 2, C: 257 },
    GETTABUP{ A: 2, B: 0, C: 258 },
    GETTABUP{ A: 4, B: 0, C: 258 },
    GETTABLE{ A: 3, B: 4, C: 259 },
    ADD{ A: 4, B: 3, C: 1 },
    SETTABLE{ A: 2, B: 259, C: 4 },
    GETTABUP{ A: 1, B: 0, C: 261 },
    ADD{ A: 2, B: 1, C: 260 },
    SETTABUP{ A: 0, B: 261, C: 2 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);
  }

  #[test]
  fn unary_test() {
    let ast = parse(
      "
    a = a & ~b
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "
    GETTABUP{ A: 2, B: 0, C: 256 },
    GETTABUP{ A: 4, B: 0, C: 257 },
    BITNOT{ A: 3, B: 4, C: 0 },
    BITAND{ A: 1, B: 2, C: 3 },
    SETTABUP{ A: 0, B: 256, C: 1 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);
  }

  #[test]
  fn postfix_expr_test() {
    let ast = parse(
      "
    a += a++ + ++a
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "
    GETTABUP{ A: 3, B: 0, C: 256 },
    MOVE{ A: 2, B: 3, C: 0 },
    GETTABUP{ A: 5, B: 0, C: 256 },
    ADD{ A: 6, B: 5, C: 257 },
    SETTABUP{ A: 0, B: 256, C: 6 },
    GETTABUP{ A: 4, B: 0, C: 256 },
    ADD{ A: 5, B: 4, C: 257 },
    SETTABUP{ A: 0, B: 256, C: 5 },
    MOVE{ A: 3, B: 5, C: 0 },
    ADD{ A: 1, B: 2, C: 3 },
    GETTABUP{ A: 2, B: 0, C: 256 },
    ADD{ A: 3, B: 2, C: 1 },
    SETTABUP{ A: 0, B: 256, C: 3 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);
  }

  #[test]
  fn cond_expr_test() {
    let ast = parse(
      "
    a = a ? a +=1 : a +=2
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "
    GETTABUP{ A: 2, B: 0, C: 256 },
    TESTSET{ A: 2, B: 0, C: 0 },
    JMP{ A: 0, sBx: 5 },
    GETTABUP{ A: 3, B: 0, C: 256 },
    ADD{ A: 4, B: 3, C: 257 },
    SETTABUP{ A: 0, B: 256, C: 4 },
    MOVE{ A: 1, B: 4, C: 0 },
    JMP{ A: 0, sBx: 4 },
    GETTABUP{ A: 3, B: 0, C: 256 },
    ADD{ A: 4, B: 3, C: 258 },
    SETTABUP{ A: 0, B: 256, C: 4 },
    MOVE{ A: 1, B: 4, C: 0 },
    SETTABUP{ A: 0, B: 256, C: 1 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);
  }

  #[test]
  fn fn_expr_test() {
    let ast = parse(
      "
    a = function f() {
      var a = function () {}
    }
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "
    CLOSURE{ A: 1, B: 0, C: 0 },
    SETTABUP{ A: 0, B: 256, C: 1 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);

    let insts = "
    LOADUNDEF{ A: 0, B: 0, C: 0 },
    CLOSURE{ A: 0, B: 0, C: 0 },";
    assert_code_eq(insts, &codegen.fs_ref().get_sub(0).tpl.code);
  }

  #[test]
  fn global_var_test() {
    let ast = parse(
      "
    var a = 1
    function f() {
      return a
    }
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "
    CLOSURE{ A: 0, B: 0, C: 0 },
    SETTABUP{ A: 0, B: 256, C: 0 },
    SETTABUP{ A: 0, B: 258, C: 257 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);

    let insts = "
    GETTABUP{ A: 0, B: 0, C: 256 },
    RETURN{ A: 0, B: 2, C: 0 },";
    assert_code_eq(insts, &as_fn_state(codegen.fs_ref().subs[0]).tpl.code);
  }

  #[test]
  fn if_stmt_test() {
    let ast = parse(
      "
    if (a) {
      a
      b
    }
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "
    GETTABUP{ A: 0, B: 0, C: 256 },
    TEST{ A: 0, B: 0, C: 0 },
    JMP{ A: 0, sBx: 2 },
    GETTABUP{ A: 0, B: 0, C: 256 },
    GETTABUP{ A: 0, B: 0, C: 257 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);
  }

  #[test]
  fn if_stmt_test2() {
    let ast = parse(
      "
    if (a) {
      a
      b
    } else {
      c
      d
    }
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "
    GETTABUP{ A: 0, B: 0, C: 256 },
    TEST{ A: 0, B: 0, C: 0 },
    JMP{ A: 0, sBx: 3 },
    GETTABUP{ A: 0, B: 0, C: 256 },
    GETTABUP{ A: 0, B: 0, C: 257 },
    JMP{ A: 0, sBx: 2 },
    GETTABUP{ A: 0, B: 0, C: 258 },
    GETTABUP{ A: 0, B: 0, C: 259 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);
  }

  #[test]
  fn if_stmt_test3() {
    let ast = parse(
      "
    if (a) {
      a
    } else if(b) {
      c
    } else { d }
    f
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "
    GETTABUP{ A: 0, B: 0, C: 256 },
    TEST{ A: 0, B: 0, C: 0 },
    JMP{ A: 0, sBx: 2 },
    GETTABUP{ A: 0, B: 0, C: 256 },
    JMP{ A: 0, sBx: 6 },
    GETTABUP{ A: 0, B: 0, C: 257 },
    TEST{ A: 0, B: 0, C: 0 },
    JMP{ A: 0, sBx: 2 },
    GETTABUP{ A: 0, B: 0, C: 258 },
    JMP{ A: 0, sBx: 1 },
    GETTABUP{ A: 0, B: 0, C: 259 },
    GETTABUP{ A: 0, B: 0, C: 260 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);
  }

  #[test]
  fn for_stmt_test() {
    let ast = parse(
      "
    for(i = 0; i + a < 10; i++) {
      b
      c
    }
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "
    SETTABUP{ A: 0, B: 257, C: 256 },
    GETTABUP{ A: 2, B: 0, C: 257 },
    GETTABUP{ A: 3, B: 0, C: 258 },
    ADD{ A: 1, B: 2, C: 3 },
    LT{ A: 1, B: 1, C: 259 },
    LOADBOO{ A: 0, B: 1, C: 1 },
    LOADBOO{ A: 0, B: 0, C: 0 },
    TEST{ A: 0, B: 0, C: 0 },
    JMP{ A: 0, sBx: 7 },
    GETTABUP{ A: 1, B: 0, C: 257 },
    GETTABUP{ A: 3, B: 0, C: 257 },
    ADD{ A: 4, B: 3, C: 260 },
    SETTABUP{ A: 0, B: 257, C: 4 },
    GETTABUP{ A: 0, B: 0, C: 261 },
    GETTABUP{ A: 0, B: 0, C: 262 },
    JMP{ A: 0, sBx: -15 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);
  }

  #[test]
  fn for_stmt_test1() {
    let ast = parse(
      "
    for(i = 0;; i++) {} a
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "
    SETTABUP{ A: 0, B: 257, C: 256 },
    LOADBOO{ A: 0, B: 1, C: 0 },
    TEST{ A: 0, B: 0, C: 0 },
    JMP{ A: 0, sBx: 5 },
    GETTABUP{ A: 1, B: 0, C: 257 },
    GETTABUP{ A: 3, B: 0, C: 257 },
    ADD{ A: 4, B: 3, C: 258 },
    SETTABUP{ A: 0, B: 257, C: 4 },
    JMP{ A: 0, sBx: -8 },
    GETTABUP{ A: 0, B: 0, C: 259 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);
  }

  #[test]
  fn for_stmt_test2() {
    let ast = parse(
      "
    for(;;) {} a
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "
    LOADBOO{ A: 0, B: 1, C: 0 },
    TEST{ A: 0, B: 0, C: 0 },
    JMP{ A: 0, sBx: 1 },
    JMP{ A: 0, sBx: -4 },
    GETTABUP{ A: 0, B: 0, C: 256 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);
  }

  #[test]
  fn while_stmt_test() {
    let ast = parse(
      "
    while(a > 1) { a } a
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "
    GETTABUP{ A: 1, B: 0, C: 256 },
    LT{ A: 1, B: 1, C: 257 },
    LOADBOO{ A: 0, B: 0, C: 1 },
    LOADBOO{ A: 0, B: 1, C: 0 },
    TEST{ A: 0, B: 0, C: 0 },
    JMP{ A: 0, sBx: 2 },
    GETTABUP{ A: 0, B: 0, C: 256 },
    JMP{ A: 0, sBx: -8 },
    GETTABUP{ A: 0, B: 0, C: 256 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);
  }

  #[test]
  fn do_while_stmt_test() {
    let ast = parse(
      "
    do { a-- } while(a > 0) a
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "
    GETTABUP{ A: 1, B: 0, C: 256 },
    GETTABUP{ A: 3, B: 0, C: 256 },
    SUB{ A: 4, B: 3, C: 257 },
    SETTABUP{ A: 0, B: 256, C: 4 },
    GETTABUP{ A: 1, B: 0, C: 256 },
    LT{ A: 1, B: 1, C: 258 },
    LOADBOO{ A: 0, B: 0, C: 1 },
    LOADBOO{ A: 0, B: 1, C: 0 },
    TEST{ A: 0, B: 0, C: 0 },
    JMP{ A: 0, sBx: -10 },
    GETTABUP{ A: 0, B: 0, C: 256 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);
  }

  #[test]
  fn break_stmt_test() {
    let ast = parse(
      "
    for(;;) {
      for(;;) {
        if(a) { break }
        1
      }
      2
      if(b) break
      3
    }
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "
    LOADBOO{ A: 0, B: 1, C: 0 },
    TEST{ A: 0, B: 0, C: 0 },
    JMP{ A: 0, sBx: 16 },
    LOADBOO{ A: 0, B: 1, C: 0 },
    TEST{ A: 0, B: 0, C: 0 },
    JMP{ A: 0, sBx: 6 },
    GETTABUP{ A: 0, B: 0, C: 256 },
    TEST{ A: 0, B: 0, C: 0 },
    JMP{ A: 0, sBx: 1 },
    JMP{ A: 0, sBx: 2 },
    LOADK{ A: 0, Bx: 257 },
    JMP{ A: 0, sBx: -9 },
    LOADK{ A: 0, Bx: 258 },
    GETTABUP{ A: 0, B: 0, C: 259 },
    TEST{ A: 0, B: 0, C: 0 },
    JMP{ A: 0, sBx: 1 },
    JMP{ A: 0, sBx: 2 },
    LOADK{ A: 0, Bx: 260 },
    JMP{ A: 0, sBx: -19 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);
  }

  #[test]
  fn cont_stmt_test() {
    let ast = parse(
      "
    for(b;;) {
      for(;;) {
        if(a) { continue }
        else break
      }
      1
      if(b) continue
      else break
    }
    2
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "
    GETTABUP{ A: 0, B: 0, C: 256 },
    LOADBOO{ A: 0, B: 1, C: 0 },
    TEST{ A: 0, B: 0, C: 0 },
    JMP{ A: 0, sBx: 18 },
    LOADBOO{ A: 0, B: 1, C: 0 },
    TEST{ A: 0, B: 0, C: 0 },
    JMP{ A: 0, sBx: 7 },
    GETTABUP{ A: 0, B: 0, C: 257 },
    TEST{ A: 0, B: 0, C: 0 },
    JMP{ A: 0, sBx: 2 },
    JMP{ A: 0, sBx: -7 },
    JMP{ A: 0, sBx: 1 },
    JMP{ A: 0, sBx: 1 },
    JMP{ A: 0, sBx: -10 },
    LOADK{ A: 0, Bx: 258 },
    GETTABUP{ A: 0, B: 0, C: 256 },
    TEST{ A: 0, B: 0, C: 0 },
    JMP{ A: 0, sBx: 2 },
    JMP{ A: 0, sBx: -18 },
    JMP{ A: 0, sBx: 1 },
    JMP{ A: 0, sBx: 1 },
    JMP{ A: 0, sBx: -21 },
    LOADK{ A: 0, Bx: 259 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);
  }

  #[test]
  fn call_expr_test() {
    let ast = parse(
      "
    print(a,b)
    print(add(a,b),1)
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "
    GETTABUP{ A: 1, B: 0, C: 256 },
    GETTABUP{ A: 2, B: 0, C: 257 },
    GETTABUP{ A: 3, B: 0, C: 258 },
    CALL{ A: 1, B: 3, C: 1 },
    GETTABUP{ A: 1, B: 0, C: 256 },
    GETTABUP{ A: 2, B: 0, C: 259 },
    GETTABUP{ A: 3, B: 0, C: 257 },
    GETTABUP{ A: 4, B: 0, C: 258 },
    CALL{ A: 2, B: 3, C: 2 },
    LOADK{ A: 3, Bx: 260 },
    CALL{ A: 1, B: 3, C: 1 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);
  }

  #[test]
  fn return_stmt_test() {
    let ast = parse(
      "
    return add(a,b)
    return
    ",
    );
    let mut symtab = SymTab::new();
    symtab.prog(&ast).unwrap();

    let mut codegen = Codegen::new(symtab);
    codegen.prog(&ast).ok();

    let insts = "
    GETTABUP{ A: 1, B: 0, C: 256 },
    GETTABUP{ A: 2, B: 0, C: 257 },
    GETTABUP{ A: 3, B: 0, C: 258 },
    CALL{ A: 1, B: 3, C: 2 },
    MOVE{ A: 0, B: 1, C: 0 },
    RETURN{ A: 0, B: 2, C: 0 },
    RETURN{ A: 0, B: 0, C: 0 },";
    assert_code_eq(insts, &codegen.fs_ref().tpl.code);
  }
}
