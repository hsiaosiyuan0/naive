use crate::asm::chunk::*;
use crate::asm::symtab::*;
use crate::ast::*;
use crate::token::*;
use crate::visitor::AstVisitor;
use std::cmp::min;
use std::collections::{HashMap, HashSet};
use std::ptr::{drop_in_place, null_mut};
use std::sync::Once;

pub type FnStatePtr = *mut FnState;

pub fn as_fn_state(ptr: FnStatePtr) -> &'static mut FnState {
  unsafe { &mut (*ptr) }
}

pub const ENV_NAME: &'static str = "__ENV__";

#[derive(Debug)]
pub struct FnState {
  id: usize,
  tpl: FunTpl,
  parent: FnStatePtr,
  idx_in_parent: u32,
  local_reg_map: HashMap<String, u32>,
  subs: Vec<FnStatePtr>,
  free_reg: u32,
  res_reg: Vec<u32>,
}

impl FnState {
  pub fn new(id: usize) -> FnStatePtr {
    Box::into_raw(Box::new(FnState {
      id,
      tpl: FunTpl::new(),
      parent: null_mut(),
      idx_in_parent: 0,
      local_reg_map: HashMap::new(),
      subs: vec![],
      free_reg: 0,
      res_reg: vec![],
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

  // adds upval with name provided by parameter `n` and returns `true` if possible
  // otherwise add `ENV` as a upval and returns `false`
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

  pub fn const2idx(&self, c: &Const) -> usize {
    self.tpl.consts.iter().position(|c1| c1.eq(c)).unwrap()
  }

  pub fn append_inst(&mut self, inst: Inst) {
    self.tpl.code.push(inst);
  }

  fn push_res_reg(&mut self, r: u32) {
    self.res_reg.push(r);
  }

  fn pop_res_reg(&mut self) -> u32 {
    self.res_reg.pop().unwrap()
  }

  fn push_inst(&mut self, c: Inst) {
    self.tpl.code.push(c);
  }

  fn free_reg_to(&mut self, r: u32) {
    self.free_reg = r;
  }

  fn free_regs(&mut self, rs: &[u32]) {
    let mut mr = std::u32::MAX;
    for r in rs {
      mr = min(mr, *r);
    }
    self.free_reg_to(mr);
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

  pub fn enter_fn_state(&mut self) {
    let cfs = self.fs_ref();
    let fs = as_fn_state(FnState::new(self.scope_id_seed));
    self.scope_id_seed += 1;
    fs.parent = self.fs;
    fs.idx_in_parent = cfs.subs.len() as u32;
    cfs.subs.push(fs);
    self.fs = fs;
  }

  pub fn leave_fn_state(&mut self) {
    self.fs = as_fn_state(self.fs).parent;
  }

  fn fs_ref(&self) -> &'static mut FnState {
    as_fn_state(self.fs)
  }

  fn symtab_scope(&self) -> &'static mut Scope {
    let fs = as_fn_state(self.fs);
    as_scope(self.symtab.get_scope(fs.id))
  }

  fn has_binding(&self, n: &str) -> bool {
    self.symtab_scope().has_binding(n)
  }

  fn bindings(&self) -> &'static HashSet<String> {
    &self.symtab_scope().bindings
  }

  fn declare_bindings(&mut self) {
    let fs = self.fs_ref();

    let bindings = self.bindings();
    // in js, we cannot declare values in root scope, the values looks in root scope are
    // just the alias of the fields of the global object, so if the scope id
    // indicates that it's root scope then we should add it's bindings as upvalues
    if self.symtab_scope().id == 0 {
      bindings
        .iter()
        .for_each(|name| assert!(fs.add_upval(name.as_str())));
    } else if bindings.len() > 0 {
      let mut i = 0;
      let mut a = 0;
      let mut b = 0;
      bindings.iter().for_each(|name| {
        let reg = fs.take_reg();
        fs.def_local(name.as_str(), reg);
        if i == 0 {
          a = reg;
        } else {
          b = reg;
        }
        i += 1;
      });
      let mut inst = Inst::new();
      inst.set_op(OpCode::LOADUNDEF);
      inst.set_a(a);
      inst.set_b(b);
      fs.push_inst(inst);
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
}

impl Drop for Codegen {
  fn drop(&mut self) {
    unsafe {
      drop_in_place(self.fs);
    }
  }
}

fn assign_upval(fs: &mut FnState, from_reg: u32, to_name: &str) {
  let has_def = fs.add_upval(to_name);
  if has_def {
    let mut inst = Inst::new();
    inst.set_op(OpCode::SETUPVAL);
    inst.set_b(fs.get_upval(to_name).unwrap().idx);
    inst.set_a(from_reg);
    fs.push_inst(inst);
  } else {
    let mut inst = Inst::new();
    inst.set_op(OpCode::SETTABUP);
    inst.set_a(fs.get_upval(ENV_NAME).unwrap().idx);
    let kst = Const::String(to_name.to_string());
    fs.add_const(&kst);
    inst.set_b(kst_id_rk(fs.const2idx(&kst)));
    inst.set_c(from_reg);
    fs.push_inst(inst);
  }
}

fn pop_last_loadk_move(fs: &mut FnState) -> Option<u32> {
  let last_inst = fs.tpl.code.last().unwrap();
  let last_op = OpCode::from_u32(last_inst.op());
  if last_op == OpCode::LOADK {
    let last_inst = fs.tpl.code.pop().unwrap();
    Some(last_inst.bx())
  } else if last_op == OpCode::MOVE {
    Some(last_inst.b())
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
    self.stmts(&stmts)
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
          let tmp_reg = fs.take_reg();
          fs.push_res_reg(tmp_reg);
          self.expr(&init).ok();

          assign_upval(fs, tmp_reg, n);
          fs.free_reg_to(tmp_reg);
        }
      }
    });
    Ok(())
  }

  fn empty_stmt(&mut self, stmt: &EmptyStmt) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn expr_stmt(&mut self, stmt: &ExprStmt) -> Result<(), CodegenError> {
    self.expr(&stmt.expr).ok();
    Ok(())
  }

  fn if_stmt(&mut self, stmt: &IfStmt) -> Result<(), CodegenError> {
    Ok(())
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
      assign_upval(pfs, ra, id);
      pfs.free_reg_to(ra);
    }

    self.declare_bindings();
    self.stmt(&stmt.body).ok();

    self.leave_fn_state();
    Ok(())
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
    let fs = self.fs_ref();
    let op_s = expr.op.symbol_data().kind;

    let mut inst = Inst::new();
    let res_reg = fs.pop_res_reg();
    inst.set_a(res_reg);

    match symbol_to_opcode(op_s) {
      Some(op) => inst.set_op(*op),
      _ => unimplemented!(),
    }

    let mut tmp_regs = vec![];
    let r_lhs = fs.take_reg();
    fs.push_res_reg(r_lhs);
    self.expr(&expr.left).ok();
    if let Some(rb) = pop_last_loadk_move(fs) {
      inst.set_b(rb);
      fs.free_reg_to(r_lhs);
    } else {
      inst.set_b(r_lhs);
      tmp_regs.push(r_lhs);
    }

    let r_rhs = fs.take_reg();
    fs.push_res_reg(r_rhs);
    self.expr(&expr.right).ok();
    if let Some(rc) = pop_last_loadk_move(fs) {
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
    match &expr.left {
      Expr::Primary(pri) => {
        let lhs_name = pri.id().name.as_str();
        if fs.has_local(lhs_name) {
          let r = fs.local2reg(lhs_name);
          fs.res_reg.push(r);
          self.expr(&expr.right).ok();
        }
      }
      _ => (),
    }
    Ok(())
  }

  fn cond_expr(&mut self, expr: &CondExpr) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn seq_expr(&mut self, expr: &SeqExpr) -> Result<(), CodegenError> {
    let fs = self.fs_ref();
    let ret_reg = fs.pop_res_reg();
    let mut tmp_regs = vec![];
    let len = expr.exprs.len();
    expr.exprs.iter().enumerate().for_each(|(i, expr)| {
      if i == len - 1 {
        fs.push_res_reg(ret_reg);
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
    if fs.has_local(n) {
      let mut inst = Inst::new();
      inst.set_op(OpCode::MOVE);
      inst.set_a(fs.pop_res_reg());
      inst.set_b(fs.local2reg(n));
      fs.push_inst(inst);
    } else {
      let has_def = fs.add_upval(n);
      if has_def {
        let mut inst = Inst::new();
        inst.set_op(OpCode::GETUPVAL);
        inst.set_a(fs.pop_res_reg());
        inst.set_b(fs.get_upval(n).unwrap().idx);
        fs.push_inst(inst);
      } else {
        let kst = Const::String(n.to_string());
        fs.add_const(&kst);
        let mut inst = Inst::new();
        inst.set_op(OpCode::GETTABUP);
        inst.set_a(fs.pop_res_reg());
        inst.set_b(fs.get_upval(ENV_NAME).unwrap().idx);
        inst.set_c(kst_id_rk(fs.const2idx(&kst)));
        fs.push_inst(inst);
      }
    }
    Ok(())
  }

  fn array_literal(&mut self, expr: &ArrayData) -> Result<(), CodegenError> {
    let fs = self.fs_ref();
    let mut inst = Inst::new();
    inst.set_op(OpCode::NEWARRAY);
    let arr_reg = fs.pop_res_reg();
    inst.set_a(arr_reg);
    fs.push_inst(inst);

    if expr.value.len() > 0 {
      let mut free_reg = 0;
      let mut i = 0;
      expr.value.iter().for_each(|expr| {
        let r = fs.take_reg();
        if i == 0 {
          free_reg = r;
        }
        fs.push_res_reg(r);
        self.expr(expr).ok();
        i += 1;
      });
      let mut inst = Inst::new();
      inst.set_op(OpCode::INITARRAY);
      inst.set_a(arr_reg);
      inst.set_b(i);
      fs.push_inst(inst);
      fs.free_reg_to(free_reg);
    }

    Ok(())
  }

  fn object_literal(&mut self, expr: &ObjectData) -> Result<(), CodegenError> {
    let fs = self.fs_ref();
    let mut inst = Inst::new();
    inst.set_op(OpCode::NEWTABLE);
    let tbl_reg = fs.pop_res_reg();
    inst.set_a(tbl_reg);
    fs.push_inst(inst);

    if expr.properties.len() > 0 {
      let mut free_reg = 0;
      let mut i = 0;
      expr.properties.iter().for_each(|prop| {
        let kst = if prop.key.primary().is_id() {
          Const::String(prop.key.primary().id().name.clone())
        } else {
          Const::String(prop.key.primary().literal().str().value.clone())
        };
        fs.add_const(&kst);

        let mut inst = Inst::new();
        inst.set_op(OpCode::SETTABLE);
        inst.set_a(tbl_reg);
        inst.set_b(kst_id_rk(fs.const2idx(&kst)));

        let r = fs.take_reg();
        if i == 0 {
          free_reg = r;
        }
        fs.push_res_reg(r);
        inst.set_c(r);

        self.expr(&prop.value).ok();

        fs.push_inst(inst);
        i += 1;
      });
      fs.free_reg_to(free_reg);
    }

    Ok(())
  }

  fn paren_expr(&mut self, expr: &ParenData) -> Result<(), CodegenError> {
    self.expr(&expr.value).ok();
    Ok(())
  }

  fn fn_expr(&mut self, expr: &FnDec) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn regexp_expr(&mut self, expr: &RegExpData) -> Result<(), CodegenError> {
    unimplemented!()
  }

  fn null_expr(&mut self, expr: &NullData) -> Result<(), CodegenError> {
    let fs = self.fs_ref();
    let mut inst = Inst::new();
    inst.set_op(OpCode::LOADNUL);
    inst.set_a(fs.pop_res_reg());
    fs.push_inst(inst);
    Ok(())
  }

  fn undef_expr(&mut self, expr: &UndefData) -> Result<(), CodegenError> {
    let fs = self.fs_ref();
    let mut inst = Inst::new();
    inst.set_op(OpCode::LOADUNDEF);
    inst.set_a(fs.pop_res_reg());
    fs.push_inst(inst);
    Ok(())
  }

  fn str_expr(&mut self, expr: &StringData) -> Result<(), CodegenError> {
    let fs = self.fs_ref();
    let kst = Const::String(expr.value.clone());
    fs.add_const(&kst);
    let mut inst = Inst::new();
    inst.set_op(OpCode::LOADK);
    inst.set_a(fs.pop_res_reg());
    inst.set_bx(kst_id_rk(fs.const2idx(&kst)));
    fs.push_inst(inst);
    Ok(())
  }

  fn bool_expr(&mut self, expr: &BoolData) -> Result<(), CodegenError> {
    let fs = self.fs_ref();
    let mut inst = Inst::new();
    inst.set_op(OpCode::LOADBOO);
    inst.set_a(fs.pop_res_reg());
    inst.set_b(if expr.value { 1 } else { 0 });
    fs.push_inst(inst);
    Ok(())
  }

  fn num_expr(&mut self, expr: &NumericData) -> Result<(), CodegenError> {
    let fs = self.fs_ref();
    let kst = Const::Number(expr.value.parse().ok().unwrap());
    fs.add_const(&kst);
    let mut inst = Inst::new();
    inst.set_op(OpCode::LOADK);
    inst.set_a(fs.pop_res_reg());
    inst.set_bx(kst_id_rk(fs.const2idx(&kst)));
    fs.push_inst(inst);
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
  use crate::lexer::*;
  use crate::parser::*;
  use crate::source::*;
  use crate::token::*;

  fn parse(code: &str) -> Prog {
    init_codegen_data();

    let code = String::from(code);
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);
    parser.prog().ok().unwrap()
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
}
