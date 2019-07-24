use crate::asm::chunk::*;
use crate::vm::gc::*;
use crate::vm::obj::*;
use std::collections::{HashMap, HashSet};
use std::mem;
use std::os::raw::c_void;
use std::ptr::{drop_in_place, null_mut};

#[derive(Debug)]
pub struct RuntimeError {
  pub msg: String,
}

impl RuntimeError {
  fn new(msg: &str) -> Self {
    RuntimeError {
      msg: msg.to_owned(),
    }
  }
}

pub type CallInfoPtr = *mut CallInfo;

#[inline(always)]
pub fn as_ci<T>(ptr: *mut T) -> &'static mut CallInfo {
  unsafe { &mut (*(ptr as CallInfoPtr)) }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct CallInfo {
  prev: CallInfoPtr,
  next: CallInfoPtr,
  pc: u32,
  fun: u32,
  base: u32,
  is_native: bool,
  open_upvals: HashMap<u32, UpValPtr>,
  is_new: bool,
  this: JsObjPtr,
}

impl CallInfo {
  pub fn new() -> CallInfoPtr {
    Box::into_raw(Box::new(CallInfo {
      prev: null_mut(),
      next: null_mut(),
      pc: 0,
      fun: 0,
      base: 0,
      is_native: false,
      open_upvals: HashMap::new(),
      is_new: false,
      this: null_mut(),
    }))
  }

  pub fn set_this(&mut self, this: JsObjPtr) {
    if !self.this.is_null() {
      as_obj(self.this).dec();
    }
    if !this.is_null() {
      as_obj(this).inc();
    }
    self.this = this;
  }
}

impl Drop for CallInfo {
  fn drop(&mut self) {
    if !self.this.is_null() {
      as_obj(self.this).dec();
    }
  }
}

pub enum RKValue {
  Kst(&'static Const),
  JsObj(JsObjPtr),
}

pub type VmPtr = *mut Vm;

#[inline(always)]
pub fn as_vm<T>(ptr: *mut T) -> &'static mut Vm {
  unsafe { &mut (*(ptr as VmPtr)) }
}

pub struct Vm {
  gc: Box<Gc>,
  c: Chunk,
  ci: CallInfoPtr,
  s: Vec<GcObjPtr>,
  pub env: JsDictPtr,
}

impl JsFunction {
  fn tpl(&self) -> &FnTpl {
    unsafe { &(*(self.f as *const FnTpl)) }
  }
}

impl Gc {
  pub fn new_fun_native(&mut self, f: NativeFn, is_root: bool) -> JsFunPtr {
    let nf = self.new_fun(is_root);
    as_fun(nf).is_native = true;
    as_fun(nf).f = f as *const c_void;
    nf
  }
}

pub type NativeFn = fn(vm: VmPtr);

fn print_obj(vm: VmPtr) {
  let args = as_vm(vm).get_args();
  args.iter().for_each(|arg| match as_obj(*arg).kind {
    GcObjKind::Undef => println!("undefined"),
    GcObjKind::Number => println!("{:#?}", as_num(*arg).d),
    GcObjKind::String => println!("{:#?}", as_str(*arg).d),
    GcObjKind::Function => println!("{:#?}", as_fun(*arg)),
    _ => (),
  });
}

impl Vm {
  pub fn new(c: Chunk, max_heap_size: usize) -> Box<Self> {
    let mut v = Box::new(Vm {
      gc: Gc::new(max_heap_size),
      c,
      ci: CallInfo::new(),
      s: vec![],
      env: null_mut(),
    });
    v.env = v.gc.new_dict(true);

    let print_fn = v.gc.new_fun(false);
    as_fun(print_fn).is_native = true;
    as_fun(print_fn).f = print_obj as *const c_void;;
    as_obj(print_fn).inc();
    as_dict(v.env).insert("print", as_obj_ptr(print_fn));

    v.s.push(v.env as GcObjPtr);

    let f = v.gc.new_fun(true);
    let fr = as_fun(f);
    fr.f = &v.c.fun_tpl as *const FnTpl as *const c_void;
    fr.is_native = false;
    v.s.push(f as GcObjPtr);

    let uv = v.gc.new_upval(as_obj_ptr(v.env), false);
    as_ci(v.ci).fun = 1;
    as_ci(v.ci).base = 2;
    as_ci(v.ci).open_upvals.insert(0, uv);

    as_obj(uv).inc();
    fr.upvals.insert(0, uv);

    v
  }

  fn set_stack_slot(&mut self, i: u32, v: JsObjPtr) {
    self.gc.inc(v);
    let i = i as usize;
    if i > self.s.len() - 1 {
      self.s.resize(i + 1, self.gc.js_undef());
    }
    match self.s.get(i) {
      Some(old) => {
        if !(*old).is_null() {
          self.gc.remove_root(*old);
          self.gc.dec(*old);
        }
      }
      _ => (),
    }
    self.gc.append_root(v);
    self.s[i] = v;
  }

  fn get_fn(&self) -> Option<JsFunPtr> {
    if self.ci.is_null() {
      return None;
    }
    let fi = as_ci(self.ci).fun as usize;
    match self.s.get(fi) {
      Some(f) => Some(*f as JsFunPtr),
      _ => None,
    }
  }

  fn fetch(&mut self) -> Result<Option<&Inst>, RuntimeError> {
    let f = match self.get_fn() {
      Some(f) => f,
      _ => return Ok(None),
    };
    let f = as_fun(f);
    if f.is_native {
      return Err(RuntimeError::new("using native fun as js"));
    }
    let pc = as_ci(self.ci).pc as usize;
    as_ci(self.ci).pc += 1;
    let code = &f.tpl().code;
    Ok(code.get(pc))
  }

  pub fn exec(&mut self) -> Result<(), RuntimeError> {
    loop {
      let i = match self.fetch() {
        Err(e) => return Err(e),
        Ok(inst) => match inst {
          None => break,
          Some(i) => i.clone(),
        },
      };
      match self.dispatch(i) {
        Err(e) => return Err(e),
        Ok(_) => (),
      }
    }
    Ok(())
  }

  fn get_kst(&self, r: u32) -> Option<&'static Const> {
    let r = r as usize;
    let mask = 1 << 8;
    let is_k = r & mask == 256;
    if !is_k {
      return None;
    }
    let f = self.get_fn().unwrap();
    let cs = &as_fun(f).tpl().consts;
    Some(&cs[r - 256])
  }

  fn rk(&self, base: u32, r: u32) -> RKValue {
    if let Some(k) = self.get_kst(r) {
      return RKValue::Kst(k);
    }
    RKValue::JsObj(self.get_stack_item(base + r))
  }

  fn x_rk(&mut self, base: u32, r: u32) -> (JsObjPtr, bool) {
    let mut is_new = false;
    let v = match self.rk(base, r) {
      RKValue::Kst(kst) => {
        is_new = true;
        self.gc.new_obj_from_kst(kst, false)
      }
      RKValue::JsObj(k) => k,
    };
    (v, is_new)
  }

  fn get_args(&self) -> Vec<JsObjPtr> {
    let ci = as_ci(self.ci);
    let len = ci.base - ci.fun - 1;
    let mut args = vec![];
    for i in 1..=len {
      args.push(self.get_stack_item(ci.fun + i));
    }
    args
  }

  fn set_return(&mut self, ret: JsObjPtr) {
    self.set_stack_slot(as_ci(self.ci).fun, ret);
  }

  fn get_stack_item(&self, i: u32) -> JsObjPtr {
    match self.s.get(i as usize) {
      Some(v) => {
        if v.is_null() {
          self.gc.js_undef()
        } else {
          *v
        }
      }
      _ => self.gc.js_undef(),
    }
  }

  fn get_upval(&self, i: u32) -> UpValPtr {
    let f = as_fun(self.get_fn().unwrap());
    *f.upvals.get(i as usize).unwrap()
  }

  fn get_fn_tpl(&self, i: u32) -> *const FnTpl {
    let cf = self.get_fn().unwrap();
    let tpl = &as_fun(cf).tpl().fun_tpls[i as usize];
    tpl as *const FnTpl
  }

  fn find_upvals(&mut self, uv_desc: &UpvalDesc) -> UpValPtr {
    let ci = as_ci(self.ci);
    let i = ci.base + uv_desc.idx;
    let open = match as_ci(self.ci).open_upvals.get(&i) {
      Some(uv) => *uv,
      None => {
        let v = self.get_stack_item(i);
        let uv = self.gc.new_upval(v, false);
        as_ci(self.ci).open_upvals.insert(i, uv);
        uv
      }
    };
    open
  }

  fn close_upvals(&mut self) {
    let ci = as_ci(self.ci);
    if ci.is_native {
      return;
    }
    for (i, uv) in &ci.open_upvals {
      as_obj(ci.open_upvals[&i]).dec();
    }
    ci.open_upvals.clear();
  }

  fn clean_ci_stack(&mut self, ret_num: u32) {
    let f = as_ci(self.ci).fun + ret_num;
    let len = self.s.len() as u32;
    for i in f..len {
      self.set_stack_slot(i, self.gc.js_undef());
    }
  }

  fn post_call(&mut self, ret_num: u32) {
    self.close_upvals();
    self.clean_ci_stack(ret_num);

    let cci = self.ci;
    let pci = as_ci(cci).prev;
    if !pci.is_null() {
      as_ci(pci).next = null_mut();
    }
    self.ci = pci;
    unsafe {
      drop_in_place(cci);
    }
  }

  fn dispatch(&mut self, i: Inst) -> Result<(), RuntimeError> {
    let op = OpCode::from_u32(i.op());
    match op {
      OpCode::GETTABUP => {
        let mut ls = LocalScope::new();
        let ra = i.a();
        let rc = i.c();
        let base = as_ci(self.ci).base;
        let (k, is_new) = self.x_rk(base, i.c());
        if is_new {
          ls.reg(k);
        }
        let uv = as_uv(self.get_upval(i.b()));
        let v = as_dict(uv.v).get(k);
        self.set_stack_slot(as_ci(self.ci).base + ra, v);
      }
      OpCode::SETTABUP => {
        let mut ls = LocalScope::new();
        let base = as_ci(self.ci).base;
        let (k, is_new) = self.x_rk(base, i.b());
        if is_new {
          ls.reg(k);
        }
        let (v, is_new) = self.x_rk(base, i.c());
        if is_new {
          ls.reg(v);
        }
        let uv = as_uv(self.get_upval(i.a()));
        as_dict(uv.v).set(k, v);
      }
      OpCode::LOADK => {
        let mut ls = LocalScope::new();
        let ra = i.a();
        let (v, is_new) = self.x_rk(0, i.bx());
        if is_new {
          ls.reg(v);
        }
        self.set_stack_slot(as_ci(self.ci).base + ra, v);
      }
      OpCode::GETUPVAL => {
        let f = self.get_fn().unwrap();
        let uvs = &as_fun(f).upvals;
        let uv = uvs[i.b() as usize];
        self.set_stack_slot(as_ci(self.ci).base + i.a(), as_uv(uv).v);
      }
      OpCode::CLOSURE => {
        let mut ls = LocalScope::new();
        let f = self.gc.new_fun(false);
        as_fun(f).f = self.get_fn_tpl(i.bx()) as *const c_void;
        ls.reg(f);

        let uv_desc = &as_fun(f).tpl().upvals;
        uv_desc.iter().for_each(|uvd| {
          if uvd.in_stack {
            let uv = self.find_upvals(uvd);
            as_obj(uv).inc();
            as_fun(f).upvals.push(uv);
          } else {
            let cf = self.get_fn().unwrap();
            let uvs = &as_fun(cf).upvals;
            let uv = *uvs.get(uvd.idx as usize).unwrap();
            let uv = self.gc.new_upval(as_uv(uv).v, false);
            as_fun(f).upvals.push(uv);
          }
        });

        self.set_stack_slot(as_ci(self.ci).base + i.a(), as_obj_ptr(f));
      }
      OpCode::MOVE => {
        let base = as_ci(self.ci).base;
        let v = self.get_stack_item(base + i.b());
        self.set_stack_slot(base + i.a(), v);
      }
      OpCode::TEST => {
        let base = as_ci(self.ci).base;
        let v = self.get_stack_item(base + i.a());
        let b = as_obj(v).t_bool();
        let com = if as_obj(b).eqs_true() { 1 } else { 0 };
        let eq = com == i.c();
        if eq {
          as_ci(self.ci).pc += 1;
        }
      }
      OpCode::TESTSET => {
        let base = as_ci(self.ci).base;
        let b = self.get_stack_item(base + i.b());
        let tb = as_obj(b).t_bool();
        let com = if as_obj(tb).eqs_true() { 1 } else { 0 };
        let eq = com == i.c();
        if eq {
          as_ci(self.ci).pc += 1;
        } else {
          self.set_stack_slot(base + i.a(), b);
        }
      }
      OpCode::JMP => {
        let pc = as_ci(self.ci).pc as i32;
        as_ci(self.ci).pc = (pc + i.sbx()) as u32;
      }
      OpCode::LOADBOO => {
        let base = as_ci(self.ci).base;
        let b = if i.b() == 1 {
          self.gc.js_true()
        } else {
          self.gc.js_false()
        };
        self.set_stack_slot(base + i.a(), as_obj_ptr(b));
        if i.c() == 1 {
          as_ci(self.ci).pc += 1;
        }
      }
      OpCode::EQ | OpCode::LT | OpCode::LE => {
        let mut ls = LocalScope::new();
        let base = as_ci(self.ci).base;
        let b = self.get_stack_item(base + i.b());
        let (b, is_new) = self.x_rk(base, i.b());
        if is_new {
          ls.reg(b);
        }
        let (c, is_new) = self.x_rk(base, i.c());
        if is_new {
          ls.reg(c);
        }
        let com = match op {
          OpCode::EQ => GcObj::eq(b, c),
          OpCode::LT => GcObj::lt(b, c),
          OpCode::LE => GcObj::le(b, c),
          _ => unimplemented!(),
        };
        let com = if com { 1 } else { 0 };
        if com != i.a() {
          as_ci(self.ci).pc += 1;
        }
      }
      OpCode::ADD | OpCode::SUB | OpCode::MUL | OpCode::DIV | OpCode::MOD => {
        let mut ls = LocalScope::new();
        let base = as_ci(self.ci).base;

        let (a, is_new) = self.x_rk(base, i.b());
        if is_new {
          ls.reg(a);
        }
        let a = as_obj(a).t_num();
        ls.reg(a);

        let (b, is_new) = self.x_rk(base, i.c());
        if is_new {
          ls.reg(b);
        }
        let b = as_obj(b).t_num();
        ls.reg(b);

        let v = match op {
          OpCode::ADD => as_num(a).add(as_num(b)),
          OpCode::SUB => as_num(a).sub(as_num(b)),
          OpCode::MUL => as_num(a).mul(as_num(b)),
          OpCode::DIV => as_num(a).div(as_num(b)),
          OpCode::MOD => as_num(a).modulo(as_num(b)),
          _ => panic!(),
        };
        ls.reg(v);
        self.set_stack_slot(as_ci(self.ci).base + i.a(), as_obj_ptr(v));
      }
      OpCode::NEWTABLE => {
        let mut ls = LocalScope::new();
        let base = as_ci(self.ci).base;
        let v = self.gc.new_dict(false);
        ls.reg(v);
        self.set_stack_slot(base + i.a(), as_obj_ptr(v));
      }
      OpCode::SETTABLE => {
        let mut ls = LocalScope::new();
        let base = as_ci(self.ci).base;
        let tb = self.get_stack_item(base + i.a());
        if !as_obj(tb).check_coercible() {
          panic!("TypeError")
        }
        let tb = as_dict(tb);
        let (k, is_new) = self.x_rk(base, i.b());
        if is_new {
          ls.reg(k);
        }
        let (v, is_new) = self.x_rk(base, i.c());
        if is_new {
          ls.reg(v);
        }
        tb.set(k, v);
      }
      OpCode::GETTABLE => {
        let mut ls = LocalScope::new();
        let base = as_ci(self.ci).base;
        let tb = self.get_stack_item(base + i.b());
        if !as_obj(tb).check_coercible() {
          panic!("TypeError")
        }
        // TODO:: dispatch to various prop resolve handler(number, string, array)
        let tb = as_dict(tb);
        let (k, is_new) = self.x_rk(base, i.c());
        if is_new {
          ls.reg(k);
        }
        let v = tb.get(k);
        self.set_stack_slot(base + i.a(), v);

        // set this for calling function on object
        as_ci(self.ci).set_this(as_obj_ptr(tb));
      }
      OpCode::RETURN => {
        let b = i.b();
        let mut ret_num = b - 1;
        if as_ci(self.ci).is_new {
          ret_num = 1;
          let ret = as_ci(self.ci).this;
          self.set_stack_slot(as_ci(self.ci).fun, ret);
        } else if ret_num == 1 {
          let ret = self.get_stack_item(as_ci(self.ci).base + i.a());
          self.set_stack_slot(as_ci(self.ci).fun, ret);
        }
        self.post_call(ret_num);
      }
      OpCode::CALL => {
        let fi = as_ci(self.ci).base + i.a();
        let fp = self.get_stack_item(fi);
        assert_eq!(
          as_obj(fp).kind,
          GcObjKind::Function,
          "Uncaught TypeError: not a function"
        );
        let f = as_fun(fp);

        let ci = CallInfo::new();
        as_ci(ci).fun = fi;
        as_ci(ci).base = fi + i.b();

        as_ci(ci).prev = self.ci;
        let cci = self.ci;
        as_ci(cci).next = ci;
        as_ci(ci).set_this(as_ci(cci).this);
        self.ci = ci;

        if f.is_native {
          as_ci(ci).is_native = true;
          unsafe {
            let f = std::mem::transmute::<*const c_void, NativeFn>(f.f);
            f(self)
          }
          self.post_call(i.c() - 1);
        } else {
          let mut ls = LocalScope::new();
          let n_args = i.b() - 1;
          let r_arg = as_ci(self.ci).fun + 1;
          let t_arg = as_ci(self.ci).base;
          for i in 0..n_args {
            let mut v = self.get_stack_item(r_arg + i);
            if as_obj(v).pass_by_value() {
              v = as_obj(v).x_pass_by_value();
              ls.reg(v);
            }
            self.set_stack_slot(t_arg + i, v);
          }
          self.exec();
        }
      }
      OpCode::THIS => {
        let base = as_ci(self.ci).base;
        let this = as_ci(self.ci).this;
        self.set_stack_slot(base + i.a(), this);
      }
      OpCode::NEW => {
        let fi = as_ci(self.ci).base + i.a();
        let fp = self.get_stack_item(fi);
        assert_eq!(
          as_obj(fp).kind,
          GcObjKind::Function,
          "Uncaught TypeError: not a function"
        );
        let f = as_fun(fp);

        let ci = CallInfo::new();
        as_ci(ci).fun = fi;
        as_ci(ci).base = fi + i.b();

        as_ci(ci).prev = self.ci;
        let cci = self.ci;
        as_ci(cci).next = ci;
        self.ci = ci;

        // we'll implement this later
        if f.is_native {
          unimplemented!()
        }

        let mut ls = LocalScope::new();
        let this = self.gc.new_dict(false);
        ls.reg(this);

        let n_args = i.b() - 1;
        let r_arg = as_ci(self.ci).fun + 1;
        let t_arg = as_ci(self.ci).base;
        for i in 0..n_args {
          let mut v = self.get_stack_item(r_arg + i);
          if as_obj(v).pass_by_value() {
            v = as_obj(v).x_pass_by_value();
            ls.reg(v);
          }
          self.set_stack_slot(t_arg + i, v);
        }

        as_ci(self.ci).set_this(as_obj_ptr(this));
        as_ci(self.ci).is_new = true;
        self.exec();
      }
      // TODO::
      _ => (),
    }
    Ok(())
  }
}

impl Vm {
  pub fn register_native_fn(&mut self, name: &str, nf: NativeFn) {
    let f = self.gc.new_fun_native(nf, false);
    as_dict(self.env).insert(name, as_obj_ptr(f));
  }
}

#[cfg(test)]
mod exec_tests {
  use super::*;
  use crate::asm::codegen::*;

  #[test]
  fn exec_init_test() {
    let chk = Codegen::gen(
      "var a = 1
    print(a)
    assert(1, a)
    print(native_fn())
    ",
    );
    let mut vm = Vm::new(chk, 1024);

    vm.register_native_fn("assert", |vm: VmPtr| {
      let args = as_vm(vm).get_args();
      assert_eq!(args.len(), 2);
      let a = args[0];
      let b = args[1];
      assert_eq!(as_num(a).d, as_num(b).d);
      println!("assert ok");
    });

    vm.register_native_fn("native_fn", |vm: VmPtr| {
      let mut ls = LocalScope::new();
      let ret = as_vm(vm).gc.new_str(false);
      ls.reg(ret);
      as_str(ret).d.push_str("return from native call");
      as_vm(vm).set_return(as_obj_ptr(ret));
    });

    vm.exec();
  }

  fn new_vm(chk: Chunk) -> Box<Vm> {
    let mut vm = Vm::new(chk, 1 << 20);

    vm.register_native_fn("assert_str_eq", |vm: VmPtr| {
      let args = as_vm(vm).get_args();
      assert_eq!(args.len(), 2);
      let a = args[0];
      let b = args[1];
      assert_eq!(as_str(a).d, as_str(b).d);
    });

    vm.register_native_fn("assert_num_eq", |vm: VmPtr| {
      let args = as_vm(vm).get_args();
      assert_eq!(args.len(), 2);
      let a = args[0];
      let b = args[1];
      assert_eq!(as_num(a).d, as_num(b).d);
    });

    vm
  }

  #[test]
  fn js_fn_test() {
    let chk = Codegen::gen(
      "
    function f() {
      return 'return from f()'
    }

    assert_str_eq('return from f()', f())
    ",
    );

    let mut vm = new_vm(chk);
    vm.exec();
  }

  #[test]
  fn closure_test() {
    let chk = Codegen::gen(
      "
    var a = 1

    function f() {
      var b = 1
      return function () {
        a = a + b
        return a
      }
    }

    function f1() {
      return a
    }
 
    var ff = f()
    assert_num_eq(1, f1())
    ff()
    assert_num_eq(2, f1())
    assert_num_eq(2, a)
    var b = f1() + 1
    var b = 3
    assert_num_eq(3, b)
    ",
    );

    let mut vm = new_vm(chk);
    vm.exec();
  }

  #[test]
  fn and_or_test() {
    let chk = Codegen::gen(
      "
    a = 1
    b = 2
    e = 3
    f = 4
    var c = a && b
    var d = e || f
    assert_num_eq(2, c)
    assert_num_eq(3, d)
    ",
    );

    let mut vm = new_vm(chk);
    vm.exec();
  }

  #[test]
  fn math_test() {
    let chk = Codegen::gen(
      "
    a = 1
    b = 2
    c = 3
    var d = a + b * c
    var e = a / b - c
    assert_num_eq(7, d)
    assert_num_eq(-2.5, e)  
    ",
    );

    let mut vm = new_vm(chk);
    vm.exec();
  }

  #[test]
  fn if_else_test() {
    let chk = Codegen::gen(
      "
    a = 2
    if (a == 1) {
      c = 4
    } else if (a == 2) {
      c = 5
    }
    assert_num_eq(5, c)
    
    a = 0
    if (a) b = 1 else b = 0
    assert_num_eq(0, b)

    a = 1
    if (a) b = 1 else b = 0
    assert_num_eq(1, b)
    ",
    );

    let mut vm = new_vm(chk);
    vm.exec();
  }

  #[test]
  fn lt_le_test() {
    let chk = Codegen::gen(
      "
    b = 3
    a = 1
    if (a < 1) b = 0 else b = 1;
    assert_num_eq(1, b)

    b = 3
    a = 1
    if (a <= 1) b = 0 else b = 1;
    assert_num_eq(0, b)

    if (a >= 1) b = 0 else b = 1;
    assert_num_eq(0, b)

    b = 3
    if (b > 1) b = 0 else b = 1;
    assert_num_eq(0, b)

    a = 1
    b = '1'
    if (a == b) { b = 1 } else b = 0
    assert_num_eq(1, b)
    ",
    );

    let mut vm = new_vm(chk);
    vm.exec();
  }

  #[test]
  fn for_test() {
    let chk = Codegen::gen(
      "
    var a = 10
    function f(a) {
      var ret = 0
      for(var i = 1; i <= a; i++) {
        ret += i
      }
      return ret
    }
    var b = f(a)
    assert_num_eq(10, a)
    assert_num_eq(55, b)
    ",
    );

    let mut vm = new_vm(chk);
    vm.exec();
  }

  #[test]
  fn for_break_test() {
    let chk = Codegen::gen(
      "
    var a = 10
    function f(a) {
      var ret = 0
      for(var i = 1; ; i++) {
        if (i > a) break
        ret += i
      }
      return ret
    }
    var b = f(a)
    assert_num_eq(10, a)
    assert_num_eq(55, b)
    ",
    );

    let mut vm = new_vm(chk);
    vm.exec();
  }

  #[test]
  fn while_test() {
    let chk = Codegen::gen(
      "
    function f(a, b) {
      var ret = 0;
      var i = 1;
      while (i <= b) {
        ret += i
        ++i
      }
      return ret
    }
    var a = f(1, 10)
    assert_num_eq(55, a)

    function f1(a, b) {
      var ret = 0;
      while (true) {
        if (b == 0) break;
        ret += b
        b--
      }
      return ret
    }
    var a = f1(1, 10)
    assert_num_eq(55, a)
    ",
    );

    let mut vm = new_vm(chk);
    vm.exec();
  }

  #[test]
  fn do_while_test() {
    let chk = Codegen::gen(
      "
    function f(a, b) {
      var ret = 0;
      do {
        ret += b
        b--
        if (b == 0) break
      } while (true)
      return ret
    }
    var a = f(1, 10)
    assert_num_eq(55, a)
    ",
    );

    let mut vm = new_vm(chk);
    vm.exec();
  }

  #[test]
  fn pass_by_value_test() {
    let chk = Codegen::gen(
      "
    function f(a) {
       a += 1
    }
    var a = 1
    f(a)
    assert_num_eq(1, a)
    ",
    );

    let mut vm = new_vm(chk);
    vm.exec();
  }

  #[test]
  fn object_literal_test() {
    let chk = Codegen::gen(
      "
      var a = { b: 1, c: {d: '1'} }
      assert_num_eq(1, a.b);
      assert_str_eq('1', a.c.d)
    ",
    );

    let mut vm = new_vm(chk);
    vm.exec();
  }

  #[test]
  fn new_object_test() {
    let chk = Codegen::gen(
      "
      var Person = function (name, age) {
        this.name = name
        this.age = age
      }
      var obj = new Person('tom', 20)
      assert_str_eq('tom', obj.name)
      assert_num_eq(20, obj.age);
    ",
    );

    let mut vm = new_vm(chk);
    vm.exec();
  }

  #[test]
  fn member_access_chain_test() {
    let chk = Codegen::gen(
      "
      var a = {
        b: {
          v: 1
          f: function () {
            return this.v
          }
        }
      };
      assert_num_eq(1, a.b.f());
    ",
    );

    let mut vm = new_vm(chk);
    vm.exec();
  }

  #[test]
  fn logic_test() {
    let chk = Codegen::gen(
      "
      var a = 1
      var b = 2
      var c = a && b
      assert_num_eq(2, c);

      var d = 0
      var e = a && d
      assert_num_eq(0, e);

      var f = d || b
      var g = d && a
      assert_num_eq(2, f);
      assert_num_eq(0, g);
    ",
    );

    let mut vm = new_vm(chk);
    vm.exec();
  }
}
