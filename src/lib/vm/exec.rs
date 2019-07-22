use crate::asm::chunk::*;
use crate::vm::gc::*;
use crate::vm::obj::*;
use std::collections::{HashMap, HashSet};
use std::mem;
use std::os::raw::c_void;
use std::ptr::{drop_in_place, null_mut};

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
    }))
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

pub struct LocalScope {
  vals: HashSet<JsObjPtr>,
}

impl LocalScope {
  pub fn new() -> Self {
    LocalScope {
      vals: HashSet::new(),
    }
  }

  pub fn reg<T>(&mut self, v: *mut T) -> JsObjPtr {
    let v = v as JsObjPtr;
    self.vals.insert(v);
    v
  }
}

impl Drop for LocalScope {
  fn drop(&mut self) {
    self.vals.iter().for_each(|v| as_obj(*v).dec())
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
      self.s.resize(i + 1, null_mut());
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
    mem::replace(&mut self.s[i], v);
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

  fn exec(&mut self) -> Result<(), RuntimeError> {
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
    *self.s.get(i as usize).unwrap()
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
      OpCode::TESTSET => {
        let base = as_ci(self.ci).base;
        let b = self.get_stack_item(base + i.b());
        let eq = if i.c() == 1 {
          // the boolean values `true` and `false` are singleton
          // them don't need to be released after been used
          let t = as_obj(b).t_bool();
          as_obj(t).eqs_true()
        } else {
          as_obj(b).eqs_false()
        };
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
      OpCode::RETURN => {
        let b = i.b();
        let ret_num = b - 1;
        assert!(ret_num <= 1);
        if ret_num == 1 {
          let ret = self.get_stack_item(as_ci(self.ci).base + i.a());
          self.set_stack_slot(as_ci(self.ci).fun, ret);
        }
        self.post_call(ret_num);
      }
      OpCode::CALL => {
        let fi = as_ci(self.ci).base + i.a();
        let fp = self.get_stack_item(fi);
        assert_eq!(as_obj(fp).kind, GcObjKind::Function);
        let f = as_fun(fp);

        let ci = CallInfo::new();
        as_ci(ci).fun = fi;
        as_ci(ci).base = fi + i.b();

        as_ci(ci).prev = self.ci;
        let cci = self.ci;
        as_ci(cci).next = ci;
        self.ci = ci;

        if f.is_native {
          as_ci(ci).is_native = true;
          unsafe {
            let f = std::mem::transmute::<*const c_void, NativeFn>(f.f);
            f(self)
          }
          self.post_call(i.c() - 1);
        } else {
          self.exec();
        }
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
    println!("{:#?}", &chk);
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

    println!("{:#?}", chk);
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

    println!("{:#?}", chk);
    let mut vm = new_vm(chk);
    vm.exec();
  }
}
