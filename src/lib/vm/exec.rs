use crate::asm::chunk::*;
use crate::vm::gc::*;
use std::collections::{HashMap, HashSet};
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
  gc: Gc,
  c: Chunk,
  ci: CallInfoPtr,
  // TODO:: release stack items
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
      self.s.resize(i, null_mut());
    }
    match self.s.get(i) {
      Some(old) => {
        if !(*old).is_null() {
          self.gc.dec(*old);
        }
      }
      _ => (),
    }
    self.gc.append_root(v);
    self.s.insert(i, v);
  }

  fn get_fn(&self) -> Result<JsFunPtr, RuntimeError> {
    let f = self.s.get(as_ci(self.ci).fun as usize);
    if f.is_none() {
      return Err(RuntimeError::new("none callable"));
    }
    Ok(*f.unwrap() as JsFunPtr)
  }

  fn fetch(&mut self) -> Result<Option<&Inst>, RuntimeError> {
    let f = match self.get_fn() {
      Ok(f) => f,
      Err(e) => return Err(e),
    };
    let f = as_fun(f);
    if f.is_native {
      return Err(RuntimeError::new("using native fun as js"));
    }
    let pc = as_ci(self.ci).pc;
    as_ci(self.ci).pc += 1;
    Ok(f.tpl().code.get(pc as usize))
  }

  fn exec(&mut self) -> Result<(), RuntimeError> {
    unsafe {
      let fun = self.get_fn().ok().unwrap();
      if !as_fun(fun).is_native {
        let f = &(*(as_fun(fun).f as *const FnTpl));
        println!("{:#?}", f);
      }
    }
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
    let f = self.get_fn().ok().unwrap();
    let cs = &as_fun(f).tpl().consts;
    Some(&cs[r - 256])
  }

  fn rk(&self, base: u32, r: u32) -> RKValue {
    if let Some(k) = self.get_kst(r) {
      return RKValue::Kst(k);
    }
    RKValue::JsObj(self.get_stack_item(base + r))
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
    let f = as_fun(self.get_fn().ok().unwrap());
    *f.upvals.get(i as usize).unwrap()
  }

  fn get_fn_tpl(&self, i: u32) -> *const FnTpl {
    let cf = self.get_fn().ok().unwrap();
    let tpl = &as_fun(cf).tpl().fun_tpls[i as usize];
    tpl as *const FnTpl
  }

  fn dispatch(&mut self, i: Inst) -> Result<(), RuntimeError> {
    let op = OpCode::from_u32(i.op());
    match op {
      OpCode::GETTABUP => {
        let mut ls = LocalScope::new();
        let ra = i.a();
        let rc = i.c();
        let base = as_ci(self.ci).base;
        let k = match self.rk(base, i.c()) {
          RKValue::Kst(kst) => {
            let t = self.gc.new_obj_from_kst(kst, false);
            ls.reg(t)
          }
          RKValue::JsObj(k) => k,
        };
        let uv = as_uv(self.get_upval(i.b()));
        let v = as_dict(uv.v).get(k);
        self.set_stack_slot(as_ci(self.ci).base + ra, v);
      }
      OpCode::SETTABUP => {
        let mut ls = LocalScope::new();
        let ra = i.a();
        let rb = i.b();
        let base = as_ci(self.ci).base;
        let k = match self.rk(base, i.b()) {
          RKValue::Kst(kst) => {
            let t = self.gc.new_obj_from_kst(kst, false);
            ls.reg(t)
          }
          RKValue::JsObj(k) => k,
        };
        let v = match self.rk(base, i.c()) {
          RKValue::Kst(kst) => {
            let t = self.gc.new_obj_from_kst(kst, false);
            ls.reg(t)
          }
          RKValue::JsObj(v) => v,
        };
        let uv = as_uv(self.get_upval(i.a()));
        as_dict(uv.v).set(k, v);
      }
      OpCode::LOADK => {
        let mut ls = LocalScope::new();
        let ra = i.a();
        let v = match self.rk(0, i.bx()) {
          RKValue::Kst(kst) => {
            let t = self.gc.new_obj_from_kst(kst, false);
            ls.reg(t)
          }
          _ => panic!(),
        };
        self.set_stack_slot(as_ci(self.ci).base + ra, v);
      }
      OpCode::CLOSURE => {
        let mut ls = LocalScope::new();
        let f = self.gc.new_fun(false);
        as_fun(f).f = self.get_fn_tpl(i.bx()) as *const c_void;
        ls.reg(f);
        self.set_stack_slot(as_ci(self.ci).base + i.a(), as_obj_ptr(f));
      }
      OpCode::RETURN => {
        let b = i.b();
        let ret_num = b - 1;
        assert!(ret_num <= 1);
        if ret_num == 1 {
          let ret = self.get_stack_item(as_ci(self.ci).base + i.a());
          self.set_stack_slot(as_ci(self.ci).fun, ret);
        }
      }
      OpCode::CALL => {
        let fi = as_ci(self.ci).base + i.a();
        let fp = self.get_stack_item(fi);
        assert_eq!(as_obj(fp).kind, GcObjKind::Function);
        let f = as_fun(fp);

        let ci = CallInfo::new();
        let cci = self.ci;
        as_ci(cci).next = ci;
        as_ci(ci).prev = self.ci;
        as_ci(ci).fun = as_ci(cci).base + i.a();
        as_ci(ci).base = i.b() + as_ci(ci).fun;
        self.ci = ci;

        if f.is_native {
          unsafe {
            let f = std::mem::transmute::<*const c_void, NativeFn>(f.f);
            f(self)
          }
        } else {
          self.exec();
        }

        let cci = self.ci;
        unsafe {
          drop_in_place(cci);
        }
        let pci = as_ci(cci).prev;
        as_ci(pci).next = null_mut();
        self.ci = pci;
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
    as_obj(f).inc();
    as_dict(self.env).insert(name, as_obj_ptr(f));
  }
}

#[cfg(test)]
mod exec_tests {
  use super::*;
  use crate::asm::codegen::*;

  #[test]
  fn exec_init_test() {
    init_gc_data();

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

  #[test]
  fn js_fn_test() {
    init_gc_data();

    let chk = Codegen::gen(
      "
    function f() {
      return 'return from f()'
    }
   
    print(f())
    ",
    );

    println!("{:#?}", &chk);
    let mut vm = Vm::new(chk, 1024);
    vm.exec();
  }
}
