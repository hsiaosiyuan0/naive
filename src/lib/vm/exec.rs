use crate::asm::chunk::*;
use crate::vm::gc::*;
use std::collections::HashMap;
use std::os::raw::c_void;
use std::ptr::null_mut;

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

pub type VmPtr = *mut Vm;

#[inline(always)]
pub fn as_vm<T>(ptr: *mut T) -> &'static mut Vm {
  unsafe { &mut (*(ptr as VmPtr)) }
}

pub struct Vm {
  //  ci: CallInfoPtr,
  gc: Gc,
  c: Chunk,
  ci: CallInfoPtr,
  s: Vec<GcObjPtr>,
  env: JsDictPtr,
}

impl JsFunction {
  fn tpl(&self) -> &FnTpl {
    unsafe { &(*(self.f as *const FnTpl)) }
  }
}

pub type NativeFn = fn(vm: *mut Vm);

fn print_obj(vm: VmPtr) {
  let args = as_vm(vm).get_args();
  args.iter().for_each(|arg| match as_obj(*arg).kind {
    GcObjKind::Undef => println!("undefined"),
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
    let fp = print_obj as *const c_void;
    as_fun(print_fn).f = fp;
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

  fn rk(&self, r: u32) -> Option<&'static Const> {
    let r = r as usize;
    let mask = !(1 << 8);
    let is_k = r & mask == 256;
    if is_k {
      return None;
    }
    let f = self.get_fn().ok().unwrap();
    Some(&as_fun(f).tpl().consts[r - 256])
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

  fn get_stack_item(&self, i: u32) -> JsObjPtr {
    *self.s.get(i as usize).unwrap()
  }

  fn dispatch(&mut self, i: Inst) -> Result<(), RuntimeError> {
    let op = OpCode::from_u32(i.op());
    match op {
      OpCode::GETTABUP => {
        let ra = i.a();
        let rc = i.c();
        let k = self.rk(rc).unwrap();
        let f = as_fun(self.get_fn().ok().unwrap());
        let d = *f.upvals.get(i.b() as usize).unwrap();
        let v = as_dict(as_uv(d).v).get(k.str());
        self.set_stack_slot(as_ci(self.ci).base + ra, v);
      }
      OpCode::CALL => {
        let fi = as_ci(self.ci).base + i.a();
        let fp = self.get_stack_item(fi);
        let f = as_fun(fp);
        if f.is_native {
          let ci = CallInfo::new();
          let cci = self.ci;
          as_ci(cci).next = ci;
          as_ci(ci).prev = self.ci;
          as_ci(ci).fun = as_ci(cci).base + i.a();
          as_ci(ci).base = i.b() + as_ci(ci).fun;
          self.ci = ci;
          unsafe {
            let f = std::mem::transmute::<*const c_void, NativeFn>(f.f);
            f(self)
          }
        }
      }
      // TODO::
      _ => (),
    }
    Ok(())
  }
}

#[cfg(test)]
mod exec_tests {
  use super::*;
  use crate::asm::codegen::*;

  #[test]
  fn exec_init_test() {
    init_gc_data();

    let chk = Codegen::gen("print(a)");
    println!("{:#?}", &chk);
    let mut vm = Vm::new(chk, 1024);
    vm.exec();
  }
}
