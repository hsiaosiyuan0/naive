use std::alloc::{handle_alloc_error, Alloc, Global, Layout};
use std::collections::{HashMap, HashSet};
use std::mem;
use std::os::raw::c_void;
use std::ptr::{drop_in_place, null, null_mut, NonNull};
use std::sync::Once;

pub type GcObjPtr = *mut GcObj;
pub type JsObj = GcObj;
pub type JsObjPtr = *mut JsObj;

pub type JsStrPtr = *mut JsString;
pub type JsNumPtr = *mut JsNumber;
pub type JsArrPtr = *mut JsArray;
pub type JsDictPtr = *mut JsDict;
pub type JsFunPtr = *mut JsFunction;

pub type UpValPtr = *mut UpVal;

pub type GcPtr = *mut Gc;

#[inline(always)]
pub fn as_obj_ptr<T>(ptr: *mut T) -> JsObjPtr {
  ptr as JsObjPtr
}

#[inline(always)]
pub fn as_obj<T>(ptr: *mut T) -> &'static mut GcObj {
  unsafe { &mut (*(ptr as GcObjPtr)) }
}

#[inline(always)]
pub fn as_str<T>(ptr: *mut T) -> &'static mut JsString {
  unsafe { &mut (*(ptr as JsStrPtr)) }
}

#[inline(always)]
pub fn as_num<T>(ptr: *mut T) -> &'static mut JsNumber {
  unsafe { &mut (*(ptr as JsNumPtr)) }
}

#[inline(always)]
pub fn as_arr<T>(ptr: *mut T) -> &'static mut JsArray {
  unsafe { &mut (*(ptr as JsArrPtr)) }
}

#[inline(always)]
pub fn as_dict<T>(ptr: *mut T) -> &'static mut JsDict {
  unsafe { &mut (*(ptr as JsDictPtr)) }
}

#[inline(always)]
pub fn as_fun<T>(ptr: *mut T) -> &'static mut JsFunction {
  unsafe { &mut (*(ptr as JsFunPtr)) }
}

#[inline(always)]
pub fn as_uv<T>(ptr: *mut T) -> &'static mut UpVal {
  unsafe { &mut (*(ptr as UpValPtr)) }
}

#[inline(always)]
pub fn as_gc<T>(ptr: *mut T) -> &'static mut Gc {
  unsafe { &mut (*(ptr as GcPtr)) }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub enum GcObjKind {
  String,
  Number,

  Array,
  Dict,
  Function,

  UpVal,

  Boolean,
  Null,
  Undef,
}

pub type Deinit = fn(ptr: JsObjPtr);

fn default_deinit(ptr: JsObjPtr) {}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct GcObj {
  ref_cnt: usize,
  pub kind: GcObjKind,
  gc: GcPtr,
  deinit: Deinit,
}

impl GcObj {
  pub fn inc(&mut self) {
    self.ref_cnt += 1;
  }

  pub fn dec(&mut self) {
    as_gc(self.gc).dec(self);
  }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct JsString {
  base: GcObj,
  d: String,
}

fn js_str_deinit(ptr: JsObjPtr) {
  let r = as_str(ptr);
  let s = mem::size_of_val(r);
  unsafe {
    drop_in_place(r);
  }
  as_gc(r.base.gc).heap_size -= s;
}

impl JsString {
  pub fn new(gc: &mut Gc, is_root: bool) -> JsStrPtr {
    let ptr = Box::into_raw(Box::new(JsString {
      base: GcObj {
        ref_cnt: 1,
        kind: GcObjKind::String,
        gc,
        deinit: js_str_deinit,
      },
      d: "".to_owned(),
    }));
    gc.register(as_obj_ptr(ptr), is_root);
    ptr
  }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct JsNumber {
  base: GcObj,
  d: f64,
}

fn js_num_deinit(ptr: JsObjPtr) {
  let r = as_num(ptr);
  let s = mem::size_of_val(r);
  unsafe {
    drop_in_place(r);
  }
  as_gc(r.base.gc).heap_size -= s;
}

impl JsNumber {
  fn new(gc: &mut Gc, is_root: bool) -> JsNumPtr {
    let ptr = Box::into_raw(Box::new(JsNumber {
      base: GcObj {
        ref_cnt: 1,
        kind: GcObjKind::Number,
        deinit: js_num_deinit,
        gc,
      },
      d: 0.0,
    }));
    gc.register(as_obj_ptr(ptr), is_root);
    ptr
  }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct JsArray {
  base: GcObj,
  d: Vec<JsObjPtr>,
}

fn js_arr_deinit(ptr: JsObjPtr) {
  let r = as_arr(ptr);
  let s = mem::size_of_val(r);

  let gc = as_gc(r.base.gc);
  for pp in &r.d {
    gc.dec(*pp);
  }

  unsafe {
    drop_in_place(r);
  }
  gc.heap_size -= s;
}

impl JsArray {
  fn new(gc: &mut Gc, is_root: bool) -> JsArrPtr {
    let ptr = Box::into_raw(Box::new(JsArray {
      base: GcObj {
        ref_cnt: 1,
        kind: GcObjKind::Array,
        deinit: js_arr_deinit,
        gc,
      },
      d: vec![],
    }));
    gc.register(as_obj_ptr(ptr), is_root);
    ptr
  }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct JsDict {
  base: GcObj,
  pub d: HashMap<String, JsObjPtr>,
}

fn js_dict_deinit(ptr: JsObjPtr) {
  let r = as_dict(ptr);
  let s = mem::size_of_val(r);

  let gc = as_gc(r.base.gc);
  for (_, pp) in &r.d {
    gc.dec(*pp);
  }

  unsafe {
    drop_in_place(r);
  }
  gc.heap_size -= s;
}

impl JsDict {
  fn new(gc: &mut Gc, is_root: bool) -> JsDictPtr {
    let ptr = Box::into_raw(Box::new(JsDict {
      base: GcObj {
        ref_cnt: 1,
        kind: GcObjKind::Dict,
        deinit: js_dict_deinit,
        gc,
      },
      d: HashMap::new(),
    }));
    gc.register(as_obj_ptr(ptr), is_root);
    ptr
  }

  pub fn insert(&mut self, k: &str, v: JsObjPtr) {
    self.d.insert(k.to_owned(), v);
  }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct UpVal {
  base: GcObj,
  pub v: JsObjPtr,
}

fn upval_deinit(ptr: JsObjPtr) {
  let r = as_uv(ptr);
  let s = mem::size_of_val(r);

  let gc = as_gc(r.base.gc);
  gc.dec(r.v);

  unsafe {
    drop_in_place(r);
  }
  gc.heap_size -= s;
}

impl UpVal {
  fn new(gc: &mut Gc, v: JsObjPtr, is_root: bool) -> UpValPtr {
    let ptr = Box::into_raw(Box::new(UpVal {
      base: GcObj {
        ref_cnt: 1,
        kind: GcObjKind::UpVal,
        deinit: upval_deinit,
        gc,
      },
      v,
    }));
    gc.register(as_obj_ptr(ptr), is_root);
    ptr
  }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct JsFunction {
  base: GcObj,
  pub f: *const c_void,
  pub is_native: bool,
  pub upvals: Vec<UpValPtr>,
}

fn js_fun_deinit(ptr: JsObjPtr) {
  let r = as_fun(ptr);
  let s = mem::size_of_val(r);
  unsafe {
    drop_in_place(r);
  }
  // TODO:: dec upvalues
  as_gc(r.base.gc).heap_size -= s;
}

impl JsFunction {
  fn new(gc: &mut Gc, is_root: bool) -> JsFunPtr {
    let ptr = Box::into_raw(Box::new(JsFunction {
      base: GcObj {
        ref_cnt: 1,
        kind: GcObjKind::Function,
        deinit: js_fun_deinit,
        gc,
      },
      f: null(),
      is_native: false,
      upvals: vec![],
    }));
    gc.register(as_obj_ptr(ptr), is_root);
    ptr
  }
}

#[repr(C)]
#[derive(Debug)]
pub struct Gc {
  heap_size: usize,
  max_heap_size: usize,
  obj_list: HashSet<GcObjPtr>,
  roots: HashSet<GcObjPtr>,
  marked: Vec<GcObjPtr>,
}

impl Gc {
  pub fn new(max_heap_size: usize) -> Self {
    Gc {
      heap_size: 0,
      max_heap_size,
      obj_list: HashSet::new(),
      roots: HashSet::new(),
      marked: vec![],
    }
  }

  pub fn new_str(&mut self, is_root: bool) -> JsStrPtr {
    let need = mem::size_of::<JsString>();
    self.xgc(need);
    self.heap_size += need;
    JsString::new(self, is_root)
  }

  pub fn new_num(&mut self, is_root: bool) -> JsNumPtr {
    let need = mem::size_of::<JsNumber>();
    self.xgc(need);
    self.heap_size += need;
    JsNumber::new(self, is_root)
  }

  pub fn new_arr(&mut self, is_root: bool) -> JsArrPtr {
    let need = mem::size_of::<JsArray>();
    self.xgc(need);
    self.heap_size += need;
    JsArray::new(self, is_root)
  }

  pub fn new_dict(&mut self, is_root: bool) -> JsDictPtr {
    let need = mem::size_of::<JsDict>();
    self.xgc(need);
    self.heap_size += need;
    JsDict::new(self, is_root)
  }

  pub fn new_fun(&mut self, is_root: bool) -> JsFunPtr {
    let need = mem::size_of::<JsFunction>();
    self.xgc(need);
    self.heap_size += need;
    JsFunction::new(self, is_root)
  }

  pub fn new_upval(&mut self, v: JsObjPtr, is_root: bool) -> UpValPtr {
    let need = mem::size_of::<UpVal>();
    self.xgc(need);
    self.heap_size += need;
    self.inc(v);
    UpVal::new(self, v, is_root)
  }

  pub fn register(&mut self, ptr: JsObjPtr, is_root: bool) {
    self.obj_list.insert(ptr);
    if is_root {
      self.roots.insert(ptr);
    }
  }

  pub fn remove_root(&mut self, ptr: JsObjPtr) {
    self.roots.remove(&as_obj_ptr(ptr));
  }

  pub fn append_root(&mut self, ptr: JsObjPtr) {
    self.roots.insert(ptr);
  }

  pub fn inc<T>(&self, ptr: *mut T) {
    as_obj(ptr).ref_cnt += 1;
  }

  pub fn dec<T>(&mut self, ptr: *mut T) {
    let obj = as_obj(ptr);
    assert!(obj.ref_cnt > 0);
    obj.ref_cnt -= 1;
    if obj.ref_cnt == 0 {
      self.drop(ptr);
    }
  }

  pub fn drop<T>(&mut self, ptr: *mut T) {
    let obj = as_obj(ptr);
    assert!(self.obj_list.remove(&(ptr as GcObjPtr)));
    self.roots.remove(&(ptr as GcObjPtr));
    (obj.deinit)(obj);
  }

  pub fn xgc(&mut self, need: usize) {
    let s = self.heap_size + need;
    if s >= self.max_heap_size {
      self.gc();
      assert!(self.heap_size + need <= self.max_heap_size);
    }
  }

  fn reset_all_ref_cnt(&self) {
    for pp in &self.obj_list {
      as_obj(*pp).ref_cnt = 0;
    }
  }

  fn mark_phase(&mut self) {
    for root in &self.roots {
      self.marked.push(*root);
    }
    while let Some(ptr) = self.marked.pop() {
      let obj = as_obj(ptr);
      obj.ref_cnt += 1;
      if obj.ref_cnt == 1 {
        match obj.kind {
          GcObjKind::String
          | GcObjKind::Number
          | GcObjKind::Boolean
          | GcObjKind::Undef
          | GcObjKind::Null => (),
          GcObjKind::Array => {
            for pp in &as_arr(obj).d {
              self.marked.push(*pp)
            }
          }
          GcObjKind::Dict => {
            for (_, pp) in &as_dict(obj).d {
              self.marked.push(*pp)
            }
          }
          GcObjKind::UpVal => {
            self.marked.push(as_uv(obj).v);
          }
          // TODO::
          GcObjKind::Function => (),
        }
      }
    }
  }

  fn sweep_phase(&mut self) {
    let mut cc = vec![];
    for pp in &self.obj_list {
      let ptr = *pp;
      if as_obj(ptr).ref_cnt == 0 {
        cc.push(ptr);
      }
    }
    cc.iter().for_each(|ptr| {
      self.drop(*ptr);
    });
  }

  pub fn gc(&mut self) {
    self.reset_all_ref_cnt();
    self.mark_phase();
    self.sweep_phase();
  }
}

impl Drop for Gc {
  fn drop(&mut self) {
    for p in self.obj_list.clone() {
      self.drop(p);
    }
  }
}

static mut JS_NULL: JsObjPtr = null_mut();
static mut JS_UNDEF: JsObjPtr = null_mut();
static mut JS_TRUE: JsObjPtr = null_mut();
static mut JS_FALSE: JsObjPtr = null_mut();

pub fn js_null() -> JsObjPtr {
  unsafe { JS_NULL }
}

pub fn js_undef() -> JsObjPtr {
  unsafe { JS_UNDEF }
}

pub fn js_true() -> JsObjPtr {
  unsafe { JS_TRUE }
}

pub fn js_false() -> JsObjPtr {
  unsafe { JS_FALSE }
}

static INIT_GC_DATA_ONCE: Once = Once::new();
pub fn init_gc_data() {
  INIT_GC_DATA_ONCE.call_once(|| unsafe {
    JS_NULL = Box::into_raw(Box::new(GcObj {
      ref_cnt: 1,
      kind: GcObjKind::Null,
      gc: null_mut(),
      deinit: default_deinit,
    }));

    JS_UNDEF = Box::into_raw(Box::new(GcObj {
      ref_cnt: 1,
      kind: GcObjKind::Undef,
      gc: null_mut(),
      deinit: default_deinit,
    }));

    JS_TRUE = Box::into_raw(Box::new(GcObj {
      ref_cnt: 1,
      kind: GcObjKind::Boolean,
      gc: null_mut(),
      deinit: default_deinit,
    }));

    JS_FALSE = Box::into_raw(Box::new(GcObj {
      ref_cnt: 1,
      kind: GcObjKind::Boolean,
      gc: null_mut(),
      deinit: default_deinit,
    }));
  });
}

#[cfg(test)]
mod gc_tests {
  use super::*;
  use crate::asm::codegen::*;

  #[test]
  fn str_test() {
    let mut gc = Gc::new(100);
    let str1 = gc.new_str(true);
    gc.dec(str1);
    let str2 = gc.new_str(true);
    assert_eq!(gc.obj_list.len(), 1);
  }

  #[test]
  fn arr_test() {
    let mut gc = Gc::new(100);
    let arr = gc.new_arr(true);
    gc.dec(arr);
    assert_eq!(gc.obj_list.len(), 0);
  }

  #[test]
  fn dict_test() {
    let mut gc = Gc::new(300);
    let dict = gc.new_dict(true);
    let str1 = gc.new_str(false);
    let arr = gc.new_arr(false);
    as_dict(dict).insert("test", as_obj_ptr(arr));
    assert_eq!(gc.obj_list.len(), 3);

    gc.dec(dict);
    gc.gc();
    assert_eq!(gc.obj_list.len(), 0);
  }
}
