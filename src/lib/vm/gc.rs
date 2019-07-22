use crate::asm::chunk::*;
use linked_hash_set::LinkedHashSet;
use std::collections::{HashMap, HashSet};
use std::mem;
use std::os::raw::c_void;
use std::ptr::{drop_in_place, null, null_mut};
use std::sync::Once;

pub type GcObjPtr = *mut GcObj;
pub type JsObj = GcObj;
pub type JsObjPtr = GcObjPtr;

pub type JsStrPtr = *mut JsString;
pub type JsNumPtr = *mut JsNumber;
pub type JsBoolPtr = *mut JsBoolean;
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
pub fn as_bool<T>(ptr: *mut T) -> &'static mut JsBoolean {
  unsafe { &mut (*(ptr as JsBoolPtr)) }
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
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
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

  pub fn gc(&self) -> GcPtr {
    self.gc
  }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct JsString {
  base: GcObj,
  pub d: String,
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
  pub d: f64,
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
pub struct JsBoolean {
  base: GcObj,
  pub d: bool,
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
    as_obj(v).inc();
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

  r.upvals.iter().for_each(|uv| as_obj(*uv).dec());

  unsafe {
    drop_in_place(r);
  }
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
  obj_list: LinkedHashSet<GcObjPtr>,
  roots: HashSet<GcObjPtr>,
  marked: Vec<GcObjPtr>,
  js_null_: JsObjPtr,
  js_undef_: JsObjPtr,
  js_true_: JsBoolPtr,
  js_false_: JsBoolPtr,
}

static INIT_GC_DATA_ONCE: Once = Once::new();

impl Gc {
  pub fn new(max_heap_size: usize) -> Box<Self> {
    let mut gc = Box::new(Gc {
      heap_size: 0,
      max_heap_size,
      obj_list: LinkedHashSet::new(),
      roots: HashSet::new(),
      marked: vec![],
      js_null_: null_mut(),
      js_undef_: null_mut(),
      js_true_: null_mut(),
      js_false_: null_mut(),
    });
    gc.init_data();
    gc
  }

  fn init_data(&mut self) {
    self.js_null_ = Box::into_raw(Box::new(GcObj {
      ref_cnt: 1,
      kind: GcObjKind::Null,
      gc: self as GcPtr,
      deinit: default_deinit,
    }));

    self.js_undef_ = Box::into_raw(Box::new(GcObj {
      ref_cnt: 1,
      kind: GcObjKind::Undef,
      gc: self as GcPtr,
      deinit: default_deinit,
    }));

    self.js_true_ = Box::into_raw(Box::new(JsBoolean {
      base: GcObj {
        ref_cnt: 1,
        kind: GcObjKind::Boolean,
        gc: self as GcPtr,
        deinit: default_deinit,
      },
      d: true,
    }));

    self.js_false_ = Box::into_raw(Box::new(JsBoolean {
      base: GcObj {
        ref_cnt: 1,
        kind: GcObjKind::Boolean,
        gc: self as GcPtr,
        deinit: default_deinit,
      },
      d: false,
    }));
  }

  pub fn js_null(&self) -> JsObjPtr {
    self.js_null_
  }

  pub fn js_undef(&self) -> JsObjPtr {
    self.js_undef_
  }

  pub fn js_true(&self) -> JsBoolPtr {
    self.js_true_
  }

  pub fn js_false(&self) -> JsBoolPtr {
    self.js_false_
  }

  pub fn new_str(&mut self, is_root: bool) -> JsStrPtr {
    let need = mem::size_of::<JsString>();
    self.xgc(need);
    self.heap_size += need;
    JsString::new(self, is_root)
  }

  pub fn new_str_from_kst(&mut self, kst: &Const, is_root: bool) -> JsStrPtr {
    let s = self.new_str(is_root);
    as_str(s).d = kst.str().to_owned();
    s
  }

  pub fn new_obj_from_kst(&mut self, kst: &Const, is_root: bool) -> JsObjPtr {
    match kst {
      Const::String(_) => as_obj_ptr(self.new_str_from_kst(kst, is_root)),
      Const::Number(_) => as_obj_ptr(self.new_num_from_kst(kst, is_root)),
    }
  }

  pub fn new_num(&mut self, is_root: bool) -> JsNumPtr {
    let need = mem::size_of::<JsNumber>();
    self.xgc(need);
    self.heap_size += need;
    JsNumber::new(self, is_root)
  }

  pub fn new_num_from_kst(&mut self, kst: &Const, is_root: bool) -> JsNumPtr {
    let n = self.new_num(is_root);
    as_num(n).d = kst.num();
    n
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
    let ptr = ptr as JsObjPtr;
    if !self.obj_list.contains(&ptr) {
      return;
    }

    let obj = as_obj(ptr);
    match obj.kind {
      GcObjKind::Null | GcObjKind::Undef | GcObjKind::Boolean => return,
      _ => (),
    }
    let em = format!("dropping dangling object {:#?} {:#?}", ptr, obj);
    assert!(obj.ref_cnt > 0, em);
    obj.ref_cnt -= 1;
    if obj.ref_cnt == 0 {
      self.drop(ptr);
    }
  }

  pub fn drop<T>(&mut self, ptr: *mut T) {
    let ptr = ptr as JsObjPtr;
    let obj = as_obj(ptr);
    self.obj_list.remove(&ptr);
    self.roots.remove(&ptr);
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
          GcObjKind::Function => {
            for pp in &as_fun(obj).upvals {
              self.marked.push(as_obj_ptr(*pp))
            }
          }
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
      if self.obj_list.contains(&p) {
        self.dec(p);
      }
    }
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

#[cfg(test)]
mod gc_tests {
  use super::*;

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
