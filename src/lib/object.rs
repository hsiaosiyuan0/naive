use std::collections::HashMap;
use std::ptr::null_mut;
use std::sync::Once;

#[repr(C)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum JSObjectType {
  String,
  Number,
  Boolean,
  Array,
  Dict,
  Null,
  Undefined,
}

pub struct RefCounting {}

pub type JSObjPtr = *mut JSObject;
pub type JSStrPtr = *mut JSString;
pub type JSNumPtr = *mut JSNumber;
pub type JSBoolPtr = *mut JSBoolean;
pub type JSArrPtr = *mut JSArray;
pub type JSDictPtr = *mut JSDict;
pub type JSNullPtr = *mut JSNull;
pub type JSUndefPtr = *mut JSUndefined;

impl RefCounting {
  pub fn ref_cnt(ptr: JSObjPtr) -> i32 {
    let obj = unsafe { Box::from_raw(ptr) };
    let rc = obj.ref_cnt;
    Box::into_raw(obj);
    rc
  }

  pub fn inc(ptr: JSObjPtr) {
    let mut obj = unsafe { Box::from_raw(ptr) };
    obj.ref_cnt += 1;
    Box::into_raw(obj);
  }

  pub fn dec(ptr: JSObjPtr) {
    let mut obj = unsafe { Box::from_raw(ptr) };
    obj.ref_cnt -= 1;
    RefCounting::x_release(Box::into_raw(obj));
  }

  pub fn x_release(ptr: JSObjPtr) {
    let rc = RefCounting::ref_cnt(ptr);
    assert!(rc >= 0);
    if rc > 0 {
      return;
    }
    let obj = unsafe { Box::from_raw(ptr) };
    let typ = obj.typ;
    Box::into_raw(obj);
    match typ {
      JSObjectType::Array => {
        let arr = unsafe { Box::from_raw(ptr as JSArrPtr) };
        for item in &arr.a {
          RefCounting::dec(*item)
        }
        Box::into_raw(arr);
      }
      JSObjectType::Dict => {
        let dict = unsafe { Box::from_raw(ptr as JSDictPtr) };
        for (_, v) in &dict.d {
          RefCounting::dec(*v);
        }
        Box::into_raw(dict);
      }
      _ => (),
    };
    unsafe { drop(Box::from_raw(ptr)) }
  }
}

#[repr(C)]
#[derive(Debug)]
pub struct JSObject {
  pub ref_cnt: i32,
  pub typ: JSObjectType,
}

impl JSObject {
  pub fn is_type(ptr: JSObjPtr, typ: JSObjectType) -> bool {
    let obj = unsafe { Box::from_raw(ptr) };
    let t = obj.typ;
    Box::into_raw(obj);
    t == typ
  }

  pub fn is_str(ptr: JSObjPtr) -> bool {
    JSObject::is_type(ptr, JSObjectType::String)
  }

  pub fn add(lhs: JSObjPtr, rhs: JSObjPtr) -> JSObjPtr {
    let lv = JSNumber::val(lhs as JSNumPtr);
    let rv = JSNumber::val(rhs as JSNumPtr);
    JSNumber::from_f64(lv + rv) as JSObjPtr
  }

  pub fn primitive() -> JSObjPtr {}

  pub fn add_str() -> JSObjPtr {}

  pub fn add_num() -> JSObjPtr {}
}

#[repr(C)]
#[derive(Debug)]
pub struct JSNumber {
  pub base: JSObject,
  pub v: f64,
}

impl JSNumber {
  pub fn from_f64(v: f64) -> JSNumPtr {
    Box::into_raw(Box::new(JSNumber {
      base: JSObject {
        ref_cnt: 1,
        typ: JSObjectType::Number,
      },
      v,
    }))
  }

  pub fn from_str(s: &str) -> JSNumPtr {
    Box::into_raw(Box::new(JSNumber {
      base: JSObject {
        ref_cnt: 1,
        typ: JSObjectType::Number,
      },
      v: s.parse().unwrap(),
    }))
  }

  pub fn val(n: JSNumPtr) -> f64 {
    let n = unsafe { Box::from_raw(n) };
    let v = n.v;
    Box::into_raw(n);
    v
  }
}

#[repr(C)]
#[derive(Debug)]
pub struct JSString {
  pub base: JSObject,
  pub s: String,
}

impl JSString {
  pub fn new(s: &str) -> JSStrPtr {
    Box::into_raw(Box::new(JSString {
      base: JSObject {
        ref_cnt: 1,
        typ: JSObjectType::String,
      },
      s: String::from(s),
    }))
  }
}

#[repr(C)]
#[derive(Debug)]
pub struct JSBoolean {
  pub base: JSObject,
  pub v: bool,
}

#[repr(C)]
#[derive(Debug)]
pub struct JSArray {
  pub base: JSObject,
  pub a: Vec<*mut JSObject>,
}

impl JSArray {
  pub fn new() -> JSArrPtr {
    Box::into_raw(Box::new(JSArray {
      base: JSObject {
        ref_cnt: 1,
        typ: JSObjectType::Array,
      },
      a: vec![],
    }))
  }

  pub fn push(arr: JSArrPtr, item: JSObjPtr) {
    let mut arr = unsafe { Box::from_raw(arr) };
    arr.a.push(item);
    RefCounting::inc(item);
    Box::into_raw(arr);
  }

  pub fn pop(arr: JSArrPtr) -> JSObjPtr {
    let mut arr = unsafe { Box::from_raw(arr) };
    unsafe {
      let ret = match arr.a.pop() {
        Some(ptr) => {
          RefCounting::dec(ptr);
          ptr
        }
        None => JS_UNDEF,
      };
      Box::into_raw(arr);
      ret
    }
  }
}

#[repr(C)]
#[derive(Debug)]
pub struct JSDict {
  pub base: JSObject,
  pub d: HashMap<String, JSObjPtr>,
}

impl JSDict {
  pub fn new() -> JSDictPtr {
    Box::into_raw(Box::new(JSDict {
      base: JSObject {
        ref_cnt: 1,
        typ: JSObjectType::Dict,
      },
      d: HashMap::new(),
    }))
  }

  pub fn set(map: JSDictPtr, k: &str, v: JSObjPtr) {
    let mut dict = unsafe { Box::from_raw(map) };
    dict.d.insert(k.to_owned(), v);
    RefCounting::inc(v);
    Box::into_raw(dict);
  }

  pub fn get(map: JSDictPtr, k: &str) -> JSObjPtr {
    let dict = unsafe { Box::from_raw(map) };
    unsafe {
      let ret = match dict.d.get(k) {
        Some(v) => *v,
        None => JS_UNDEF,
      };
      Box::into_raw(dict);
      ret
    }
  }

  pub fn del(map: JSDictPtr, k: &str) -> JSObjPtr {
    let mut dict = unsafe { Box::from_raw(map) };
    unsafe {
      let v = match dict.d.remove(k) {
        Some(v) => {
          RefCounting::dec(v);
          v
        }
        None => JS_UNDEF,
      };
      Box::into_raw(dict);
      v
    }
  }
}

#[repr(C)]
#[derive(Debug)]
pub struct JSNull {
  pub base: JSObject,
}

#[repr(C)]
#[derive(Debug)]
pub struct JSUndefined {
  pub base: JSObject,
}

pub static mut JS_NULL: JSObjPtr = null_mut();
pub static mut JS_UNDEF: JSObjPtr = null_mut();

pub fn js_null() -> JSObjPtr {
  unsafe { JS_NULL }
}

pub fn js_undef() -> JSObjPtr {
  unsafe { JS_UNDEF }
}

static INIT_JS_NULL_UNDEF_ONCE: Once = Once::new();
pub fn init_js_null_undef() {
  INIT_JS_NULL_UNDEF_ONCE.call_once(|| unsafe {
    let ptr = Box::into_raw(Box::new(JSNull {
      base: JSObject {
        ref_cnt: 1,
        typ: JSObjectType::Null,
      },
    }));
    JS_NULL = ptr as JSObjPtr;

    let ptr = Box::into_raw(Box::new(JSUndefined {
      base: JSObject {
        ref_cnt: 1,
        typ: JSObjectType::Undefined,
      },
    }));
    JS_UNDEF = ptr as JSObjPtr;
  });
}

impl JSObject {
  pub fn dump(ptr: JSObjPtr) {
    let obj = unsafe { Box::from_raw(ptr) };
    let typ = obj.typ;
    Box::into_raw(obj);
    match typ {
      JSObjectType::String => {
        let obj = unsafe { Box::from_raw(ptr as JSStrPtr) };
        println!("{:#?}", &obj);
        Box::into_raw(obj);
      }
      JSObjectType::Number => {
        let obj = unsafe { Box::from_raw(ptr as JSNumPtr) };
        println!("{:#?}", &obj);
        Box::into_raw(obj);
      }
      JSObjectType::Boolean => {
        let obj = unsafe { Box::from_raw(ptr as JSBoolPtr) };
        println!("{:#?}", &obj);
        Box::into_raw(obj);
      }
      JSObjectType::Array => {
        let obj = unsafe { Box::from_raw(ptr as JSArrPtr) };
        println!("{:#?}", &obj);
        Box::into_raw(obj);
      }
      JSObjectType::Dict => {
        let obj = unsafe { Box::from_raw(ptr as JSDictPtr) };
        println!("{:#?}", &obj);
        Box::into_raw(obj);
      }
      JSObjectType::Null => {
        let obj = unsafe { Box::from_raw(ptr as JSNullPtr) };
        println!("{:#?}", &obj);
        Box::into_raw(obj);
      }
      JSObjectType::Undefined => {
        let obj = unsafe { Box::from_raw(ptr as JSUndefPtr) };
        println!("{:#?}", &obj);
        Box::into_raw(obj);
      }
    }
  }
}

#[cfg(test)]
mod object_tests {
  use super::*;

  #[test]
  fn rc_test() {
    init_js_null_undef();

    let s_ptr = JSString::new("hello world");
    let arr_ptr = JSArray::new();
    JSArray::push(arr_ptr, s_ptr as JSObjPtr);

    JSObject::dump(s_ptr as JSObjPtr);
    assert_eq!(2, RefCounting::ref_cnt(s_ptr as JSObjPtr));

    let s_ptr = JSArray::pop(arr_ptr);
    JSObject::dump(s_ptr as JSObjPtr);
    assert_eq!(1, RefCounting::ref_cnt(s_ptr as JSObjPtr));

    JSArray::push(arr_ptr, s_ptr as JSObjPtr);
    assert_eq!(2, RefCounting::ref_cnt(s_ptr as JSObjPtr));

    RefCounting::dec(arr_ptr as JSObjPtr);
    assert_eq!(1, RefCounting::ref_cnt(s_ptr as JSObjPtr));
    JSObject::dump(s_ptr as JSObjPtr);
  }

  #[test]
  fn dict_test() {
    init_js_null_undef();

    let s_ptr = JSString::new("hello world");
    let dict_ptr = JSDict::new();
    JSDict::set(dict_ptr, "test", s_ptr as JSObjPtr);
    assert_eq!(2, RefCounting::ref_cnt(s_ptr as JSObjPtr));

    JSDict::get(dict_ptr, "test");
    assert_eq!(2, RefCounting::ref_cnt(s_ptr as JSObjPtr));

    JSDict::del(dict_ptr, "test");
    assert_eq!(1, RefCounting::ref_cnt(s_ptr as JSObjPtr));

    unsafe {
      assert_eq!(JS_UNDEF, JSDict::get(dict_ptr, "test"));
    }

    unsafe {
      assert_eq!(JS_UNDEF, JSDict::get(dict_ptr, "test"));
    }
  }
}
