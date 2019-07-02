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

impl RefCounting {
  pub fn ref_cnt(ptr: *mut JSObject) -> i32 {
    let obj = unsafe { Box::from_raw(ptr) };
    let rc = obj.ref_cnt;
    Box::into_raw(obj);
    rc
  }

  pub fn inc(ptr: *mut JSObject) {
    let mut obj = unsafe { Box::from_raw(ptr) };
    obj.ref_cnt += 1;
    Box::into_raw(obj);
  }

  pub fn dec(ptr: *mut JSObject) {
    let mut obj = unsafe { Box::from_raw(ptr) };
    obj.ref_cnt -= 1;
    RefCounting::x_release(Box::into_raw(obj));
  }

  pub fn x_release(ptr: *mut JSObject) {
    let rc = RefCounting::ref_cnt(ptr);
    assert!(rc >= 0);
    if rc == 0 {
      let obj = unsafe { Box::from_raw(ptr) };
      let typ = obj.typ;
      Box::into_raw(obj);
      match typ {
        JSObjectType::Array => {
          let arr = unsafe { Box::from_raw(ptr as *mut JSArray) };
          for item in &arr.a {
            RefCounting::dec(*item)
          }
          Box::into_raw(arr);
        }
        JSObjectType::Dict => {
          let dict = unsafe { Box::from_raw(ptr as *mut JSDict) };
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
}

#[repr(C)]
#[derive(Debug)]
pub struct JSObject {
  pub ref_cnt: i32,
  pub typ: JSObjectType,
}

impl JSObject {
  pub fn is_type(ptr: *mut JSObject, typ: JSObjectType) -> bool {
    let obj = unsafe { Box::from_raw(ptr) };
    let t = obj.typ;
    Box::into_raw(obj);
    t == typ
  }

  pub fn is_str(ptr: *mut JSObject) -> bool {
    JSObject::is_type(ptr, JSObjectType::String)
  }
}

#[repr(C)]
#[derive(Debug)]
pub struct JSNumber {
  pub base: JSObject,
  pub v: f64,
}

#[repr(C)]
#[derive(Debug)]
pub struct JSString {
  pub base: JSObject,
  pub s: String,
}

impl JSString {
  pub fn new(s: &str) -> *mut JSString {
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
  pub fn new() -> *mut JSArray {
    Box::into_raw(Box::new(JSArray {
      base: JSObject {
        ref_cnt: 1,
        typ: JSObjectType::Array,
      },
      a: vec![],
    }))
  }

  pub fn push(arr: *mut JSArray, item: *mut JSObject) {
    let mut arr = unsafe { Box::from_raw(arr) };
    arr.a.push(item);
    RefCounting::inc(item);
    Box::into_raw(arr);
  }

  pub fn pop(arr: *mut JSArray) -> *mut JSObject {
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
  pub d: HashMap<String, *mut JSObject>,
}

impl JSDict {
  pub fn new() -> *mut JSDict {
    Box::into_raw(Box::new(JSDict {
      base: JSObject {
        ref_cnt: 1,
        typ: JSObjectType::Dict,
      },
      d: HashMap::new(),
    }))
  }

  pub fn set(map: *mut JSDict, k: &str, v: *mut JSObject) {
    let mut dict = unsafe { Box::from_raw(map) };
    dict.d.insert(k.to_owned(), v);
    RefCounting::inc(v);
    Box::into_raw(dict);
  }

  pub fn get(map: *mut JSDict, k: &str) -> *mut JSObject {
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

  pub fn del(map: *mut JSDict, k: &str) -> *mut JSObject {
    let v = JSDict::get(map, k);
    unsafe {
      if v != JS_UNDEF {
        RefCounting::dec(v);
      }
    }
    v
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

pub static mut JS_NULL: *mut JSObject = null_mut();
pub static mut JS_UNDEF: *mut JSObject = null_mut();

static INIT_JS_NULL_UNDEF_ONCE: Once = Once::new();
pub fn init_js_null_undef() {
  INIT_JS_NULL_UNDEF_ONCE.call_once(|| unsafe {
    let ptr = Box::into_raw(Box::new(JSNull {
      base: JSObject {
        ref_cnt: 1,
        typ: JSObjectType::Null,
      },
    }));
    JS_NULL = ptr as *mut JSObject;

    let ptr = Box::into_raw(Box::new(JSUndefined {
      base: JSObject {
        ref_cnt: 1,
        typ: JSObjectType::Undefined,
      },
    }));
    JS_UNDEF = ptr as *mut JSObject;
  });
}

impl JSObject {
  pub fn dump(ptr: *mut JSObject) {
    let obj = unsafe { Box::from_raw(ptr) };
    let typ = obj.typ;
    Box::into_raw(obj);
    match typ {
      JSObjectType::String => {
        let obj = unsafe { Box::from_raw(ptr as *mut JSString) };
        println!("{:#?}", &obj);
        Box::into_raw(obj);
      }
      JSObjectType::Number => {
        let obj = unsafe { Box::from_raw(ptr as *mut JSNumber) };
        println!("{:#?}", &obj);
        Box::into_raw(obj);
      }
      JSObjectType::Boolean => {
        let obj = unsafe { Box::from_raw(ptr as *mut JSBoolean) };
        println!("{:#?}", &obj);
        Box::into_raw(obj);
      }
      JSObjectType::Array => {
        let obj = unsafe { Box::from_raw(ptr as *mut JSArray) };
        println!("{:#?}", &obj);
        Box::into_raw(obj);
      }
      JSObjectType::Dict => {
        let obj = unsafe { Box::from_raw(ptr as *mut JSDict) };
        println!("{:#?}", &obj);
        Box::into_raw(obj);
      }
      JSObjectType::Null => {
        let obj = unsafe { Box::from_raw(ptr as *mut JSDict) };
        println!("{:#?}", &obj);
        Box::into_raw(obj);
      }
      JSObjectType::Undefined => {
        let obj = unsafe { Box::from_raw(ptr as *mut JSObject) };
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
    JSArray::push(arr_ptr, s_ptr as *mut JSObject);

    JSObject::dump(s_ptr as *mut JSObject);
    assert_eq!(2, RefCounting::ref_cnt(s_ptr as *mut JSObject));

    let s_ptr = JSArray::pop(arr_ptr);
    JSObject::dump(s_ptr as *mut JSObject);
    assert_eq!(1, RefCounting::ref_cnt(s_ptr as *mut JSObject));

    JSArray::push(arr_ptr, s_ptr as *mut JSObject);
    assert_eq!(2, RefCounting::ref_cnt(s_ptr as *mut JSObject));

    RefCounting::dec(arr_ptr as *mut JSObject);
    assert_eq!(1, RefCounting::ref_cnt(s_ptr as *mut JSObject));
    JSObject::dump(s_ptr as *mut JSObject);
  }

  #[test]
  fn dict_test() {
    init_js_null_undef();

    let s_ptr = JSString::new("hello world");
    let dict_ptr = JSDict::new();
    JSDict::set(dict_ptr, "test", s_ptr as *mut JSObject);
    assert_eq!(2, RefCounting::ref_cnt(s_ptr as *mut JSObject));

    JSDict::get(dict_ptr, "test");
    assert_eq!(2, RefCounting::ref_cnt(s_ptr as *mut JSObject));

    JSDict::del(dict_ptr, "test");
    assert_eq!(1, RefCounting::ref_cnt(s_ptr as *mut JSObject));
  }
}
