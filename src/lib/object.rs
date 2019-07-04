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
  pub fn ref_cnt<T>(ptr: *mut T) -> i32 {
    let ptr = ptr as JSObjPtr;
    let obj = unsafe { Box::from_raw(ptr) };
    let rc = obj.ref_cnt;
    Box::into_raw(obj);
    rc
  }

  pub fn inc<T>(ptr: *mut T) {
    let ptr = ptr as JSObjPtr;
    let mut obj = unsafe { Box::from_raw(ptr) };
    obj.ref_cnt += 1;
    Box::into_raw(obj);
  }

  pub fn dec<T>(ptr: *mut T) {
    let ptr = ptr as JSObjPtr;
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
    JSObject::dump(ptr);
    unsafe { drop(Box::from_raw(ptr)) }
  }
}

#[repr(C)]
#[derive(Debug)]
pub struct JSObject {
  pub ref_cnt: i32,
  pub typ: JSObjectType,
}

#[repr(C)]
#[derive(Debug)]
pub struct JSNumber {
  pub base: JSObject,
  pub f: f64,
  pub i: bool,
}

#[repr(C)]
#[derive(Debug)]
pub struct JSString {
  pub base: JSObject,
  pub s: String,
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

#[repr(C)]
#[derive(Debug)]
pub struct JSDict {
  pub base: JSObject,
  pub d: HashMap<String, JSObjPtr>,
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
  pub fn dump<T>(ptr: *mut T) {
    let ptr = ptr as JSObjPtr;
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
