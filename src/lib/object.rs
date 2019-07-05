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
  Nan,
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
pub type JSNanPtr = *mut JSNan;

pub fn box_it<T>(ptr: *mut T) -> Box<T> {
  unsafe { Box::from_raw(ptr) }
}

pub fn box_as<F, T>(ptr: *mut F) -> Box<T> {
  unsafe { Box::from_raw(ptr as *mut T) }
}

pub fn unbox<T>(b: Box<T>) -> *mut T {
  Box::into_raw(b)
}

impl RefCounting {
  pub fn ref_cnt<T>(ptr: *mut T) -> i32 {
    let obj = box_as::<T, JSObject>(ptr);
    let rc = obj.ref_cnt;
    unbox(obj);
    rc
  }

  pub fn inc<T>(ptr: *mut T) {
    let mut obj = box_as::<T, JSObject>(ptr);
    obj.ref_cnt += 1;
    unbox(obj);
  }

  pub fn dec<T>(ptr: *mut T) {
    let mut obj = box_as::<T, JSObject>(ptr);
    obj.ref_cnt -= 1;
    RefCounting::x_release(unbox(obj));
  }

  pub fn x_release<T>(ptr: *mut T) {
    JSObject::dump(ptr);
    let rc = RefCounting::ref_cnt(ptr);
    assert!(rc >= 0);
    if rc > 0 {
      return;
    }

    let obj = box_as::<T, JSObject>(ptr);
    let typ = obj.typ;
    unbox(obj);
    match typ {
      JSObjectType::String => {
        let s = box_as::<T, JSString>(ptr);
      }
      JSObjectType::Number => {
        let n = box_as::<T, JSNumber>(ptr);
      }
      JSObjectType::Boolean => {
        let b = box_as::<T, JSBoolean>(ptr);
      }
      JSObjectType::Array => {
        let arr = box_as::<T, JSArray>(ptr);
        for item in &arr.a {
          RefCounting::dec(*item);
        }
      }
      JSObjectType::Dict => {
        let dict = box_as::<T, JSDict>(ptr);
        for (_, v) in &dict.d {
          RefCounting::dec(*v);
        }
      }
      _ => (),
    };
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

#[repr(C)]
#[derive(Debug)]
pub struct JSNan {
  pub base: JSObject,
}

pub static mut JS_NULL: JSObjPtr = null_mut();
pub static mut JS_UNDEF: JSObjPtr = null_mut();
pub static mut JS_NAN: JSObjPtr = null_mut();

pub fn js_null() -> JSObjPtr {
  unsafe { JS_NULL }
}

pub fn js_undef() -> JSObjPtr {
  unsafe { JS_UNDEF }
}

pub fn js_nan() -> JSObjPtr {
  unsafe { JS_NAN }
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

    let ptr = Box::into_raw(Box::new(JSNan {
      base: JSObject {
        ref_cnt: 1,
        typ: JSObjectType::Nan,
      },
    }));
    JS_NAN = ptr as JSObjPtr;
  });
}

impl JSObject {
  pub fn dump<T>(ptr: *mut T) {
    let obj = box_as::<T, JSObject>(ptr);
    let typ = obj.typ;
    unbox(obj);
    match typ {
      JSObjectType::String => {
        let obj = box_as::<T, JSString>(ptr);
        println!("{:#?}", &obj);
        unbox(obj);
      }
      JSObjectType::Number => {
        let obj = box_as::<T, JSNumber>(ptr);
        println!("{:#?}", &obj);
        unbox(obj);
      }
      JSObjectType::Boolean => {
        let obj = box_as::<T, JSBoolean>(ptr);
        println!("{:#?}", &obj);
        unbox(obj);
      }
      JSObjectType::Array => {
        let obj = box_as::<T, JSArray>(ptr);
        println!("{:#?}", &obj);
        unbox(obj);
      }
      JSObjectType::Dict => {
        let obj = box_as::<T, JSDict>(ptr);
        println!("{:#?}", &obj);
        unbox(obj);
      }
      JSObjectType::Null => {
        let obj = box_as::<T, JSNull>(ptr);
        println!("{:#?}", &obj);
        unbox(obj);
      }
      JSObjectType::Undefined => {
        let obj = box_as::<T, JSUndefined>(ptr);
        println!("{:#?}", &obj);
        unbox(obj);
      }
      JSObjectType::Nan => {
        let obj = box_as::<T, JSNan>(ptr);
        println!("{:#?}", &obj);
        unbox(obj);
      }
    }
  }
}
