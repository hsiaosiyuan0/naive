use crate::vm::gc::*;
use std::f64;

impl JsNumber {
  pub fn add(&mut self, b: JsNumPtr) -> JsNumPtr {
    let gc = as_gc(as_obj(self).gc());
    let n = gc.new_num(false);
    let a = self.d;
    let b = as_num(b).d;
    as_num(n).d = a + b;
    n
  }

  pub fn sub(&mut self, b: JsNumPtr) -> JsNumPtr {
    let gc = as_gc(as_obj(self).gc());
    let n = gc.new_num(false);
    let a = self.d;
    let b = as_num(b).d;
    as_num(n).d = a - b;
    n
  }

  pub fn mul(&mut self, b: JsNumPtr) -> JsNumPtr {
    let gc = as_gc(as_obj(self).gc());
    let n = gc.new_num(false);
    let a = self.d;
    let b = as_num(b).d;
    as_num(n).d = a * b;
    n
  }

  pub fn div(&mut self, b: JsNumPtr) -> JsNumPtr {
    let gc = as_gc(as_obj(self).gc());
    let n = gc.new_num(false);
    let a = self.d;
    let b = as_num(b).d;
    as_num(n).d = a / b;
    n
  }

  pub fn modulo(&mut self, b: JsNumPtr) -> JsNumPtr {
    let gc = as_gc(as_obj(self).gc());
    let n = gc.new_num(false);
    let a = self.d;
    let b = as_num(b).d;
    as_num(n).d = a % b;
    n
  }

  pub fn eq(&mut self, b: JsNumPtr) -> bool {
    let a = self.d;
    let b = as_num(b).d;
    a == b
  }

  pub fn set_v_str(&mut self, b: JsStrPtr) {
    self.d = match as_str(b).d.parse().ok() {
      Some(v) => v,
      _ => f64::NAN,
    }
  }

  pub fn set_v_bool(&mut self, b: JsObjPtr) {
    let gc = as_gc(as_obj(self).gc());
    let b = b as JsBoolPtr;
    self.d = if b == gc.js_true() { 1.0 } else { 0.0 }
  }
}
