use crate::vm::gc::*;

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
}
