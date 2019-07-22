use crate::vm::gc::*;

impl JsDict {
  pub fn set(&mut self, k: JsObjPtr, v: JsObjPtr) {
    let k = as_str(k).d.as_str();
    as_obj(v).inc();
    match self.d.get(k) {
      Some(old) => as_obj(*old).dec(),
      _ => (),
    }
    self.d.insert(k.to_owned(), v);
  }

  pub fn get(&mut self, k: JsObjPtr) -> JsObjPtr {
    let gc = as_gc(as_obj(self).gc());
    let k = as_str(k).d.as_str();
    match self.d.get(k) {
      Some(v) => *v,
      None => gc.js_undef(),
    }
  }
}
