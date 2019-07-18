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

  pub fn get(&self, k: JsObjPtr) -> JsObjPtr {
    let k = as_str(k).d.as_str();
    match self.d.get(k) {
      Some(v) => *v,
      None => js_undef(),
    }
  }
}
