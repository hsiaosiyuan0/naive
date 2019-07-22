use crate::vm::gc::*;
use std::f64;

impl GcObj {
  pub fn eqs_true(&mut self) -> bool {
    let gc = as_gc(self.gc());
    as_obj_ptr(self) == gc.js_true()
  }

  pub fn eqs_false(&mut self) -> bool {
    let gc = as_gc(self.gc());
    as_obj_ptr(self) == gc.js_false()
  }

  pub fn t_pri(&mut self) -> JsObjPtr {
    as_obj_ptr(self)
  }

  pub fn t_num(&mut self) -> JsNumPtr {
    let gc = as_gc(self.gc());
    match self.kind {
      GcObjKind::Undef => {
        let n = gc.new_num(false);
        as_num(n).d = f64::NAN;
        n
      }
      GcObjKind::Null => gc.new_num(false),
      GcObjKind::Boolean => {
        let n = gc.new_num(false);
        let is_true = as_obj_ptr(self) == gc.js_true();
        as_num(n).d = if is_true { 1.0 } else { 0.0 };
        n
      }
      GcObjKind::Number => {
        self.inc();
        as_obj_ptr(self) as JsNumPtr
      }
      GcObjKind::String => {
        let s = as_str(self);
        let f: f64 = s.d.parse().ok().unwrap();
        let n = gc.new_num(false);
        as_num(n).d = f;
        n
      }
      _ => as_obj(self.t_pri()).t_num(),
    }
  }

  pub fn t_bool(&mut self) -> JsObjPtr {
    let gc = as_gc(self.gc());
    match self.kind {
      GcObjKind::Undef => gc.js_false(),
      GcObjKind::Null => gc.js_false(),
      GcObjKind::Boolean => as_obj_ptr(self),
      GcObjKind::Number => {
        let n = as_num(self);
        if n.d == 0.0 || n.d.is_nan() {
          return gc.js_false();
        }
        gc.js_true()
      }
      GcObjKind::String => {
        let s = as_str(self);
        if s.d.len() == 0 {
          return gc.js_false();
        }
        gc.js_true()
      }
      _ => gc.js_true(),
    }
  }
}
