use crate::vm::gc::*;
use std::f64;
use std::ptr::eq;

impl GcObj {
  pub fn pass_by_value(&self) -> bool {
    match self.kind {
      GcObjKind::String | GcObjKind::Number => true,
      _ => false,
    }
  }

  pub fn x_pass_by_value(&mut self) -> JsObjPtr {
    let gc = as_gc(self.gc());
    match self.kind {
      GcObjKind::String => {
        let v = gc.new_str(false);
        as_str(v).d = as_str(self).d.clone();
        as_obj_ptr(v)
      }
      GcObjKind::Number => {
        let v = gc.new_num(false);
        as_num(v).d = as_num(self).d;
        as_obj_ptr(v)
      }
      _ => panic!(),
    }
  }

  pub fn eqs_true(&mut self) -> bool {
    self.kind == GcObjKind::Boolean && as_bool(self).d
  }

  pub fn eqs_false(&mut self) -> bool {
    self.kind == GcObjKind::Boolean && !as_bool(self).d
  }

  pub fn t_pri(&mut self) -> JsObjPtr {
    // TODO:: call [[DefaultValue]] internal method
    as_obj(self).inc();
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
        let is_true = as_obj(self).eqs_true();
        as_num(n).d = if is_true { 1.0 } else { 0.0 };
        n
      }
      GcObjKind::Number => {
        self.inc();
        as_obj_ptr(self) as JsNumPtr
      }
      GcObjKind::String => {
        let n = gc.new_num(false);
        as_num(n).set_v_str(as_str(self));
        n
      }
      _ => as_obj(self.t_pri()).t_num(),
    }
  }

  pub fn t_bool(&mut self) -> JsBoolPtr {
    let gc = as_gc(self.gc());
    match self.kind {
      GcObjKind::Undef => gc.js_false(),
      GcObjKind::Null => gc.js_false(),
      GcObjKind::Boolean => as_obj_ptr(self) as JsBoolPtr,
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

  pub fn is_compound(&self) -> bool {
    match self.kind {
      GcObjKind::String
      | GcObjKind::Number
      | GcObjKind::Boolean
      | GcObjKind::Null
      | GcObjKind::Undef => false,
      _ => true,
    }
  }

  pub fn eq(a: JsObjPtr, b: JsObjPtr) -> bool {
    let gc = as_gc(as_obj(a).gc());
    let mut local_scope = LocalScope::new();
    let ta = as_obj(a).kind;
    let tb = as_obj(b).kind;

    if ta == tb {
      if ta == GcObjKind::Undef {
        return true;
      } else if ta == GcObjKind::Null {
        return true;
      } else if ta == GcObjKind::Number {
        let av = as_num(a).d;
        let bv = as_num(b).d;
        if av.is_nan() {
          return false;
        }
        if bv.is_nan() {
          return false;
        }
        return av == bv;
      } else if ta == GcObjKind::String {
        let av = &as_str(a).d;
        let bv = &as_str(b).d;
        return av == bv;
      } else if ta == GcObjKind::Boolean {
        return a == b;
      }
    } else {
      if ta == GcObjKind::Undef && tb == GcObjKind::Null
        || ta == GcObjKind::Null && tb == GcObjKind::Undef
      {
        return true;
      }
      if ta == GcObjKind::Number && tb == GcObjKind::String {
        let nb = gc.new_num(false);
        local_scope.reg(nb);
        as_num(nb).set_v_str(as_str(b));
        return as_num(a).eq(nb);
      }
      if ta == GcObjKind::String && tb == GcObjKind::Number {
        let na = gc.new_num(false);
        local_scope.reg(na);
        as_num(na).set_v_str(as_str(a));
        return as_num(b).eq(na);
      }
      if ta == GcObjKind::Boolean {
        let na = gc.new_num(false);
        local_scope.reg(na);
        as_num(na).set_v_bool(a);
        return GcObj::eq(as_obj_ptr(na), b);
      }
      if tb == GcObjKind::Boolean {
        let nb = gc.new_num(false);
        local_scope.reg(nb);
        as_num(nb).set_v_bool(b);
        return GcObj::eq(a, as_obj_ptr(nb));
      }
      if (ta == GcObjKind::Number || ta == GcObjKind::String) && as_obj(b).is_compound() {
        let pri = as_obj(b).t_pri();
        local_scope.reg(pri);
        return GcObj::eq(a, pri);
      }
      if (tb == GcObjKind::Number || tb == GcObjKind::String) && as_obj(a).is_compound() {
        let pri = as_obj(a).t_pri();
        local_scope.reg(pri);
        return GcObj::eq(b, pri);
      }
    }
    return false;
  }

  pub fn lt(a: JsObjPtr, b: JsObjPtr) -> bool {
    let mut local_scope = LocalScope::new();
    let gc = as_gc(as_obj(a).gc());

    let pa = as_obj(a).t_pri();
    local_scope.reg(a);

    let pb = as_obj(b).t_pri();
    local_scope.reg(b);

    let ta = as_obj(pa).kind;
    let tb = as_obj(pb).kind;

    if !(ta == GcObjKind::String && tb == GcObjKind::String) {
      let na = as_obj(pa).t_num();
      local_scope.reg(na);
      let nb = as_obj(pb).t_num();
      local_scope.reg(nb);

      // js spec says that if `na` above(`nx` in spec) is NAN then just return `undefined`
      // I probably understand why, since compare NAN meaningless so return `undefined` looks sound,
      // but I think just return `false` is only the intuitive way, here we just on the basis of
      // what the CLua does - just return `false` to keep `LE` to return only one type value
      if as_num(na).d.is_nan() {
        // return gc.js_undef();
        return false;
      }
      if as_num(nb).d.is_nan() {
        // return gc.js_undef();
        return false;
      }
      if as_num(na).d == as_num(nb).d {
        return false;
      }
      if as_num(na).d < as_num(nb).d {
        return true;
      }
      return false;
    }

    let sa = as_str(pa);
    let sb = as_str(pb);
    if sa.d.starts_with(sb.d.as_str()) {
      return false;
    }
    if sb.d.starts_with(sa.d.as_str()) {
      return true;
    }

    let cbs = sb.d.chars().collect::<Vec<_>>();
    for ca in sa.d.chars().enumerate() {
      let cb = *cbs.get(ca.0).unwrap();
      if ca.1 < cb {
        return true;
      } else if ca.1 > cb {
        return false;
      }
    }
    return false;
  }

  pub fn le(a: JsObjPtr, b: JsObjPtr) -> bool {
    GcObj::lt(a, b) || GcObj::eq(a, b)
  }
}
