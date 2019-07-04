use crate::ast::*;
use crate::lexer::*;
use crate::object::*;
use crate::parser::*;
use crate::token::*;
use crate::visitor::AstVisitor;
use std::collections::{HashMap, HashSet};
use std::ops::Deref;
use std::ptr::{drop_in_place, null_mut};

pub struct Interpreter<'a> {
  ctx: &'a mut Context,
}

pub struct RuntimeError {}

impl<'a> Interpreter<'a> {
  fn local_scope(&self) -> LocalScopePtr {
    self.ctx.local_scope
  }
}

impl<'a> AstVisitor<Option<JSObjPtr>, RuntimeError> for Interpreter<'a> {
  fn prog(&mut self, prog: &Prog) -> Result<Option<JSObjPtr>, RuntimeError> {
    let scope = LocalScope::new(&mut self.ctx);
    let mut ret = Ok(None);
    for stmt in &prog.body {
      ret = match self.stmt(stmt) {
        Err(e) => return Err(e),
        Ok(v) => match v {
          Some(v) => {
            scope.close(v);
            Ok(Some(v))
          }
          _ => Ok(None),
        },
      }
    }
    ret
  }

  fn block_stmt(&mut self, stmt: &BlockStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn var_dec_stmt(&mut self, stmt: &VarDec) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn empty_stmt(&mut self, stmt: &EmptyStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn expr_stmt(&mut self, stmt: &ExprStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    let scope = LocalScope::new(self.ctx);
    match self.expr(&stmt.expr) {
      Ok(v) => match v {
        Some(v) => {
          scope.close(v);
          Ok(Some(v))
        }
        None => Ok(None),
      },
      Err(e) => return Err(e),
    }
  }

  fn if_stmt(&mut self, stmt: &IfStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn for_stmt(&mut self, stmt: &ForStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn for_in_stmt(&mut self, stmt: &ForInStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn do_while_stmt(&mut self, stmt: &DoWhileStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn while_stmt(&mut self, stmt: &WhileStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn cont_stmt(&mut self, stmt: &ContStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn break_stmt(&mut self, stmt: &BreakStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn ret_stmt(&mut self, stmt: &ReturnStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn with_stmt(&mut self, stmt: &WithStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn switch_stmt(&mut self, stmt: &SwitchStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn throw_stmt(&mut self, stmt: &ThrowStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn try_stmt(&mut self, stmt: &TryStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn debug_stmt(&mut self, stmt: &DebugStmt) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn fn_stmt(&mut self, stmt: &FnDec) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn member_expr(&mut self, expr: &MemberExpr) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn new_expr(&mut self, expr: &NewExpr) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn call_expr(&mut self, expr: &CallExpr) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn unary_expr(&mut self, expr: &UnaryExpr) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn binary_expr(&mut self, expr: &BinaryExpr) -> Result<Option<JSObjPtr>, RuntimeError> {
    let lhs = match self.expr(&expr.left) {
      Ok(v) => v.unwrap(),
      Err(e) => return Err(e),
    };
    let rhs = match self.expr(&expr.right) {
      Ok(v) => v.unwrap(),
      Err(e) => return Err(e),
    };
    let op = expr.op.symbol_data();
    match op.kind {
      Symbol::Add => Ok(Some(JSObject::add(lhs, rhs, self.ctx.local_scope))),
      Symbol::Mul => Ok(Some(JSObject::mul(lhs, rhs, self.ctx.local_scope))),
      _ => Ok(Some(js_undef())),
    }
    //    Ok(None)
  }

  fn assign_expr(&mut self, expr: &AssignExpr) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn cond_expr(&mut self, expr: &CondExpr) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn seq_expr(&mut self, expr: &SeqExpr) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn this_expr(&mut self, expr: &ThisExprData) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn id_expr(&mut self, expr: &IdData) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn array_literal(&mut self, expr: &ArrayData) -> Result<Option<JSObjPtr>, RuntimeError> {
    let arr = JSArray::new(self.local_scope());
    let scope = LocalScope::new(self.ctx);
    for v_expr in &expr.value {
      match self.expr(v_expr) {
        Ok(v) => {
          let v = v.unwrap();
          JSArray::push(arr, v);
        }
        Err(e) => return Err(e),
      }
    }
    Ok(Some(arr as JSObjPtr))
  }

  fn object_literal(&mut self, expr: &ObjectData) -> Result<Option<JSObjPtr>, RuntimeError> {
    let dict = JSDict::new(self.local_scope());
    let scope = LocalScope::new(self.ctx);
    unimplemented!()
  }

  fn paren_expr(&mut self, expr: &ParenData) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn fn_expr(&mut self, expr: &FnDec) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn regexp_expr(&mut self, expr: &RegExpData) -> Result<Option<JSObjPtr>, RuntimeError> {
    unimplemented!()
  }

  fn null_expr(&mut self, expr: &NullData) -> Result<Option<JSObjPtr>, RuntimeError> {
    Ok(Some(js_null()))
  }

  fn str_expr(&mut self, expr: &StringData) -> Result<Option<JSObjPtr>, RuntimeError> {
    let v = JSString::new(expr.value.as_str(), self.local_scope());
    Ok(Some(v as JSObjPtr))
  }

  fn bool_expr(&mut self, expr: &BoolData) -> Result<Option<JSObjPtr>, RuntimeError> {
    let v = JSBoolean::new(expr.value, self.local_scope());
    Ok(Some(v as JSObjPtr))
  }

  fn num_expr(&mut self, expr: &NumericData) -> Result<Option<JSObjPtr>, RuntimeError> {
    let v = JSNumber::from_str(&expr.value, self.local_scope());
    Ok(Some(v as JSObjPtr))
  }
}

pub struct Env {
  outer: Option<Box<Env>>,
  inner: Vec<Env>,
  upvals: Vec<String>,
  maps: HashMap<String, JSObjPtr>,
}

pub type LocalScopePtr = *mut LocalScope;
pub type ContextPtr = *mut Context;

pub struct Context {
  local_scope: LocalScopePtr,
  scope: Box<LocalScope>,
}

impl Context {
  pub fn new() -> Box<Context> {
    let mut scope = Box::new(LocalScope {
      ctx: null_mut(),
      outer: null_mut(),
      vals: HashSet::new(),
    });
    let scope_ptr = Box::into_raw(scope);
    scope = unsafe { Box::from_raw(scope_ptr) };

    let mut ctx = Box::new(Context {
      local_scope: null_mut(),
      scope,
    });
    let ctx_ptr = Box::into_raw(ctx);
    ctx = unsafe { Box::from_raw(ctx_ptr) };

    scope = unsafe { Box::from_raw(scope_ptr) };
    scope.ctx = ctx_ptr;
    Box::into_raw(scope);

    ctx.local_scope = scope_ptr;
    ctx
  }
}

pub struct LocalScope {
  ctx: ContextPtr,
  outer: LocalScopePtr,
  vals: HashSet<JSObjPtr>,
}

impl LocalScope {
  pub fn new(ctx: &mut Context) -> Box<LocalScope> {
    let pool = Box::new(LocalScope {
      ctx: ctx as ContextPtr,
      outer: ctx.local_scope,
      vals: HashSet::new(),
    });
    ctx.local_scope = Box::into_raw(pool);
    unsafe { Box::from_raw(ctx.local_scope) }
  }

  pub fn reg<T>(scope: LocalScopePtr, val: *mut T) -> *mut T {
    let mut scope = unsafe { Box::from_raw(scope) };
    let v = val as JSObjPtr;
    scope.vals.insert(v);
    Box::into_raw(scope);
    val
  }

  pub fn del<T>(scope: LocalScopePtr, val: *mut T) {
    let mut scope = unsafe { Box::from_raw(scope) };
    let v = val as JSObjPtr;
    scope.vals.remove(&v);
    Box::into_raw(scope);
  }

  pub fn close(&self, v: JSObjPtr) {
    RefCounting::inc(v);
    LocalScope::reg(self.outer, v);
  }
}

impl Drop for LocalScope {
  fn drop(&mut self) {
    let mut ctx = unsafe { Box::from_raw(self.ctx) };
    ctx.local_scope = self.outer;
    Box::into_raw(ctx);
    for val in &self.vals {
      RefCounting::dec(*val);
    }
    self.vals.clear();
  }
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

  pub fn add(lhs: JSObjPtr, rhs: JSObjPtr, scope: LocalScopePtr) -> JSObjPtr {
    let lv = JSNumber::val(lhs as JSNumPtr);
    let rv = JSNumber::val(rhs as JSNumPtr);
    JSNumber::from_f64(lv + rv, scope) as JSObjPtr
  }

  pub fn mul(lhs: JSObjPtr, rhs: JSObjPtr, scope: LocalScopePtr) -> JSObjPtr {
    let lv = JSNumber::val(lhs as JSNumPtr);
    let rv = JSNumber::val(rhs as JSNumPtr);
    JSNumber::from_f64(lv * rv, scope) as JSObjPtr
  }
}

impl JSString {
  pub fn new(s: &str, scope: LocalScopePtr) -> JSStrPtr {
    let ptr = Box::into_raw(Box::new(JSString {
      base: JSObject {
        ref_cnt: 1,
        typ: JSObjectType::String,
      },
      s: String::from(s),
    }));
    LocalScope::reg(scope, ptr)
  }
}

impl JSNumber {
  pub fn is_int(f: f64) -> bool {
    f.floor() == f
  }

  pub fn from_f64(v: f64, scope: LocalScopePtr) -> JSNumPtr {
    let (i, f) = match JSNumber::is_int(v) {
      true => (true, v),
      false => (false, v),
    };
    let ptr = Box::into_raw(Box::new(JSNumber {
      base: JSObject {
        ref_cnt: 1,
        typ: JSObjectType::Number,
      },
      i,
      f,
    }));
    LocalScope::reg(scope, ptr)
  }

  pub fn from_str(s: &str, scope: LocalScopePtr) -> JSNumPtr {
    let v: f64 = s.parse().unwrap();
    JSNumber::from_f64(v, scope)
  }

  pub fn val(n: JSNumPtr) -> f64 {
    let n = unsafe { Box::from_raw(n) };
    let v = n.f;
    Box::into_raw(n);
    v
  }

  pub fn eq(a: JSNumPtr, b: JSNumPtr) -> bool {
    let a = JSNumber::val(a);
    let b = JSNumber::val(b);
    a == b
  }
}

impl JSBoolean {
  pub fn new(v: bool, scope: LocalScopePtr) -> JSBoolPtr {
    let ptr = Box::into_raw(Box::new(JSBoolean {
      base: JSObject {
        ref_cnt: 1,
        typ: JSObjectType::Boolean,
      },
      v,
    }));
    LocalScope::reg(scope, ptr)
  }

  pub fn from_str(s: &str, scope: LocalScopePtr) -> JSBoolPtr {
    JSBoolean::new(s == "true", scope)
  }
}

impl JSArray {
  pub fn new(scope: LocalScopePtr) -> JSArrPtr {
    let ptr = Box::into_raw(Box::new(JSArray {
      base: JSObject {
        ref_cnt: 1,
        typ: JSObjectType::Array,
      },
      a: vec![],
    }));
    LocalScope::reg(scope, ptr)
  }

  pub fn push<T>(arr: JSArrPtr, item: *mut T) {
    let item = item as JSObjPtr;
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

  pub fn len(arr: JSArrPtr) -> usize {
    let mut arr = unsafe { Box::from_raw(arr) };
    let ret = arr.a.len();
    Box::into_raw(arr);
    ret
  }

  pub fn idx(arr: JSArrPtr, i: usize) -> JSObjPtr {
    let mut arr = unsafe { Box::from_raw(arr) };
    let v = match arr.a.get(i) {
      Some(ptr) => *ptr,
      None => null_mut(),
    };
    Box::into_raw(arr);
    v
  }
}

impl JSDict {
  pub fn new(scope: LocalScopePtr) -> JSDictPtr {
    let ptr = Box::into_raw(Box::new(JSDict {
      base: JSObject {
        ref_cnt: 1,
        typ: JSObjectType::Dict,
      },
      d: HashMap::new(),
    }));
    LocalScope::reg(scope, ptr)
  }

  pub fn set<T>(map: JSDictPtr, k: &str, v: *mut T) {
    let mut dict = unsafe { Box::from_raw(map) };
    let ptr = v as JSObjPtr;
    dict.d.insert(k.to_owned(), ptr);
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

#[cfg(test)]
mod interp_tests {
  use super::*;
  use crate::source::*;

  #[test]
  fn rc_test() {
    init_js_null_undef();

    let mut ctx = Context::new();
    let s_ptr = JSString::new("hello world", ctx.local_scope);
    let arr_ptr = JSArray::new(ctx.local_scope);
    JSArray::push(arr_ptr, s_ptr);

    JSObject::dump(s_ptr);
    assert_eq!(2, RefCounting::ref_cnt(s_ptr));

    let s_ptr = JSArray::pop(arr_ptr);
    JSObject::dump(s_ptr);
    assert_eq!(1, RefCounting::ref_cnt(s_ptr));

    JSArray::push(arr_ptr, s_ptr);
    assert_eq!(2, RefCounting::ref_cnt(s_ptr));

    LocalScope::del(ctx.local_scope, arr_ptr);
    RefCounting::dec(arr_ptr);
    assert_eq!(1, RefCounting::ref_cnt(s_ptr));
    JSObject::dump(s_ptr);
  }

  #[test]
  fn dict_test() {
    init_js_null_undef();

    let mut ctx = Context::new();
    let s_ptr = JSString::new("hello world", ctx.local_scope);
    let dict_ptr = JSDict::new(ctx.local_scope);
    JSDict::set(dict_ptr, "test", s_ptr);
    assert_eq!(2, RefCounting::ref_cnt(s_ptr));

    JSDict::get(dict_ptr, "test");
    assert_eq!(2, RefCounting::ref_cnt(s_ptr));

    JSDict::del(dict_ptr, "test");
    assert_eq!(1, RefCounting::ref_cnt(s_ptr));

    unsafe {
      assert_eq!(JS_UNDEF, JSDict::get(dict_ptr, "test"));
    }

    unsafe {
      assert_eq!(JS_UNDEF, JSDict::get(dict_ptr, "test"));
    }
  }

  #[test]
  fn exec_test() {
    init_token_data();

    let code = String::from("1.0 + 2.0");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);
    let mut ctx = Context::new();
    let mut interp = Interpreter { ctx: &mut ctx };
    let node = parser.prog().ok().unwrap();
    let res = interp.prog(&node).ok().unwrap().unwrap();
    JSObject::dump(res);
  }

  #[test]
  fn arr_test() {
    init_token_data();

    let code = String::from("[1, 1, 'hello']");
    let src = Source::new(&code);
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);
    let mut ctx = Context::new();
    let mut interp = Interpreter { ctx: &mut ctx };
    let node = parser.prog().ok().unwrap();
    let arr = interp.prog(&node).ok().unwrap().unwrap();
    JSObject::dump(arr);

    let arr_ptr = arr as JSArrPtr;
    assert!(JSNumber::eq(
      JSArray::idx(arr_ptr, 0) as JSNumPtr,
      JSArray::idx(arr_ptr, 1) as JSNumPtr
    ));
  }
}
