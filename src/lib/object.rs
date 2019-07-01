#[repr(C)]
#[derive(Debug)]
pub struct JSObjType {}

#[repr(C)]
#[derive(Debug)]
pub struct JSObject {
  pub ref_cnt: i32,
  pub typ: JSObjType,
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
  pub str: String,
}

#[repr(C)]
#[derive(Debug)]
pub struct Person {
  pub age: u32,
  //  pub name: String,
}

#[cfg(test)]
mod object_tests {
  use super::*;
  use crate::source::*;
  use libc::*;
  use std::mem;
  use std::os::raw::c_void;

  #[test]
  fn malloc_expr() {
    let sz = mem::size_of::<Person>();
    println!("{:#?}", sz);

    unsafe {
      let p = malloc(sz) as *mut Person;
      (*p).age = 1;
      //      (*p).name = "tom".to_string();
      println!("{:#?}", *p);
      // should I explicitly free the memory used by `name` before
      // free the outer object pointer `p`? In other words, will rust
      // automatically generate some code for freeing `name`?
      free(p as *mut c_void);
    }
  }
}
