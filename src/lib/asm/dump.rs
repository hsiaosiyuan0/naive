extern crate byteorder;

use crate::asm::chunk::*;
use byteorder::{BigEndian, WriteBytesExt};
use std::io::Write;
use std::mem;

pub struct Dumper {
  buf: Vec<u8>,
  chk: Chunk,
}

impl Dumper {
  pub fn w_byte(&mut self, x: u8) {
    self.buf.write(&[x]).ok();
  }

  pub fn w_i64(&mut self, x: i64) {
    self.buf.write(&x.to_be_bytes()).ok();
  }

  pub fn w_u64(&mut self, x: u64) {
    self.buf.write(&x.to_be_bytes()).ok();
  }

  pub fn w_u32(&mut self, x: u32) {
    self.buf.write(&x.to_be_bytes()).ok();
  }

  pub fn w_size(&mut self, x: u64) {
    self.buf.write(&x.to_be_bytes()).ok();
  }

  pub fn w_double(&mut self, x: f64) {
    let mut buf = [0u8; mem::size_of::<f64>()];
    buf.as_mut().write_f64::<BigEndian>(x).ok();
    self.buf.write(&buf).ok();
  }

  pub fn w_string(&mut self, s: &str) {
    self.w_size(s.len() as u64);
    self.buf.write(s.as_bytes()).ok();
  }

  pub fn process(&mut self) {
    self.w_header();
    self.w_byte(self.chk.upval_cnt);
    let ptr = &self.chk.top_fun_tpl as *const FunTpl;
    unsafe { self.w_fun_tpl(&(*ptr)) }
  }

  fn w_header(&mut self) {
    self.w_string(self.chk.sig);
    self.w_u64(self.chk.ver);
  }

  fn w_const(&mut self, c: &Const) {
    self.w_byte(c.typ_id());
    match c {
      Const::String(v) => self.w_string(v.as_str()),
      Const::Number(v) => self.w_double(*v),
    }
  }

  fn w_upval(&mut self, u: &Upval) {
    self.w_byte(u.in_stack as u8);
    self.w_u32(u.idx);
  }

  fn w_fun_tpl(&mut self, f: &FunTpl) {
    self.w_byte(f.param_cnt);
    self.w_byte(f.is_vararg as u8);
    self.w_code(&f.code);
    self.w_consts(&f.consts);
    self.w_upvals(&f.upvals);
    self.w_fun_tpls(&f.fun_tpls);
  }

  fn w_code(&mut self, insts: &Vec<Inst>) {
    self.w_u64(insts.len() as u64);
    insts.iter().for_each(|inst| self.w_u32(inst.raw));
  }

  fn w_consts(&mut self, cs: &Vec<Const>) {
    self.w_u64(cs.len() as u64);
    cs.iter().for_each(|c| self.w_const(c));
  }

  fn w_upvals(&mut self, uvs: &Vec<Upval>) {
    self.w_u64(uvs.len() as u64);
    uvs.iter().for_each(|uv| self.w_upval(uv));
  }

  fn w_fun_tpls(&mut self, tpls: &Vec<FunTpl>) {
    self.w_u64(tpls.len() as u64);
    tpls.iter().for_each(|tpl| self.w_fun_tpl(tpl));
  }
}
