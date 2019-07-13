use std::collections::HashMap;
use std::fmt;
use std::intrinsics::transmute;
use std::sync::Once;

#[derive(Debug, Clone)]
pub enum Const {
  String(String),
  Number(f64),
}

impl Const {
  pub fn typ_id(&self) -> u8 {
    match self {
      Const::String(_) => 0,
      Const::Number(_) => 1,
    }
  }

  pub fn is_str(&self) -> bool {
    match self {
      Const::String(_) => true,
      _ => false,
    }
  }

  pub fn is_num(&self) -> bool {
    match self {
      Const::Number(_) => true,
      _ => false,
    }
  }

  pub fn num(&self) -> f64 {
    match self {
      Const::Number(v) => *v,
      _ => panic!(),
    }
  }

  pub fn str(&self) -> &str {
    match self {
      Const::String(v) => v.as_str(),
      _ => panic!(),
    }
  }

  pub fn eq(&self, c: &Const) -> bool {
    if self.typ_id() == c.typ_id() {
      let eq = match self {
        Const::Number(v) => *v == c.num(),
        Const::String(v) => v == c.str(),
      };
      return eq;
    }
    false
  }
}

#[derive(Debug, Clone)]
pub struct Upval {
  pub name: String,
  pub in_stack: bool,
  pub idx: u32,
}

#[derive(Debug, Clone)]
pub struct Local {
  pub name: String,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum InstType {
  ABC,
  ABx,
  AsBx,
}

#[derive(Clone)]
pub struct Inst {
  pub raw: u32,
}

impl Inst {
  pub fn new() -> Self {
    Inst { raw: 0 }
  }

  pub fn a(&self) -> u32 {
    (self.raw >> 6) & 0xff
  }

  pub fn b(&self) -> u32 {
    (self.raw >> 23) & 0x1ff
  }

  pub fn c(&self) -> u32 {
    (self.raw >> 14) & 0x1ff
  }

  pub fn bx(&self) -> u32 {
    (self.raw >> 14) & 0x3ffff
  }

  pub fn sbx(&self) -> i32 {
    let t = ((self.raw >> 14) & 0x3ffff) as i32;
    t - 131071
  }

  pub fn op(&self) -> u32 {
    self.raw & 0x3f
  }

  pub fn set_op(&mut self, op: OpCode) {
    self.raw = (self.raw & !0x3f) | (op as u32);
  }

  pub fn set_a(&mut self, a: u32) {
    self.raw = (self.raw & !(0xff << 6)) | (a << 6);
  }

  pub fn set_b(&mut self, b: u32) {
    self.raw = (self.raw & !(0x1ff << 23)) | (b << 23);
  }

  pub fn set_c(&mut self, c: u32) {
    self.raw = (self.raw & !(0x1ff << 14)) | (c << 14);
  }

  pub fn set_bx(&mut self, bx: u32) {
    self.raw = (self.raw & !(0x3ffff << 14)) | (bx << 14);
  }

  pub fn set_sbx(&mut self, mut sbx: i32) {
    sbx += 131071;
    let sbx = sbx as u32;
    self.raw = (self.raw & !(0x3ffff << 14)) | (sbx << 14);;
  }

  // converts an integer to a "floating point byte", from CLua
  pub fn int2fb(mut x: u32) -> u32 {
    let mut e = 0; /* exponent */
    if x < 8 {
      return x;
    }
    while x >= 8 << 4 {
      /* coarse steps */
      x = (x + 0xf) >> 4; /* x = ceil(x / 16) */
      e += 4;
    }
    while x >= 8 << 1 {
      /* fine steps */
      x = (x + 1) >> 1; /* x = ceil(x / 2) */
      e += 1;
    }
    ((e + 1) << 3) | (x - 8)
  }

  pub fn fb2int(x: u32) -> u32 {
    if x < 8 {
      return x;
    }
    ((x & 7) + 8) << ((x >> 3) - 1)
  }
}

impl fmt::Debug for Inst {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let op = OpCode::from_u32(self.op());
    match op.mode() {
      InstType::ABC => write!(
        f,
        "{:#?}{{ A: {}, B: {}, C: {} }}",
        op,
        self.a(),
        self.b(),
        self.c()
      ),
      InstType::ABx => write!(f, "{:#?}{{ A: {}, Bx: {} }}", op, self.a(), self.bx()),
      InstType::AsBx => write!(f, "{:#?}{{ A: {}, sBx: {} }}", op, self.a(), self.sbx()),
    }
  }
}

#[derive(Debug, Clone)]
pub struct FunTpl {
  pub param_cnt: u8,
  pub is_vararg: bool,
  pub code: Vec<Inst>,
  pub consts: Vec<Const>,
  pub upvals: Vec<Upval>,
  pub locals: Vec<Local>,
  pub fun_tpls: Vec<FunTpl>,
}

impl FunTpl {
  pub fn new() -> Self {
    FunTpl {
      param_cnt: 0,
      is_vararg: false,
      code: vec![],
      consts: vec![],
      upvals: vec![],
      locals: vec![],
      fun_tpls: vec![],
    }
  }
}

pub struct Chunk {
  pub sig: &'static str,
  pub ver: u64,
  pub upval_cnt: u8,
  pub top_fun_tpl: FunTpl,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum OpCode {
  MOVE,
  LOADK,
  LOADKX,
  LOADBOO,
  LOADNUL,
  LOADUNDEF,
  GETUPVAL,
  GETTABUP,
  SETTABUP,
  SETUPVAL,
  SETTABLE,
  NEWTABLE,
  NEWARRAY,
  INITARRAY,
  THIS,
  ADD,
  SUB,
  MUL,
  MOD,
  DIV,
  CLOSURE,
}

static mut OPCODE_VAL_NAME: Option<Vec<&'static str>> = None;
static mut OPCODE_VAL_MODE: Option<Vec<u8>> = None;

macro_rules! gen_opcode_map {
  ($($id:expr => $name:expr, $mode:expr)*) => {
    {
      let mut id_name = vec![];
      let mut id_mode = vec![];
      $(
        id_name.push($name);
        id_mode.push($mode);
      )*
      (Some(id_name), Some(id_mode))
    }
  };
}

fn init_opcodes() {
  let (id_name, id_mode) = gen_opcode_map! {
    MOVE => "MOVE", 0
    LOADK => "LOADK", 1
    LOADKX => "LOADKX", 1
    LOADBOO => "LOADBOO", 0
    LOADNUL => "LOADNUL", 0
    LOADUNDEF => "LOADUNDEF", 0
    GETUPVAL => "GETUPVAL", 0
    GETTABUP => "GETTABUP", 0
    SETTABUP => "SETTABUP", 0
    SETUPVAL => "SETUPVAL", 0
    SETTABLE => "SETTABLE", 0
    NEWTABLE => "NEWTABLE", 0
    NEWARRAY => "NEWARRAY", 0
    INITARRAY => "INITARRAY", 0
    THIS => "THIS", 0
    ADD => "ADD", 0
    SUB => "SUB", 0
    MUL => "MUL", 0
    MOD => "MOD", 0
    DIV => "DIV", 0
    CLOSURE => "CLOSURE", 0
  };
  unsafe {
    OPCODE_VAL_NAME = id_name;
    OPCODE_VAL_MODE = id_mode;
  }
}

static INIT_OPCODE_DATA_ONCE: Once = Once::new();
pub fn init_opcode_data() {
  INIT_OPCODE_DATA_ONCE.call_once(|| {
    init_opcodes();
  });
}

impl OpCode {
  pub fn from_u32(x: u32) -> Self {
    unsafe { transmute(x as u8) }
  }

  pub fn mode(&self) -> InstType {
    let idx = *self as usize;
    let mode = unsafe { OPCODE_VAL_MODE.as_ref().unwrap().get(idx).unwrap() };
    unsafe { transmute((*mode) as u8) }
  }
}

#[cfg(test)]
mod chunk_tests {
  use super::*;

  #[test]
  fn inst_test() {
    let mut inst = Inst::new();
    inst.set_op(OpCode::LOADUNDEF);
    inst.set_a(0);
    inst.set_b(1);
    assert_eq!(0, inst.a());
    assert_eq!(1, inst.b());

    let mut inst = Inst::new();
    inst.set_op(OpCode::LOADUNDEF);
    inst.set_a(1);
    inst.set_bx(20);
    assert_eq!(1, inst.a());
    assert_eq!(20, inst.bx());

    let mut inst = Inst::new();
    inst.set_op(OpCode::LOADUNDEF);
    inst.set_a(1);
    inst.set_sbx(-20);
    assert_eq!(1, inst.a());
    assert_eq!(-20, inst.sbx());
  }

  #[test]
  fn opcode_test() {
    init_opcodes();

    assert_eq!(InstType::ABC, OpCode::from_u32(0).mode());
    assert_eq!(InstType::ABx, OpCode::from_u32(1).mode());
  }
}
