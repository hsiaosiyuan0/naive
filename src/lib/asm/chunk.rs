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
  pub fn new_str(s: &str) -> Self {
    Const::String(s.to_string())
  }

  pub fn new_num(n: f64) -> Self {
    Const::Number(n)
  }

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
pub struct UpvalDesc {
  pub name: String,
  pub in_stack: bool,
  pub idx: u32,
}

#[derive(Debug, Clone)]
pub struct Local {
  pub name: String,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum OpMode {
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

  pub fn new_abc(op: OpCode, a: u32, b: u32, c: u32) -> Self {
    let mut inst = Inst::new();
    inst.set_op(op);
    inst.set_a(a);
    inst.set_b(b);
    inst.set_c(c);
    inst
  }

  pub fn new_a_bx(op: OpCode, a: u32, bx: u32) -> Self {
    let mut inst = Inst::new();
    inst.set_op(op);
    inst.set_a(a);
    inst.set_bx(bx);
    inst
  }

  pub fn new_a_sbx(op: OpCode, a: u32, sbx: i32) -> Self {
    let mut inst = Inst::new();
    inst.set_op(op);
    inst.set_a(a);
    inst.set_sbx(sbx);
    inst
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
      OpMode::ABC => write!(
        f,
        "{:#?}{{ A: {}, B: {}, C: {} }}",
        op,
        self.a(),
        self.b(),
        self.c()
      ),
      OpMode::ABx => write!(f, "{:#?}{{ A: {}, Bx: {} }}", op, self.a(), self.bx()),
      OpMode::AsBx => write!(f, "{:#?}{{ A: {}, sBx: {} }}", op, self.a(), self.sbx()),
    }
  }
}

#[derive(Debug, Clone)]
pub struct FnTpl {
  pub param_cnt: u8,
  pub is_vararg: bool,
  pub code: Vec<Inst>,
  pub consts: Vec<Const>,
  pub upvals: Vec<UpvalDesc>,
  pub locals: Vec<Local>,
  pub fun_tpls: Vec<FnTpl>,
}

impl FnTpl {
  pub fn new() -> Self {
    FnTpl {
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

#[derive(Debug)]
pub struct Chunk {
  pub sig: &'static str,
  pub ver: u64,
  pub upval_cnt: u8,
  pub fun_tpl: FnTpl,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum OpCode {
  MOVE,
  LOADK,
  LOADKX,
  LOADBOO,
  LOADNUL,
  LOADUNDEF,
  GETUPVAL,
  GETTABUP,
  GETTABLE,
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
  LT,
  LE,
  EQ,
  EQS,
  JMP,
  TEST,
  TESTSET,
  BITAND,
  BITOR,
  BITXOR,
  SHL,
  SAR,
  SHR,
  UNM,
  NOT,
  BITNOT,
  CLOSURE,
  CALL,
  RETURN,
  NEW,
}

static mut OPCODE_NAME: Option<HashMap<OpCode, &'static str>> = None;
static mut OPCODE_MODE: Option<HashMap<OpCode, OpMode>> = None;

macro_rules! gen_opcode_map {
  ($($op:expr => $name:expr, $mode:expr)*) => {
    {
      let mut op_name = HashMap::new();
      let mut op_mode = HashMap::new();
      $(
        op_name.insert($op, $name);
        op_mode.insert($op, $mode);
      )*
      (Some(op_name), Some(op_mode))
    }
  };
}

fn init_opcodes() {
  let (op_name, op_mode) = gen_opcode_map! {
    OpCode::MOVE => "MOVE", OpMode::ABC
    OpCode::LOADK => "LOADK", OpMode::ABx
    OpCode::LOADKX => "LOADKX", OpMode::ABx
    OpCode::LOADBOO => "LOADBOO", OpMode::ABC
    OpCode::LOADNUL => "LOADNUL", OpMode::ABC
    OpCode::LOADUNDEF => "LOADUNDEF", OpMode::ABC
    OpCode::GETUPVAL => "GETUPVAL", OpMode::ABC
    OpCode::GETTABUP => "GETTABUP", OpMode::ABC
    OpCode::GETTABLE => "GETTABLE", OpMode::ABC
    OpCode::SETTABUP => "SETTABUP", OpMode::ABC
    OpCode::SETUPVAL => "SETUPVAL", OpMode::ABC
    OpCode::SETTABLE => "SETTABLE", OpMode::ABC
    OpCode::NEWTABLE => "NEWTABLE", OpMode::ABC
    OpCode::NEWARRAY => "NEWARRAY", OpMode::ABC
    OpCode::INITARRAY => "INITARRAY", OpMode::ABC
    OpCode::THIS => "THIS", OpMode::ABC
    OpCode::ADD => "ADD", OpMode::ABC
    OpCode::SUB => "SUB", OpMode::ABC
    OpCode::MUL => "MUL", OpMode::ABC
    OpCode::MOD => "MOD", OpMode::ABC
    OpCode::DIV => "DIV", OpMode::ABC
    OpCode::LT => "LT", OpMode::ABC
    OpCode::LE => "LE", OpMode::ABC
    OpCode::EQ => "EQ", OpMode::ABC
    OpCode::EQS => "EQS", OpMode::ABC
    OpCode::JMP => "JMP", OpMode::AsBx
    OpCode::TEST => "TEST", OpMode::ABC
    OpCode::TESTSET => "TESTSET", OpMode::ABC
    OpCode::BITAND => "BITAND", OpMode::ABC
    OpCode::BITOR => "BITOR", OpMode::ABC
    OpCode::BITXOR => "BITXOR", OpMode::ABC
    OpCode::SHL => "SHL", OpMode::ABC
    OpCode::SAR => "SAR", OpMode::ABC
    OpCode::SHR => "SHR", OpMode::ABC
    OpCode::UNM => "UNM", OpMode::ABC
    OpCode::NOT => "NOT", OpMode::ABC
    OpCode::BITNOT => "BITNOT", OpMode::ABC
    OpCode::CLOSURE => "CLOSURE", OpMode::ABC
    OpCode::CALL => "CALL", OpMode::ABC
    OpCode::RETURN => "RETURN", OpMode::ABC
    OpCode::NEW => "NEW", OpMode::ABC
  };
  unsafe {
    OPCODE_NAME = op_name;
    OPCODE_MODE = op_mode;
  }
}

static INIT_OPCODE_DATA_ONCE: Once = Once::new();
pub fn init_opcode_data() {
  INIT_OPCODE_DATA_ONCE.call_once(|| {
    init_opcodes();
  });
}

pub fn op_to_name(op: &OpCode) -> &'static str {
  unsafe { OPCODE_NAME.as_ref().unwrap().get(op).unwrap() }
}

pub fn op_to_mode(op: &OpCode) -> &'static OpMode {
  unsafe { OPCODE_MODE.as_ref().unwrap().get(op).unwrap() }
}

impl OpCode {
  pub fn from_u32(x: u32) -> Self {
    unsafe { transmute(x as u8) }
  }

  pub fn mode(&self) -> OpMode {
    *op_to_mode(self)
  }

  pub fn eq(&self, op: u32) -> bool {
    OpCode::from_u32(op) == *self
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

    assert_eq!(OpMode::ABC, OpCode::from_u32(0).mode());
    assert_eq!(OpMode::ABx, OpCode::from_u32(1).mode());
  }
}
