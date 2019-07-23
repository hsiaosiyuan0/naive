extern crate lib;
use lib::asm::codegen::Codegen;
use lib::vm::exec::Vm;
use std::path::Path;
use std::process::exit;
use std::{env, fs};

fn main() {
  let args: Vec<String> = env::args().collect();
  let file = match args.get(1) {
    Some(arg) => arg,
    _ => exit({
      println!("missing file");
      1
    }),
  };
  let file = Path::new(file);
  let src = match fs::read_to_string(file) {
    Ok(src) => src,
    Err(e) => exit({
      format!("unable to read file: {:?}", e);
      1
    }),
  };
  let chk = Codegen::gen(src.as_str());
  let mut vm = Vm::new(chk, (1 << 20) * 100);
  match vm.exec() {
    Err(e) => exit({
      eprintln!("error: {:?}", e);
      1
    }),
    Ok(_) => (),
  }
}
