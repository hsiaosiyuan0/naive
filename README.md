# Naive

This is a naive JavaScript engine implemented in pure Rust to improve my JavaScript knowledge.

[![asciicast](https://asciinema.org/a/NSyybvRaFimC4OLIDtarKJkII.svg)](https://asciinema.org/a/NSyybvRaFimC4OLIDtarKJkII)

## Checklist

### Frontend

* [x] Lexer
* [x] Recursive descent parser with [Operator Precedence Algorithm](https://en.wikipedia.org/wiki/Operator-precedence_parser) to optimize operators parsing
* [x] Generates abstract syntax tree on basis of [ESTree](https://github.com/estree/estree)

### Intermediate

* [x] Generates bytecode(opcode) from AST
* [ ] Bytecode analysis
* [ ] Optimizes bytecode according to the analysis result

### VM

* [x] Reference-counting GC with Mark-and-Sweep to deal with the reference cycle
* [x] Common Flow Control Statements(if-else, for, while, do-while)
* [x] Closure
* [x] Mathematical Operations
* [x] Function Invocation
* [x] Logical Operations
* [ ] Bitwise Operations
* [x] Object Literal
* [x] New Expression
* [x] Member Access Expression
* [ ] For-in Statement
* [ ] Switch Case Statement
* [ ] Try Cache Statement
* [ ] Debug

## How to run

Thanks to the Rust development infrastructure, run this engine from source is very easy, just:

```
$ cargo build
$ cargo run your-script.js
```

It's still in development, after it’s completed I’d like to write a series of posts to describe how to build a JavaScript engine from scratch, stay tuned.

Feel free to leave any response by submitting a issue. Star/Fork to subscribe its updates is welcome.