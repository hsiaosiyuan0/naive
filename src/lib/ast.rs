use crate::token::*;
use std::rc::Rc;

#[derive(Debug)]
pub enum Literal {
  RegExp(RegExpData),
  Null(NullData),
  String(StringData),
  Bool(BoolData),
  Numeric(NumericData),
}

impl Literal {
  pub fn is_regexp(&self) -> bool {
    match self {
      Literal::RegExp(_) => true,
      _ => false,
    }
  }

  pub fn is_null(&self) -> bool {
    match self {
      Literal::Null(_) => true,
      _ => false,
    }
  }

  pub fn is_str(&self) -> bool {
    match self {
      Literal::String(_) => true,
      _ => false,
    }
  }

  pub fn is_bool(&self) -> bool {
    match self {
      Literal::Bool(_) => true,
      _ => false,
    }
  }

  pub fn is_num(&self) -> bool {
    match self {
      Literal::Numeric(_) => true,
      _ => false,
    }
  }

  pub fn regexp(&self) -> &RegExpData {
    match self {
      Literal::RegExp(d) => d,
      _ => panic!(),
    }
  }

  pub fn null(&self) -> &NullData {
    match self {
      Literal::Null(d) => d,
      _ => panic!(),
    }
  }

  pub fn str(&self) -> &StringData {
    match self {
      Literal::String(d) => d,
      _ => panic!(),
    }
  }

  pub fn bool(&self) -> &BoolData {
    match self {
      Literal::Bool(d) => d,
      _ => panic!(),
    }
  }

  pub fn num(&self) -> &NumericData {
    match self {
      Literal::Numeric(d) => d,
      _ => panic!(),
    }
  }
}

impl From<RegExpData> for Literal {
  fn from(f: RegExpData) -> Self {
    Literal::RegExp(f)
  }
}

impl From<NullData> for Literal {
  fn from(f: NullData) -> Self {
    Literal::Null(f)
  }
}

impl From<StringData> for Literal {
  fn from(f: StringData) -> Self {
    Literal::String(f)
  }
}

impl From<BoolData> for Literal {
  fn from(f: BoolData) -> Self {
    Literal::Bool(f)
  }
}

impl From<NumericData> for Literal {
  fn from(f: NumericData) -> Self {
    Literal::Numeric(f)
  }
}

#[derive(Debug)]
pub enum PrimaryExpr {
  This(ThisExprData),
  Identifier(IdData),
  Literal(Literal),
  ArrayLiteral(ArrayData),
  ObjectLiteral(ObjectData),
  Parenthesized(ParenData),
}

impl PrimaryExpr {
  pub fn is_id(&self) -> bool {
    match self {
      PrimaryExpr::Identifier(_) => true,
      _ => false,
    }
  }

  pub fn is_this(&self) -> bool {
    match self {
      PrimaryExpr::This(_) => true,
      _ => false,
    }
  }

  pub fn is_literal(&self) -> bool {
    match self {
      PrimaryExpr::Literal(_) => true,
      _ => false,
    }
  }

  pub fn is_array(&self) -> bool {
    match self {
      PrimaryExpr::ArrayLiteral(_) => true,
      _ => false,
    }
  }

  pub fn is_object(&self) -> bool {
    match self {
      PrimaryExpr::ObjectLiteral(_) => true,
      _ => false,
    }
  }

  pub fn is_paren(&self) -> bool {
    match self {
      PrimaryExpr::Parenthesized(_) => true,
      _ => false,
    }
  }

  pub fn this(&self) -> &ThisExprData {
    match self {
      PrimaryExpr::This(d) => d,
      _ => panic!(),
    }
  }

  pub fn literal(&self) -> &Literal {
    match self {
      PrimaryExpr::Literal(d) => d,
      _ => panic!(),
    }
  }

  pub fn array(&self) -> &ArrayData {
    match self {
      PrimaryExpr::ArrayLiteral(d) => d,
      _ => panic!(),
    }
  }

  pub fn object(&self) -> &ObjectData {
    match self {
      PrimaryExpr::ObjectLiteral(d) => d,
      _ => panic!(),
    }
  }

  pub fn paren(&self) -> &ParenData {
    match self {
      PrimaryExpr::Parenthesized(d) => d,
      _ => panic!(),
    }
  }

  pub fn id(&self) -> &IdData {
    match self {
      PrimaryExpr::Identifier(d) => d,
      _ => panic!(),
    }
  }
}

impl From<ThisExprData> for PrimaryExpr {
  fn from(f: ThisExprData) -> Self {
    PrimaryExpr::This(f)
  }
}

impl From<IdData> for PrimaryExpr {
  fn from(f: IdData) -> Self {
    PrimaryExpr::Identifier(f)
  }
}

impl From<Literal> for PrimaryExpr {
  fn from(f: Literal) -> Self {
    PrimaryExpr::Literal(f)
  }
}

impl From<StringData> for PrimaryExpr {
  fn from(f: StringData) -> Self {
    let node: Literal = f.into();
    node.into()
  }
}

impl From<NullData> for PrimaryExpr {
  fn from(f: NullData) -> Self {
    let node: Literal = f.into();
    node.into()
  }
}

impl From<RegExpData> for PrimaryExpr {
  fn from(f: RegExpData) -> Self {
    let node: Literal = f.into();
    node.into()
  }
}

impl From<BoolData> for PrimaryExpr {
  fn from(f: BoolData) -> Self {
    let node: Literal = f.into();
    node.into()
  }
}

impl From<NumericData> for PrimaryExpr {
  fn from(f: NumericData) -> Self {
    let node: Literal = f.into();
    node.into()
  }
}

impl From<ArrayData> for PrimaryExpr {
  fn from(f: ArrayData) -> Self {
    PrimaryExpr::ArrayLiteral(f)
  }
}

#[derive(Debug)]
pub struct UnaryExpr {
  pub op: Token,
  pub argument: Expr,
  pub prefix: bool,
}

impl UnaryExpr {
  pub fn new(op: Token, argument: Expr, prefix: bool) -> Self {
    UnaryExpr {
      op,
      argument,
      prefix,
    }
  }
}

#[derive(Debug)]
pub struct BinaryExpr {
  pub op: Token,
  pub left: Expr,
  pub right: Expr,
}

#[derive(Debug)]
pub struct MemberExpr {
  pub object: Expr,
  pub property: Expr,
  pub computed: bool,
}

#[derive(Debug)]
pub struct NewExpr {
  pub callee: Expr,
  pub arguments: Vec<Expr>,
}

#[derive(Debug)]
pub struct CallExpr {
  pub callee: Expr,
  pub arguments: Vec<Expr>,
}

#[derive(Debug)]
pub struct CondExpr {
  pub test: Expr,
  pub cons: Expr,
  pub alt: Expr,
}

#[derive(Debug)]
pub struct AssignExpr {
  pub op: Token,
  pub left: Expr,
  pub right: Expr,
}

#[derive(Debug)]
pub struct SeqExpr {
  pub expressions: Vec<Expr>,
}

#[derive(Debug)]
pub enum Expr {
  Primary(Rc<PrimaryExpr>),
  Function,
  Member(Rc<MemberExpr>),
  New(Rc<NewExpr>),
  Call(Rc<CallExpr>),
  Unary(Rc<UnaryExpr>),
  Binary(Rc<BinaryExpr>),
  Assignment(Rc<AssignExpr>),
  Conditional(Rc<CondExpr>),
  Sequence(Rc<SeqExpr>),
}

impl Expr {
  pub fn is_unary(&self) -> bool {
    match self {
      Expr::Unary(_) => true,
      _ => false,
    }
  }

  pub fn is_member(&self) -> bool {
    match self {
      Expr::Member(_) => true,
      _ => false,
    }
  }

  pub fn is_new(&self) -> bool {
    match self {
      Expr::New(_) => true,
      _ => false,
    }
  }

  pub fn is_call(&self) -> bool {
    match self {
      Expr::Call(_) => true,
      _ => false,
    }
  }

  pub fn is_bin(&self) -> bool {
    match self {
      Expr::Binary(_) => true,
      _ => false,
    }
  }

  pub fn is_cond(&self) -> bool {
    match self {
      Expr::Conditional(_) => true,
      _ => false,
    }
  }

  pub fn is_assign(&self) -> bool {
    match self {
      Expr::Assignment(_) => true,
      _ => false,
    }
  }

  pub fn is_seq(&self) -> bool {
    match self {
      Expr::Sequence(_) => true,
      _ => false,
    }
  }

  pub fn primary(&self) -> &PrimaryExpr {
    match self {
      Expr::Primary(expr) => expr,
      _ => panic!(),
    }
  }

  pub fn member(&self) -> &MemberExpr {
    match self {
      Expr::Member(expr) => expr,
      _ => panic!(),
    }
  }

  pub fn unary(&self) -> &UnaryExpr {
    match self {
      Expr::Unary(expr) => expr,
      _ => panic!(),
    }
  }

  pub fn new_expr(&self) -> &NewExpr {
    match self {
      Expr::New(expr) => expr,
      _ => panic!(),
    }
  }

  pub fn call_expr(&self) -> &CallExpr {
    match self {
      Expr::Call(expr) => expr,
      _ => panic!(),
    }
  }

  pub fn bin_expr(&self) -> &BinaryExpr {
    match self {
      Expr::Binary(expr) => expr,
      _ => panic!(),
    }
  }

  pub fn cond_expr(&self) -> &CondExpr {
    match self {
      Expr::Conditional(expr) => expr,
      _ => panic!(),
    }
  }

  pub fn assign_expr(&self) -> &AssignExpr {
    match self {
      Expr::Assignment(expr) => expr,
      _ => panic!(),
    }
  }

  pub fn seq_expr(&self) -> &SeqExpr {
    match self {
      Expr::Sequence(expr) => expr,
      _ => panic!(),
    }
  }
}

impl From<UnaryExpr> for Expr {
  fn from(f: UnaryExpr) -> Self {
    Expr::Unary(Rc::new(f))
  }
}

impl From<PrimaryExpr> for Expr {
  fn from(f: PrimaryExpr) -> Self {
    let expr = Rc::new(f);
    Expr::Primary(expr)
  }
}

impl From<NewExpr> for Expr {
  fn from(f: NewExpr) -> Self {
    let expr = Rc::new(f);
    Expr::New(expr)
  }
}

impl From<CallExpr> for Expr {
  fn from(f: CallExpr) -> Self {
    let expr = Rc::new(f);
    Expr::Call(expr)
  }
}

impl From<BinaryExpr> for Expr {
  fn from(f: BinaryExpr) -> Self {
    let expr = Rc::new(f);
    Expr::Binary(expr)
  }
}

impl From<CondExpr> for Expr {
  fn from(f: CondExpr) -> Self {
    let expr = Rc::new(f);
    Expr::Conditional(expr)
  }
}

impl From<AssignExpr> for Expr {
  fn from(f: AssignExpr) -> Self {
    let expr = Rc::new(f);
    Expr::Assignment(expr)
  }
}

impl From<SeqExpr> for Expr {
  fn from(f: SeqExpr) -> Self {
    let expr = Rc::new(f);
    Expr::Sequence(expr)
  }
}

#[derive(Debug)]
pub struct ThisExprData {
  pub loc: SourceLoc,
}

impl ThisExprData {
  pub fn new(loc: SourceLoc) -> Self {
    ThisExprData { loc }
  }
}

#[derive(Debug)]
pub struct IdData {
  pub loc: SourceLoc,
  pub name: String,
}

impl IdData {
  pub fn new(loc: SourceLoc, name: String) -> Self {
    IdData { loc, name }
  }
}

#[derive(Debug)]
pub struct RegExpData {
  pub loc: SourceLoc,
  pub value: String,
}

impl RegExpData {
  pub fn new(loc: SourceLoc, value: String) -> Self {
    RegExpData { loc, value }
  }
}

#[derive(Debug)]
pub struct NullData {
  pub loc: SourceLoc,
}

impl NullData {
  pub fn new(loc: SourceLoc) -> Self {
    NullData { loc }
  }
}

#[derive(Debug)]
pub struct StringData {
  pub loc: SourceLoc,
  pub value: String,
}

impl StringData {
  pub fn new(loc: SourceLoc, value: String) -> Self {
    StringData { loc, value }
  }
}

#[derive(Debug)]
pub struct BoolData {
  pub loc: SourceLoc,
  pub value: bool,
}

impl BoolData {
  pub fn new(loc: SourceLoc, value: bool) -> Self {
    BoolData { loc, value }
  }
}

#[derive(Debug)]
pub struct NumericData {
  pub loc: SourceLoc,
  pub value: String,
}

impl NumericData {
  pub fn new(loc: SourceLoc, value: String) -> Self {
    NumericData { loc, value }
  }
}

#[derive(Debug)]
pub struct ArrayData {
  pub loc: SourceLoc,
  pub value: Vec<Expr>,
}

#[derive(Debug)]
pub struct ObjectProperty {
  pub loc: SourceLoc,
  pub key: Expr,
  pub value: Expr,
}

#[derive(Debug)]
pub struct ObjectData {
  pub loc: SourceLoc,
  pub properties: Vec<ObjectProperty>,
}

#[derive(Debug)]
pub struct ParenData {
  pub loc: SourceLoc,
  pub value: Expr,
}
