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

  pub fn loc(&self) -> &SourceLoc {
    match self {
      Literal::RegExp(d) => &d.loc,
      Literal::Null(d) => &d.loc,
      Literal::Numeric(d) => &d.loc,
      Literal::String(d) => &d.loc,
      Literal::Bool(d) => &d.loc,
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
  pub loc: SourceLoc,
  pub op: Token,
  pub argument: Expr,
  pub prefix: bool,
}

#[derive(Debug)]
pub struct BinaryExpr {
  pub loc: SourceLoc,
  pub op: Token,
  pub left: Expr,
  pub right: Expr,
}

#[derive(Debug)]
pub struct MemberExpr {
  pub loc: SourceLoc,
  pub object: Expr,
  pub property: Expr,
  pub computed: bool,
}

#[derive(Debug)]
pub struct NewExpr {
  pub loc: SourceLoc,
  pub callee: Expr,
  pub arguments: Vec<Expr>,
}

#[derive(Debug)]
pub struct CallExpr {
  pub loc: SourceLoc,
  pub callee: Expr,
  pub arguments: Vec<Expr>,
}

#[derive(Debug)]
pub struct CondExpr {
  pub loc: SourceLoc,
  pub test: Expr,
  pub cons: Expr,
  pub alt: Expr,
}

#[derive(Debug)]
pub struct AssignExpr {
  pub loc: SourceLoc,
  pub op: Token,
  pub left: Expr,
  pub right: Expr,
}

#[derive(Debug)]
pub struct SeqExpr {
  pub loc: SourceLoc,
  pub exprs: Vec<Expr>,
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

#[derive(Debug)]
pub struct ExprStmt {
  pub expr: Expr,
}

#[derive(Debug)]
pub struct BlockStmt {
  pub loc: SourceLoc,
  pub body: Vec<Stmt>,
}

#[derive(Debug)]
pub struct VarDecor {
  pub id: PrimaryExpr,
  pub init: Option<Expr>,
}

#[derive(Debug)]
pub struct VarDec {
  pub loc: SourceLoc,
  pub decs: Vec<VarDecor>,
}

#[derive(Debug)]
pub struct EmptyStmt {
  pub loc: SourceLoc,
}

#[derive(Debug)]
pub struct IfStmt {
  pub loc: SourceLoc,
  pub test: Expr,
  pub cons: Stmt,
  pub alt: Option<Stmt>,
}

#[derive(Debug)]
pub enum ForFirst {
  VarDec(Rc<VarDec>),
  Expr(Expr),
}

#[derive(Debug)]
pub struct ForStmt {
  pub loc: SourceLoc,
  pub init: Option<ForFirst>,
  pub test: Option<Expr>,
  pub update: Option<Expr>,
  pub body: Stmt,
}

#[derive(Debug)]
pub struct ForInStmt {
  pub loc: SourceLoc,
  pub left: ForFirst,
  pub right: Expr,
  pub body: Stmt,
}

#[derive(Debug)]
pub struct DoWhileStmt {
  pub loc: SourceLoc,
  pub test: Expr,
  pub body: Stmt,
}

#[derive(Debug)]
pub struct WhileStmt {
  pub loc: SourceLoc,
  pub test: Expr,
  pub body: Stmt,
}

#[derive(Debug)]
pub struct ContStmt {
  pub loc: SourceLoc,
}

#[derive(Debug)]
pub struct BreakStmt {
  pub loc: SourceLoc,
}

#[derive(Debug)]
pub struct ReturnStmt {
  pub loc: SourceLoc,
  pub argument: Option<Expr>,
}

#[derive(Debug)]
pub struct WithStmt {
  pub loc: SourceLoc,
  pub object: Expr,
  pub body: Stmt,
}

#[derive(Debug)]
pub struct SwitchStmt {
  pub loc: SourceLoc,
}

#[derive(Debug)]
pub struct ThrowStmt {
  pub loc: SourceLoc,
  pub argument: Option<Expr>,
}

#[derive(Debug)]
pub struct CatchClause {
  pub id: IdData,
  pub body: BreakStmt,
}

#[derive(Debug)]
pub struct TryStmt {
  pub loc: SourceLoc,
  pub block: BlockStmt,
  pub handler: Option<CatchClause>,
  pub finalizer: Option<BlockStmt>,
}

#[derive(Debug)]
pub struct DebugStmt {
  pub loc: SourceLoc,
}

#[derive(Debug)]
pub enum Stmt {
  Block(Rc<BlockStmt>),
  VarDec(Rc<VarDec>),
  Empty(Rc<EmptyStmt>),
  Expr(Rc<ExprStmt>),
  If(Rc<IfStmt>),
  For(Rc<ForStmt>),
  ForIn(Rc<ForInStmt>),
  DoWhile(Rc<DoWhileStmt>),
  While(Rc<WhileStmt>),
  Cont(Rc<ContStmt>),
  Break(Rc<BreakStmt>),
  Return(Rc<ReturnStmt>),
  With(Rc<WithStmt>),
  Switch(Rc<SwitchStmt>),
  Throw(Rc<ThrowStmt>),
  Try(Rc<TryStmt>),
  Debugger(Rc<DebugStmt>),
}

impl From<BlockStmt> for Stmt {
  fn from(f: BlockStmt) -> Self {
    let expr = Rc::new(f);
    Stmt::Block(expr)
  }
}

impl From<ExprStmt> for Stmt {
  fn from(f: ExprStmt) -> Self {
    let expr = Rc::new(f);
    Stmt::Expr(expr)
  }
}

impl From<VarDec> for Stmt {
  fn from(f: VarDec) -> Self {
    let expr = Rc::new(f);
    Stmt::VarDec(expr)
  }
}

impl From<IfStmt> for Stmt {
  fn from(f: IfStmt) -> Self {
    let expr = Rc::new(f);
    Stmt::If(expr)
  }
}

impl From<ForInStmt> for Stmt {
  fn from(f: ForInStmt) -> Self {
    let expr = Rc::new(f);
    Stmt::ForIn(expr)
  }
}

impl From<ForStmt> for Stmt {
  fn from(f: ForStmt) -> Self {
    let expr = Rc::new(f);
    Stmt::For(expr)
  }
}

impl From<DoWhileStmt> for Stmt {
  fn from(f: DoWhileStmt) -> Self {
    let expr = Rc::new(f);
    Stmt::DoWhile(expr)
  }
}

impl From<WhileStmt> for Stmt {
  fn from(f: WhileStmt) -> Self {
    let expr = Rc::new(f);
    Stmt::While(expr)
  }
}

impl Stmt {
  pub fn is_block(&self) -> bool {
    match self {
      Stmt::Block(_) => true,
      _ => false,
    }
  }

  pub fn is_expr(&self) -> bool {
    match self {
      Stmt::Expr(_) => true,
      _ => false,
    }
  }

  pub fn is_var(&self) -> bool {
    match self {
      Stmt::VarDec(_) => true,
      _ => false,
    }
  }

  pub fn is_if(&self) -> bool {
    match self {
      Stmt::If(_) => true,
      _ => false,
    }
  }

  pub fn is_for(&self) -> bool {
    match self {
      Stmt::For(_) => true,
      _ => false,
    }
  }

  pub fn is_for_in(&self) -> bool {
    match self {
      Stmt::ForIn(_) => true,
      _ => false,
    }
  }

  pub fn is_do_while(&self) -> bool {
    match self {
      Stmt::DoWhile(_) => true,
      _ => false,
    }
  }

  pub fn is_while_stmt(&self) -> bool {
    match self {
      Stmt::While(_) => true,
      _ => false,
    }
  }

  pub fn block(&self) -> &BlockStmt {
    match self {
      Stmt::Block(s) => s,
      _ => panic!(),
    }
  }

  pub fn expr(&self) -> &ExprStmt {
    match self {
      Stmt::Expr(s) => s,
      _ => panic!(),
    }
  }

  pub fn var_dec(&self) -> &VarDec {
    match self {
      Stmt::VarDec(s) => s,
      _ => panic!(),
    }
  }

  pub fn if_stmt(&self) -> &IfStmt {
    match self {
      Stmt::If(s) => s,
      _ => panic!(),
    }
  }

  pub fn for_stmt(&self) -> &ForStmt {
    match self {
      Stmt::For(s) => s,
      _ => panic!(),
    }
  }

  pub fn for_in(&self) -> &ForInStmt {
    match self {
      Stmt::ForIn(s) => s,
      _ => panic!(),
    }
  }

  pub fn do_while(&self) -> &DoWhileStmt {
    match self {
      Stmt::DoWhile(s) => s,
      _ => panic!(),
    }
  }

  pub fn while_stmt(&self) -> &WhileStmt {
    match self {
      Stmt::While(s) => s,
      _ => panic!(),
    }
  }
}
