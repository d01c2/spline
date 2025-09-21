package spline.lang

enum Stat {
  // assignment
  case SAssign(x: String, e: Expr)
  // sequence
  case SSeq(s1: Stat, s2: Stat)
  // conditional
  case SCond(c: Cond, s1: Stat, s2: Stat)
  // loop
  case SLoop(c: Cond, s: Stat)
  // no-op
  case SNop
  // assertion
  case SAssert(c: Cond)

  // the string form of a statement
  def str: String = this match
    case SAssign(x, e)    => s"$x <- ${e.str}"
    case SSeq(s1, s2)     => s"${s1.str}; ${s2.str}"
    case SCond(c, s1, s2) => s"if ${c.str} then ${s1.str} else ${s2.str} endif"
    case SLoop(c, s)      => s"while ${c.str} do ${s.str} done"
    case SNop             => "skip"
    case SAssert(c)       => s"assert ${c.str}"
}

enum BOp {
  case Add, Sub, Mul, Div
  override def toString: String = this match
    case Add => "+"
    case Sub => "-"
    case Mul => "*"
    case Div => "/"
}

case class Interval(c1: Value, c2: Value) {
  override def toString: String = s"[$c1, $c2]"
}

enum Expr {
  // variable
  case EVar(v: String)
  // numeric constant
  case ENum(c: Value)
  // negation
  case ENeg(e: Expr)
  // binary operation
  case EBOp(bop: BOp, e1: Expr, e2: Expr)
  // input variable with interval
  case EInput(itv: Interval)

  // the string form of an expression
  def str: String = this match
    case EVar(v)           => v
    case ENum(c)           => c.toString
    case ENeg(e)           => s"-${e.str}"
    case EBOp(bop, e1, e2) => s"${e1.str} $bop ${e2.str}"
    case EInput(itv)       => itv.toString
}

enum Cmp {
  case Leq, Geq, Lt, Gt, Eq, Neq
  override def toString: String = this match
    case Leq => "<="
    case Geq => ">="
    case Lt  => "<"
    case Gt  => ">"
    case Eq  => "=="
    case Neq => "!="
}

enum Cond {
  // comparison
  case CCmp(cmp: Cmp, e1: Expr, e2: Expr)
  // boolean constant
  case CBool(b: Boolean)
  // logic negation
  case CNot(c: Cond)
  // logic and
  case CAnd(c1: Cond, c2: Cond)
  // logic or
  case COr(c1: Cond, c2: Cond)

  // the string form of a condition
  def str: String = this match
    case CCmp(cmp, e1, e2) => s"${e1.str} $cmp ${e2.str}"
    case CBool(b)          => b.toString
    case CNot(c)           => s"!${c.str}"
    case CAnd(c1, c2)      => s"${c1.str} /\\ ${c2.str}"
    case COr(c1, c2)       => s"${c1.str} \\/ ${c2.str}"
}

// value
type Value = BigInt

// state
type State = Map[String, Value]

// parsers
object Stat extends Parser.From(Parser.stat)
object Expr extends Parser.From(Parser.expr)
object Cond extends Parser.From(Parser.cond)
