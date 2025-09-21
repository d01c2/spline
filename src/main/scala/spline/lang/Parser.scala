package spline.lang

import spline.utils.*
import scala.util.parsing.combinator.*

// TODO: test parser with realistic examples
object Parser extends RegexParsers with PackratParsers {
  type P[+T] = PackratParser[T]
  class From[T](p: Parser[T]) {
    def apply(s: String): T = parseAll(p, s).getOrElse(error("parse error"))
  }

  override val whiteSpace = """[ \t\r\n]+""".r

  private val keywords = Set(
    "assert",
    "do",
    "done",
    "else",
    "endif",
    "false",
    "if",
    "skip",
    "then",
    "true",
    "while",
  )
  private lazy val ident: P[String] = "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { s =>
    if (keywords.contains(s)) error(s"keyword used as an identifier: $s")
    else s
  }

  private lazy val num: P[BigInt] = "-?[0-9]+".r ^^ { BigInt(_) }
  private lazy val bool: P[Boolean] = "true" ^^^ true | "false" ^^^ false

  // stat parser
  lazy val stat: P[Stat] =
    import Stat.*
    lazy val s0: P[Stat] =
      (("if" ~> cond) ~ ("then" ~> stat) ~ ("else" ~> stat) <~ "endif") ^^ {
        case c ~ t ~ e => SCond(c, t, e)
      } |
      (("while" ~> cond) ~ ("do" ~> stat) <~ "done") ^^ {
        case c ~ b => SLoop(c, b)
      } |
      ("skip" ^^^ SNop) |
      (("assert" ~> cond) ^^ (SAssert(_))) |
      ((ident ~ ("<-" ~> expr)) ^^ { case x ~ e => SAssign(x, e) })

    (rep1sep(s0, ";") <~ opt(";")) ^^ {
      case hd :: tl => tl.foldLeft(hd)(SSeq(_, _))
      case Nil      => Stat.SNop
    }

  // expr parser
  lazy val expr: P[Expr] =
    import Expr.*
    import BOp.*
    lazy val e0: P[Expr] =
      (num ^^ (n => ENum(n))) |
      ("[" ~> num ~ ("," ~> num) <~ "]" ^^ {
        case c1 ~ c2 => EInput(Interval(c1, c2))
      }) |
      (ident ^^ (v => EVar(v)))
    lazy val e1: P[Expr] = ("-" ~> e1 ^^ (ENeg(_))) | e0
    lazy val mulOp: P[BOp] = ("*" ^^^ Mul) | ("/" ^^^ Div)
    lazy val addOp: P[BOp] = ("+" ^^^ Add) | ("-" ^^^ Sub)
    lazy val e2: P[Expr] = e1 ~ rep(mulOp ~ e1) ^^ {
      case h ~ ts =>
        ts.foldLeft(h) { case (acc, op ~ r) => EBOp(op, acc, r) }
    }
    lazy val e3: P[Expr] = e2 ~ rep(addOp ~ e2) ^^ {
      case h ~ ts =>
        ts.foldLeft(h) { case (acc, op ~ r) => EBOp(op, acc, r) }
    }
    e3

  // cond parser
  lazy val cond: P[Cond] =
    import Cond.*
    import Cmp.*
    lazy val cmp: P[Cmp] =
      ("<=" ^^^ Leq) | (">=" ^^^ Geq) |
      ("==" ^^^ Eq) | ("!=" ^^^ Neq) |
      ("<" ^^^ Lt) | (">" ^^^ Gt)
    lazy val c0: P[Cond] =
      (bool ^^ (CBool(_))) |
      ((expr ~ cmp ~ expr) ^^ { case e1 ~ c ~ e2 => CCmp(c, e1, e2) })
    lazy val c1: P[Cond] = ("!" ~> c1 ^^ (CNot(_))) | c0
    lazy val c2: P[Cond] = c1 ~ rep("/\\" ~> c1) ^^ {
      case h ~ ts => ts.foldLeft(h)(CAnd(_, _))
    }
    lazy val c3: P[Cond] = c2 ~ rep("\\/" ~> c2) ^^ {
      case h ~ ts => ts.foldLeft(h)(COr(_, _))
    }
    c3
}
