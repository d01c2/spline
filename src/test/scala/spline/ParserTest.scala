package spline

import spline.frontend.*

class ParserTest extends munit.FunSuite:
  private def parseExpr(input: String): Expr = Expr(input)
  private def parseCond(input: String): Cond = Cond(input)
  private def parseStat(input: String): Stat = Stat(input)

  test("expr: arithmetic precedence and associativity") {
    val expr1 = "1 + 2 * 3 - 4"
    val parsed = parseExpr(expr1)
    val expected = Expr.EBOp(
      BOp.Sub,
      Expr.EBOp(
        BOp.Add,
        Expr.ENum(BigInt(1)),
        Expr.EBOp(BOp.Mul, Expr.ENum(BigInt(2)), Expr.ENum(BigInt(3))),
      ),
      Expr.ENum(BigInt(4)),
    )
    assertEquals(expr1, expected.str)
    assertEquals(parsed, expected)
  }

  test("expr: unary negation with interval input") {
    val expr2 = "-x + [0, 10]"
    val parsed = parseExpr(expr2)
    val expected = Expr.EBOp(
      BOp.Add,
      Expr.ENeg(Expr.EVar("x")),
      Expr.EInput(BigInt(0), BigInt(10)),
    )
    assertEquals(expr2, expected.str)
    assertEquals(parsed, expected)
  }

  test("cond: logical precedence") {
    val cond1 = "!true \\/ false /\\ x <= 1"
    val parsed = parseCond(cond1)
    val expected = Cond.COr(
      Cond.CNot(Cond.CBool(true)),
      Cond.CAnd(
        Cond.CBool(false),
        Cond.CCmp(Cmp.Leq, Expr.EVar("x"), Expr.ENum(BigInt(1))),
      ),
    )
    assertEquals(cond1, expected.str)
    assertEquals(parsed, expected)
  }

  test("stat: sequential composition builds left-associated tree") {
    val stat1 = "x <- 1; y <- x + 2; assert x != y"
    val parsed = parseStat(stat1)
    val expected = Stat.SSeq(
      Stat.SSeq(
        Stat.SAssign("x", Expr.ENum(BigInt(1))),
        Stat.SAssign(
          "y",
          Expr.EBOp(BOp.Add, Expr.EVar("x"), Expr.ENum(BigInt(2))),
        ),
      ),
      Stat.SAssert(
        Cond.CCmp(Cmp.Neq, Expr.EVar("x"), Expr.EVar("y")),
      ),
    )
    assertEquals(stat1, expected.str)
    assertEquals(parsed, expected)
  }

  test("stat: while loop body parses as nested sequence") {
    val stat2 = "while x < 10 do x <- x + 1; y <- y - 1 done; z <- z + 0"
    val parsed = parseStat(stat2)
    val loopBody = Stat.SSeq(
      Stat.SAssign(
        "x",
        Expr.EBOp(BOp.Add, Expr.EVar("x"), Expr.ENum(BigInt(1))),
      ),
      Stat.SAssign(
        "y",
        Expr.EBOp(BOp.Sub, Expr.EVar("y"), Expr.ENum(BigInt(1))),
      ),
    )
    val expected = Stat.SSeq(
      Stat.SLoop(
        Cond.CCmp(Cmp.Lt, Expr.EVar("x"), Expr.ENum(BigInt(10))),
        loopBody,
      ),
      Stat.SAssign(
        "z",
        Expr.EBOp(BOp.Add, Expr.EVar("z"), Expr.ENum(BigInt(0))),
      ),
    )
    assertEquals(stat2, expected.str)
    assertEquals(parsed, expected)
  }

  test("expr: rejects keywords used as identifiers") {
    val ex = intercept[Exception] {
      parseExpr("if")
    }
    assert(ex.getMessage.contains("keyword used as an identifier"))
  }
