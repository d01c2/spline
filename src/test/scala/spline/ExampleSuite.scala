package spline

import munit.FunSuite
import spline.lang.{Expr as ExprAST, Cond as CondAST, Stat as StatAST, BOp, Cmp, Interval}

class ParserSuite extends FunSuite:

  private def parseExpr(input: String): ExprAST = ExprAST(input)
  private def parseCond(input: String): CondAST = CondAST(input)
  private def parseStat(input: String): StatAST = StatAST(input)

  test("expr: arithmetic precedence and associativity") { 
    val expr1 = "1 + 2 * 3 - 4"
    val parsed = parseExpr(expr1)
    val expected = ExprAST.EBOp(
      BOp.Sub,
      ExprAST.EBOp(
        BOp.Add,
        ExprAST.ENum(BigInt(1)),
        ExprAST.EBOp(BOp.Mul, ExprAST.ENum(BigInt(2)), ExprAST.ENum(BigInt(3))),
      ),
      ExprAST.ENum(BigInt(4)),
    )
    assertEquals(expr1,expected.str)
    assertEquals(parsed, expected)
  }

  test("expr: unary negation with interval input") {
    val expr2 = "-x + [0, 10]"
    val parsed = parseExpr(expr2)
    val expected = ExprAST.EBOp(
      BOp.Add,
      ExprAST.ENeg(ExprAST.EVar("x")),
      ExprAST.EInput(Interval(BigInt(0), BigInt(10))),
    )
    assertEquals(expr2,expected.str)
    assertEquals(parsed, expected)
  }

  test("cond: logical precedence") {
    val cond1 = "!true \\/ false /\\ x <= 1"
    val parsed = parseCond(cond1)
    val expected = CondAST.COr(
      CondAST.CNot(CondAST.CBool(true)),
      CondAST.CAnd(
        CondAST.CBool(false),
        CondAST.CCmp(Cmp.Leq, ExprAST.EVar("x"), ExprAST.ENum(BigInt(1))),
      ),
    )
    assertEquals(cond1,expected.str)
    assertEquals(parsed, expected)
  }

  test("stat: sequential composition builds left-associated tree") {
    val stat1 = "x <- 1; y <- x + 2; assert x != y"
    val parsed = parseStat(stat1)
    val expected = StatAST.SSeq(
      StatAST.SSeq(
        StatAST.SAssign("x", ExprAST.ENum(BigInt(1))),
        StatAST.SAssign(
          "y",
          ExprAST.EBOp(BOp.Add, ExprAST.EVar("x"), ExprAST.ENum(BigInt(2))),
        ),
      ),
      StatAST.SAssert(
        CondAST.CCmp(Cmp.Neq, ExprAST.EVar("x"), ExprAST.EVar("y")),
      ),
    )
    assertEquals(stat1,expected.str)
    assertEquals(parsed, expected)
  }

  test("stat: while loop body parses as nested sequence") {
    val stat2 = "while x < 10 do x <- x + 1; y <- y - 1 done; z <- z + 0"
    val parsed = parseStat(stat2)
    val loopBody = StatAST.SSeq(
      StatAST.SAssign(
        "x",
        ExprAST.EBOp(BOp.Add, ExprAST.EVar("x"), ExprAST.ENum(BigInt(1))),
      ),
      StatAST.SAssign(
        "y",
        ExprAST.EBOp(BOp.Sub, ExprAST.EVar("y"), ExprAST.ENum(BigInt(1))),
      ),
    )
    val expected = StatAST.SSeq(
      StatAST.SLoop(
        CondAST.CCmp(Cmp.Lt, ExprAST.EVar("x"), ExprAST.ENum(BigInt(10))),
        loopBody,
      ),
      StatAST.SAssign(
        "z",
        ExprAST.EBOp(BOp.Add, ExprAST.EVar("z"), ExprAST.ENum(BigInt(0))),
      ),
    )
    assertEquals(stat2,expected.str)
    assertEquals(parsed, expected)
  }

  test("expr: rejects keywords used as identifiers") {
    val ex = intercept[Exception] {
      parseExpr("if")
    }
    assert(ex.getMessage.contains("keyword used as an identifier"))
  }
end ParserSuite
