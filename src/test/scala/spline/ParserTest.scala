package spline

import spline.frontend.*

class ParserTest extends munit.FunSuite:
  private def parseExpr(input: String): Expr = Expr(input)
  private def parseCond(input: String): Cond = Cond(input)
  private def parseStat(input: String): Stat = Stat(input)

  test("expr: arithmetic precedence and associativity") {
    val expected = "1 + 2 * 3 - 4"
    val actual = parseExpr(expected).str
    assertEquals(actual, expected)
  }

  test("expr: unary negation with interval input") {
    val expected = "-x + [0, 10]"
    val actual = parseExpr(expected).str
    assertEquals(actual, expected)
  }

  test("cond: logical precedence") {
    val expected = "!true \\/ false /\\ x <= 1"
    val actual = parseCond(expected).str
    assertEquals(actual, expected)
  }

  test("stat: sequential composition builds left-associated tree") {
    val expected = "x <- 1; y <- x + 2; assert x != y"
    val actual = parseStat(expected).str
    assertEquals(actual, expected)
  }

  test("stat: while loop body parses as nested sequence") {
    val expected = "while x < 10 do x <- x + 1; y <- y - 1 done; z <- z + 0"
    val actual = parseStat(expected).str
    assertEquals(actual, expected)
  }

  test("expr: rejects keyword used as identifier") {
    val exception = intercept[Exception] { parseExpr("if") }
    assert(exception.getMessage.contains("keyword used as an identifier"))
  }
