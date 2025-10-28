package spline

import spline.analyzer.sign.{*, given}
import spline.utils.TestUtils.*

class SignAnalysisTest extends munit.FunSuite:
  test("sign: addition Zero + PosOrZero => PosOrZero") {
    val prog = "X <- 0; Y <- 5; X <- X + Y"
    val actual = SignDomain.transfer(parseStat(prog), Map.empty)
    val expected = Map("X" -> Sign.PosOrZero, "Y" -> Sign.PosOrZero)
    assertEquals(actual, expected)
  }
  test("sign: if both branches assign Zero => Zero") {
    val prog = "X <- 1; if X < 0 then X <- 0 else X <- 0 endif"
    val actual = SignDomain.transfer(parseStat(prog), Map.empty)
    val expected = Map("X" -> Sign.Zero)
    assertEquals(actual, expected)
  }
  test("sign: simple while loop") {
    val prog = "X <- 0; while X < 40 do X <- X + 1 done"
    val actual = SignDomain.transfer(parseStat(prog), Map.empty)
    val expected = Map("X" -> Sign.PosOrZero)
    assertEquals(actual, expected)
  }
