package spline

import spline.frontend.*
import spline.interpreter.*
import spline.utils.TestUtils.*

class InterpreterTest extends munit.FunSuite:
  test("expr: division by zero") {
    val expected = Set[Value]()
    val actual = Interpreter.eval(parseExpr("1 + 2 / 0"), State())
    assertEquals(actual, expected)
  }

  test("expr: interval input with division by zero") {
    val expected = Set[Value]()
    val actual = Interpreter.eval(parseExpr("[-1, 1] / 0"), State())
    assertEquals(actual, expected)
  }

  test("expr: division with interval input") {
    val expected = Set[Value](-1, 1)
    val actual = Interpreter.eval(parseExpr("1 / [-1, 1]"), State())
    assertEquals(actual, expected)
  }

  test("cond: division by zero") {
    val expected = Set[State]()
    val actual = Interpreter.eval(parseCond("1 == 1 / 0"), Set[State]())
    assertEquals(actual, expected)
  }

  test("cond: logical or with division by zero") {
    val defaultSts = Set(State("x" -> 42))
    val expected = defaultSts
    val actual = Interpreter.eval(parseCond("1 == 1 / 0 \\/ true"), defaultSts)
    assertEquals(actual, expected)
  }

  test("stat: while loop") {
    val actual = Interpreter.eval(
      parseStat("Q <- 0; R <- A; while R >= B do R <- R - B; Q <- Q + 1 done"),
      Set(State("A" -> 10, "B" -> 3)),
    )
    val expected = Set(State("A" -> 10, "B" -> 3, "Q" -> 3, "R" -> 1))
    assertEquals(actual, expected)
  }
