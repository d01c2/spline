package spline

import spline.analyzer.interval.{*, given}
import spline.utils.TestUtils.*

class IntervalAnalysisTest extends munit.FunSuite:
  test("stat: simple while loop") {
    val expected = Map("X" -> Interval(40, Infinity(true)))
    val actual = IntervalDomain.transfer(
      parseStat("X <- 0; while X < 40 do X <- X + 1 done"),
      Map.empty,
    )
    assertEquals(actual, expected)
  }
