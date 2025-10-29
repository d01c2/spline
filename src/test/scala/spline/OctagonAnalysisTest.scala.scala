package spline

import spline.analyzer.octagon.{*, given}
import spline.utils.TestUtils.*

class OctagonAnalysisTest extends munit.FunSuite:
  test("stat: simple while loop") {
    val actual = OctagonDomain.transfer(
      parseStat("X <- 0; while X < 40 do X <- X + 1 done"),
      Map.empty,
    )

    // Octagon 도메인에서는 변수 X가 [40, +∞) 범위에 있어야 함
    actual.get("X") match {
      case Some(oct) =>
        assert(!oct.isBottom, "Result should not be bottom")

        oct.varIndex("X") match {
          case Some(idx) =>
            val i = idx * 2 // x_X
            val i_comp = i + 1 // -x_X

            val lowerBound = oct.dbm(i_comp)(i)
            assert(
              lowerBound <= (-80: Bound),
              s"Expected X ≥ 40, but got constraint: -2X ≤ $lowerBound",
            )

            val upperBound = oct.dbm(i)(i_comp)
            assert(
              upperBound == Infinity(true),
              s"Expected X ≤ +∞, but got constraint: 2X ≤ $upperBound",
            )

          case None => fail("Variable X not found in octagon")
        }
      case None => fail("No result for variable X")
    }
  }
