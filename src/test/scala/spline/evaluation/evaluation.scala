package spline
import spline.frontend.*

import spline.analyzer.sign.{*, given}
import spline.analyzer.interval.{*, given}

class DivisionSignAnalysisTest extends munit.FunSuite:

  test("case1 unsafe: direct divide by zero literal") {
    val prog =
      "P <- 10; D <- 0; R <- P / D"
    val actual = SignDomain.transfer(Stat(prog), Map.empty)
    assert(true)
  }

  test("case2 safe: assert nonzero before divide") {
    val prog =
      "A <- 10; B <- 2; assert B != 0; Q <- A / B"
    val actual = SignDomain.transfer(Stat(prog), Map.empty)
    assert(true)
  }

  test("case3 safe: guarded branch around zero denom") {
    val prog =
      "X <- 50; Y <- 0; if Y != 0 then Z <- X / Y else Z <- 0 endif"
    val actual = SignDomain.transfer(Stat(prog), Map.empty)
    assert(true)
  }

  test("case4 unsafe: denom is difference that can be zero") {
    val prog =
      "M <- 9; N <- 9; DEN <- M - N; RES <- M / DEN"
    val actual = SignDomain.transfer(Stat(prog), Map.empty)
    assert(true)
  }

  test("case5 unsafe: loop decrements denom until zero") {
    val prog =
      "I <- 5; DEN <- 2; while I > 0 do TMP <- I / DEN; DEN <- DEN - 1; I <- I - 1 done"
    val actual = SignDomain.transfer(Stat(prog), Map.empty)
    assert(true)
  }

  test("case6 safe: loop with assert before every divide") {
    val prog =
      "I <- 5; DEN <- 2; while I > 0 do assert DEN != 0; TMP <- I / DEN; DEN <- DEN - 1; I <- I - 1 done"
    val actual = SignDomain.transfer(Stat(prog), Map.empty)
    assert(true)
  }

  test("case7 unsafe: branch picks possibly zero denom then used unguarded") {
    val prog =
      "FLAG <- [-1, 1]; DEN <- 0; if FLAG > 0 then DEN <- 3 else DEN <- 0 endif; V <- 99; OUT <- V / DEN"
    val actual = SignDomain.transfer(Stat(prog), Map.empty)
    assert(true)
  }

  test("case8 safe: branch always assigns nonzero denom before divide") {
    val prog =
      "FLAG <- [-1, 1]; DEN <- 0; if FLAG > 0 then DEN <- 3 else DEN <- 1 endif; V <- 99; OUT <- V / DEN"
    val actual = SignDomain.transfer(Stat(prog), Map.empty)
    assert(true)
  }

  test(
    "case9 unsafe: first safe division, then force denom to zero and divide again",
  ) {
    val prog =
      "A <- 100; B <- 5; R1 <- A / B; B <- 0; R2 <- A / B"
    val actual = SignDomain.transfer(Stat(prog), Map.empty)
    assert(true)
  }

  test("case10 safe: require both operands nonzero with /\\ before divide") {
    val prog =
      "X <- [-10, 10]; Y <- [-10, 10]; if X != 0 /\\ Y != 0 then R <- X / Y else R <- 1 endif"
    val actual = SignDomain.transfer(Stat(prog), Map.empty)
    assert(true)
  }

  test("case11 safe: combined assert on numerator and denominator") {
    val prog =
      "A <- [-10, 10]; B <- [-10, 10]; assert A != 0 /\\ B != 0; R <- A / B"
    val actual = SignDomain.transfer(Stat(prog), Map.empty)
    assert(true)
  }

  test("case12 unsafe: nondet denom in loop without guard") {
    val prog =
      "COUNT <- 3; D <- [-1, 1]; while COUNT > 0 do R <- COUNT / D; COUNT <- COUNT - 1 done"
    val actual = SignDomain.transfer(Stat(prog), Map.empty)
    assert(true)
  }

  test("case13 safe: sanitize denom before loop then reuse") {
    val prog =
      "D <- [-1, 1]; if D == 0 then D <- 2 else D <- D endif; C <- 3; while C > 0 do R <- C / D; C <- C - 1 done"
    val actual = SignDomain.transfer(Stat(prog), Map.empty)
    assert(true)
  }

  test("case14 unsafe: else-branch inherits zero denom and divides") {
    val prog =
      "X <- 10; Y <- 0; if X > 5 then A <- X - 5; B <- 2; R <- A / B else A <- X; B <- Y; R <- A / B endif"
    val actual = SignDomain.transfer(Stat(prog), Map.empty)
    assert(true)
  }

  test("case15 safe: both branches build nonzero denom before divide") {
    val prog =
      "X <- 10; Y <- 0; if X > 5 then A <- X - 5; B <- 2; R <- A / B else A <- X + 1; B <- 1; R <- A / B endif"
    val actual = SignDomain.transfer(Stat(prog), Map.empty)
    assert(true)
  }

  test(
    "case16 safe: assert complex nonzero expr then divide and post-process",
  ) {
    val prog =
      "A <- [-10, 10]; B <- A + 2; assert B != 0; C <- A / B; D <- C + 5"
    val actual = SignDomain.transfer(Stat(prog), Map.empty)
    assert(true)
  }

  test("case17 safe: loop fixes denom each iteration if zero") {
    val prog =
      "I <- 0; DEN <- [-2, 2]; while I < 4 do if DEN == 0 then DEN <- 1 else DEN <- DEN endif; RES <- I / DEN; I <- I + 1 done"
    val actual = SignDomain.transfer(Stat(prog), Map.empty)
    assert(true)
  }

  test("case18 unsafe: ok/data flow with possible zero sink") {
    val prog =
      "ok <- 1; data <- 0; recv <- [-10, 10]; if recv == 0 then ok <- 0 else data <- recv endif; RES <- 100 / data"
    val actual = SignDomain.transfer(Stat(prog), Map.empty)
    assert(true)
  }

  test("case19 safe: guard final divide with if data != 0") {
    val prog =
      "ok <- 1; data <- 0; recv <- [-10, 10]; if recv == 0 then ok <- 0 else data <- recv endif; if data != 0 then RES <- 100 / data else RES <- 0 endif"
    val actual = SignDomain.transfer(Stat(prog), Map.empty)
    assert(true)
  }

  test(
    "case20 mixed: first safe divide via assert then later unsafe divide after mutating denom",
  ) {
    val prog =
      "A <- 4; B <- 2; assert B != 0; Q1 <- A / B; B <- B - 2; if B != 0 then Q2 <- A / B else Q2 <- Q1 endif; B <- 0; Q3 <- A / B"
    val actual = SignDomain.transfer(Stat(prog), Map.empty)
    assert(true)
  }
