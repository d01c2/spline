package spline

import spline.frontend.*

import spline.analyzer.sign.{*, given}
import spline.analyzer.interval.{*, given}

@main def main: Unit =
  val program = Stat(
    """if 0 <= a /\ 0 <= b then
      |  q <- 0;
      |  r <- a;
      |  while b <= r do
      |    r <- r - b;
      |    q <- q + 1
      |  done;
      |  assert 0 <= q;
      |  assert 0 <= r
      |else
      |  skip
      |endif
      |""".stripMargin,
  )
  val initSt = Map[String, Value]("a" -> 20, "b" -> 3, "q" -> 0, "r" -> 0)

  val signResult =
    import SignDomain.*
    transfer(program, initSt.map((k, v) => k -> alpha(Set(v))))

  val intervalResult =
    import IntervalDomain.*
    transfer(program, initSt.map((k, v) => k -> alpha(Set(v))))

  println(s"Sign    : $signResult")
  println(s"Interval: $intervalResult")
  println(s"Octagon : <not implemented yet>")
