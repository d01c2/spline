package spline.analyzer.interval

import spline.analyzer.*
import spline.frontend.*
import spline.utils.*

given AbsDomain: AbsDomain[Interval] with {
  def bot = Interval(Infinity(true), Infinity(false)) // canonical bottom
  def top = Interval(Infinity(false), Infinity(true))

  extension (interval: Interval) {
    def isBottom: Boolean = (interval.lb, interval.ub) match
      case (Infinity(true), _) | (_, Infinity(false)) => true
      case (lb, ub) if lb > ub                        => true
      case _                                          => false
    def isTop: Boolean = interval == top
  }

  def gamma(interval: Interval): Set[Value] =
    if (interval.isBottom) Set.empty
    else {
      (interval.lb, interval.ub) match
        case (Infinity(_), _) | (_, Infinity(_)) =>
          error(s"concretization on unbounded interval: $interval")
        case (lb: Value, ub: Value) => (lb to ub).toSet
    }

  def alpha(x: Set[Value]): Interval =
    if x.isEmpty then bot else Interval(x.min, x.max)

  extension (self: Interval) {
    def ⊑(other: Interval): Boolean = (self, other) match
      case _ if self.isBottom               => true
      case _ if other.isBottom              => false
      case (Interval(a, b), Interval(c, d)) => (a >= c) && (b <= d)

    def ⊔(other: Interval): Interval = (self, other) match
      case _ if self.isBottom               => other
      case _ if other.isBottom              => self
      case (Interval(a, b), Interval(c, d)) => Interval(min(a, c), max(b, d))

    def ⊓(other: Interval): Interval = (self, other) match
      case _ if self.isBottom || other.isBottom => bot
      case (Interval(a, b), Interval(c, d)) if max(a, c) <= min(b, d) =>
        Interval(max(a, c), min(b, d))
      case _ => bot

    def ▽(other: Interval): Interval =
      val lb = if self.lb <= other.lb then self.lb else Infinity(false)
      val ub = if self.ub >= other.ub then self.ub else Infinity(true)
      Interval(lb, ub)
  }

  def absNeg(interval: Interval): AbsValue =
    val Interval(a, b) = interval
    Interval(-b, -a)

  def absBOp(bop: BOp, interval1: Interval, interval2: Interval): AbsValue =
    val Interval(a, b) = interval1
    val Interval(c, d) = interval2
    bop match
      case BOp.Add => Interval(a + c, b + d)
      case BOp.Sub => Interval(a - d, b - c)
      case BOp.Mul =>
        val prods = Seq(a * c, a * d, b * c, b * d)
        Interval(min(prods), max(prods))
      case BOp.Div =>
        if c >= 1 then Interval(min(a / c, a / d), max(b / c, b / d))
        else if d <= -1 then Interval(min(b / c, b / d), max(a / c, a / d))
        else
          absBOp(BOp.Div, interval1, interval2 ⊓ Interval(1, Infinity(true))) ⊔
          absBOp(BOp.Div, interval1, interval2 ⊓ Interval(Infinity(false), -1))
}
