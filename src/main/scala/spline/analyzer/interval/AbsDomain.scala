package spline.analyzer.interval

import spline.analyzer.*
import spline.frontend.*
import spline.utils.*

given IntervalDomain: AbsDomain[Interval] with {
  extension (self: Interval) {
    def ⊑(other: Interval): Boolean = (self, other) match
      case _ if self.isBottom               => true
      case _ if other.isBottom              => false
      case (Interval(a, b), Interval(c, d)) => (a >= c) && (b <= d)

    def ⊔(other: Interval): Interval = (self, other) match
      case _ if self.isBottom               => other
      case _ if other.isBottom              => self
      case (Interval(a, b), Interval(c, d)) => Interval(a min c, b max d)

    def ⊓(other: Interval): Interval = (self, other) match
      case _ if self.isBottom || other.isBottom => bot
      case (Interval(a, b), Interval(c, d)) if (a max c) <= (b min d) =>
        Interval(a max c, b min d)
      case _ => bot

    // TODO: implement widening for interval domain
    def ▽(other: Interval): Interval = ???
  }

  def gamma(interval: Interval): Set[Value] =
    if (interval.isBottom) Set.empty
    else {
      (interval.lb, interval.ub) match
        case (Infinity(_), _) | (_, Infinity(_)) =>
          error(s"concretization on unbounded interval: $interval")
        case (lb: Value, ub: Value) => (lb to ub).toSet
    }

  def bot = Interval(Infinity(true), Infinity(false)) // canonical bottom
  def top = Interval(Infinity(false), Infinity(true))

  def alpha(x: Set[Value]): Interval =
    if x.isEmpty then bot else Interval(x.min, x.max)
}

// helper methods
extension (interval: Interval) {
  def isBottom: Boolean = (interval.lb, interval.ub) match
    case (Infinity(true), _) | (_, Infinity(false)) => true
    case (lb, ub) if lb > ub                        => true
    case _                                          => false
}
