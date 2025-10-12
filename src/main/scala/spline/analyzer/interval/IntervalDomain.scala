package spline.analyzer.interval

import spline.analyzer.*
import spline.frontend.*
import spline.utils.*

// TODO: Implement advanced abstract tests in Ch 4.6
// TODO: Implement advanced iteration techniques in Ch 4.7
given IntervalDomain: AbsDomain[Interval] with {
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

  // heuristic to improve precision
  import Expr.*, Cond.*, Stat.*
  override def transfer(c: Cond, st: Map[String, Interval]): AbsState =
    c match
      case CCmp(cmp, e1, e2) =>
        (cmp, e1, e2) match
          case (Cmp.Leq, EVar(v), ENum(k)) =>
            val Interval(a, b) = st(v)
            if a <= k then st.updated(v, Interval(a, min(b, k)))
            else Map.empty
          case (Cmp.Leq, EVar(v), EVar(w)) =>
            val Interval(a, b) = st(v)
            val Interval(c, d) = st(w)
            if a <= d then
              st ++ Map(
                v -> Interval(a, min(b, d)),
                w -> Interval(max(a, c), d),
              )
            else Map.empty
          case (Cmp.Geq, EVar(v), ENum(k)) =>
            val Interval(a, b) = st(v)
            if b >= k then st.updated(v, Interval(max(a, k), b))
            else Map.empty
          case (Cmp.Geq, EVar(v), EVar(w)) =>
            val Interval(a, b) = st(v)
            val Interval(c, d) = st(w)
            if b >= c then
              st ++ Map(
                v -> Interval(max(a, c), b),
                w -> Interval(c, min(b, d)),
              )
            else Map.empty
          case (Cmp.Lt, EVar(v), ENum(k)) =>
            val Interval(a, b) = st(v)
            if a < k then st.updated(v, Interval(a, min(b, k - 1)))
            else Map.empty
          case (Cmp.Lt, EVar(v), EVar(w)) =>
            val Interval(a, b) = st(v)
            val Interval(c, d) = st(w)
            if a < d then
              st ++ Map(
                v -> Interval(a, min(b, d - 1)),
                w -> Interval(max(a + 1, c), d),
              )
            else Map.empty
          case (Cmp.Gt, EVar(v), ENum(k)) =>
            val Interval(a, b) = st(v)
            if b > k then st.updated(v, Interval(max(a, k + 1), b))
            else Map.empty
          case (Cmp.Gt, EVar(v), EVar(w)) =>
            val Interval(a, b) = st(v)
            val Interval(c, d) = st(w)
            if b > c then
              st ++ Map(
                v -> Interval(max(a, c + 1), b),
                w -> Interval(c, min(b - 1, d)),
              )
            else Map.empty
          case (Cmp.Eq, EVar(v), ENum(k)) =>
            val Interval(a, b) = st(v)
            if a <= k && k <= b then st.updated(v, Interval(k, k))
            else Map.empty
          case (Cmp.Eq, EVar(v), EVar(w)) =>
            val Interval(a, b) = st(v)
            val Interval(c, d) = st(w)
            if max(a, c) <= min(b, d) then
              val e = Interval(max(a, c), min(b, d))
              st ++ Map(v -> e, w -> e)
            else Map.empty
          case (Cmp.Neq, EVar(v), ENum(k)) =>
            val Interval(a, b) = st(v)
            if a == b && a == k then Map.empty
            else st
          case (Cmp.Neq, EVar(v), EVar(w)) =>
            val Interval(a, b) = st(v)
            val Interval(c, d) = st(w)
            if a == b && c == d && a == c then Map.empty
            else st
          case _ => super.transfer(c, st)
      case _ => super.transfer(c, st)
}
