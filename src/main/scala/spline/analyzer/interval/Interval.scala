package spline.analyzer.interval

import spline.frontend.*

case class Infinity(pos: Boolean) {
  override def toString: String = if pos then "+∞" else "-∞"
}

type Bound = Value | Infinity

given Ordering[Bound] with {
  def compare(x: Bound, y: Bound): Int = (x, y) match
    case (Infinity(true), Infinity(true))   => 0
    case (Infinity(false), Infinity(false)) => 0
    case (Infinity(true), _)                => 1
    case (_, Infinity(true))                => -1
    case (Infinity(false), _)               => -1
    case (_, Infinity(false))               => 1
    case (v1: Value, v2: Value)             => v1 compare v2
}

extension (a: Bound) {
  def min(b: Bound)(using ord: Ordering[Bound]) = ord.min(a, b)
  def max(b: Bound)(using ord: Ordering[Bound]) = ord.max(a, b)
}

case class Interval(lb: Bound, ub: Bound) {
  override def toString: String = if this.isBottom then "⊥" else s"[$lb, $ub]"
}
