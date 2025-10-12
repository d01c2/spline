package spline.analyzer.interval

import spline.frontend.*
import spline.utils.*

case class Infinity(sign: Boolean) {
  override def toString: String = if sign then "+∞" else "-∞"
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

def min(bs: Bound*): Bound = bs.min
def min(bs: Iterable[Bound]): Bound = bs.min
def max(bs: Bound*): Bound = bs.max
def max(bs: Iterable[Bound]): Bound = bs.max

extension (self: Bound) {
  def unary_- : Bound = self match
    case Infinity(sign) => Infinity(!sign)
    case v: Value       => -v
  def +(other: Bound): Bound = (self, other) match
    case (Infinity(true), _) | (_, Infinity(true))   => Infinity(true)
    case (Infinity(false), _) | (_, Infinity(false)) => Infinity(false)
    case (v1: Value, v2: Value)                      => v1 + v2
  def -(other: Bound): Bound = self + (-other)
  def *(other: Bound): Bound = (self, other) match
    case (Infinity(sign1), Infinity(sign2)) => Infinity(sign1 == sign2)
    case (Infinity(sign), v: Value) =>
      if v == 0 then 0
      else { if (v > 0) Infinity(sign) else Infinity(!sign) }
    case (v: Value, Infinity(sign)) =>
      if v == 0 then 0
      else { if (v > 0) Infinity(sign) else Infinity(!sign) }
    case (v1: Value, v2: Value) => v1 * v2
  def /(other: Bound): Bound = (self, other) match
    case (Infinity(_), Infinity(_)) => 0
    case (v: Value, Infinity(sign)) => 0
    case (_, v: Value) if v == 0 =>
      error(s"division by zero: $self / $other")
    case (bound, v: Value) =>
      bound match
        case Infinity(sign) => if (v > 0) Infinity(sign) else Infinity(!sign)
        case v0: BigInt     => v0 / v
}

case class Interval(lb: Bound, ub: Bound) {
  override def toString: String = if this.isBottom then "⊥" else s"[$lb, $ub]"
}
