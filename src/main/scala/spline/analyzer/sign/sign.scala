package spline.analyzer.sign

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

enum Sign {
  case Top
  case PosOrZero
  case NegOrZero
  case Zero
  case Bottom

  override def toString: String = this match
    case Bottom    => "⊥"
    case Top       => "⊤"
    case PosOrZero => "≥0"
    case NegOrZero => "≤0"
    case Zero      => "0"

}
