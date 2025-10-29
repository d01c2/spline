package spline.analyzer.sign

import spline.frontend.*
import spline.utils.*

case class Infinity(sign: Boolean) {
  override def toString: String = if sign then "+∞" else "-∞"
}

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
