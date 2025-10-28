package spline.analyzer.sign

import spline.analyzer.*
import spline.frontend.*
import spline.utils.*
import spline.analyzer.sign.Sign.*
// TODO: Implement advanced abstract tests in Ch 4.6
// TODO: Implement advanced iteration techniques in Ch 4.7
given SignDomain: AbsDomain[Sign] with {
  def bot = Bottom // canonical bottom
  def top = Top

  extension (sign: Sign) {
    def isBottom: Boolean = sign == Bottom
    def isTop: Boolean = sign == Top
  }
  def gamma(sign: Sign): Set[Value] =
    if (sign.isBottom) Set.empty
    if (sign == Zero) Set(0)
    else {
      error(s"concretization on unbounded: $sign")
    }
  def alpha(x: Set[Value]): Sign =
    if x.isEmpty then Bottom
    else if x.min < 0 then
      if x.max > 0 then Top
      else NegOrZero
    else if x.max > 0 then PosOrZero
    else Zero

  extension (self: Sign) {
    def ⊑(other: Sign): Boolean = (self, other) match
      case (a, b) if a == b              => true // reflexive
      case (Bottom, _)                   => true // ⊥ ⊑ x
      case (_, Top)                      => true // x ⊑ ⊤
      case (Zero, PosOrZero | NegOrZero) => true // 0 ⊑ ≥0, 0 ⊑ ≤0
      case _                             => false

    def ⊔(other: Sign): Sign = (self, other) match
      case (Bottom, _)                           => other
      case (_, Bottom)                           => self
      case (a, b) if a == b                      => a
      case (Zero, PosOrZero) | (PosOrZero, Zero) => PosOrZero
      case (Zero, NegOrZero) | (NegOrZero, Zero) => NegOrZero
      case _                                     => Top

    def ⊓(other: Sign): Sign = (self, other) match
      case (Top, _)                                        => other
      case (_, Top)                                        => self
      case (a, b) if a == b                                => a
      case (PosOrZero, NegOrZero) | (NegOrZero, PosOrZero) => Zero
      case _                                               => Bottom

    def ▽(other: Sign): Sign = ⊔(other)

  }

  def absNeg(sign: Sign): AbsValue = sign match
    case Zero   => Zero
    case Bottom => Bottom
    case _      => Top

  def absBOp(bop: BOp, sign1: Sign, sign2: Sign): AbsValue =
    bop match
      case BOp.Add =>
        (sign1, sign2) match
          case (Bottom, _) | (_, Bottom) => Bottom
          case (Zero, x)                 => x
          case (x, Zero)                 => x
          case (PosOrZero, PosOrZero)    => PosOrZero
          case (NegOrZero, NegOrZero)    => NegOrZero
          case _                         => Top
      case BOp.Sub =>
        (sign1, sign2) match
          case (Bottom, _) | (_, Bottom) => Bottom
          case (x, Zero)                 => x
          case (Zero, PosOrZero)         => NegOrZero
          case (Zero, NegOrZero)         => PosOrZero
          case (PosOrZero, NegOrZero)    => PosOrZero
          case (NegOrZero, PosOrZero)    => NegOrZero
          case _                         => Top
      case BOp.Mul =>
        (sign1, sign2) match
          case (Bottom, _) | (_, Bottom) => Bottom
          case (Zero, _) | (_, Zero)     => Zero
          case (PosOrZero, PosOrZero)    => PosOrZero
          case (NegOrZero, NegOrZero)    => PosOrZero
          case (PosOrZero, NegOrZero)    => NegOrZero
          case (NegOrZero, PosOrZero)    => NegOrZero
          case _                         => Top
      case BOp.Div =>
        (sign1, sign2) match
          case (Bottom, _) | (_, Bottom) => Bottom
          case (Zero, _)                 => Zero
          case (_, Zero)                 => Bottom // division by zero
          case (PosOrZero, PosOrZero)    => PosOrZero
          case (NegOrZero, NegOrZero)    => PosOrZero
          case (PosOrZero, NegOrZero)    => NegOrZero
          case (NegOrZero, PosOrZero)    => NegOrZero
          case _                         => Top

}
