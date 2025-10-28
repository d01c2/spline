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
  import Expr.*, Cond.*, Stat.*
  override def transfer(c: Cond, st: Map[String, Sign]): AbsState =
    c match
      case CCmp(cmp, e1, e2) =>
        (cmp, e1, e2) match
          case (Cmp.Leq, EVar(v), ENum(0)) =>
            st(v) match
              case PosOrZero | Zero => st.updated(v, Zero)
              case NegOrZero | Top  => st.updated(v, NegOrZero)
              case _                => Map.empty
          case (Cmp.Leq, EVar(v), EVar(w)) =>
            val sign1 = st(v)
            val sign2 = st(w)
            val a =
              sign1 match
                case Zero | PosOrZero => st.updated(w, PosOrZero)
                case _                => st
            val b =
              sign2 match
                case Zero | NegOrZero => st.updated(v, NegOrZero)
                case _                => st
            a ⊓ b
          case (Cmp.Geq, EVar(v), ENum(0)) =>
            st(v) match
              case NegOrZero | Zero => st.updated(v, Zero)
              case PosOrZero | Top  => st.updated(v, PosOrZero)
              case _                => Map.empty
          case (Cmp.Geq, EVar(v), EVar(w)) =>
            val sign1 = st(v)
            val sign2 = st(w)
            val a = sign1 match
              case Zero | NegOrZero => st.updated(w, NegOrZero)
              case _                => st
            val b = sign2 match
              case Zero | PosOrZero => st.updated(v, PosOrZero)
              case _                => st
            a ⊓ b
          case (Cmp.Lt, EVar(v), ENum(0)) =>
            st(v) match
              case NegOrZero | Zero => Map.empty
              case PosOrZero | Top  => st.updated(v, PosOrZero)
              case _                => Map.empty
          case (Cmp.Lt, EVar(v), EVar(w)) =>
            val sign1 = st(v)
            val sign2 = st(w)
            val a = sign1 match
              case Zero | NegOrZero => Map.empty
              case PosOrZero | Top  => st.updated(w, PosOrZero)
              case _                => st
            val b = sign2 match
              case Zero | PosOrZero => Map.empty
              case NegOrZero | Top  => st.updated(v, NegOrZero)
              case _                => st
            a ⊓ b
          case (Cmp.Gt, EVar(v), ENum(0)) =>
            st(v) match
              case PosOrZero | Zero => Map.empty
              case NegOrZero | Top  => st.updated(v, NegOrZero)
              case _                => Map.empty
          case (Cmp.Gt, EVar(v), EVar(w)) =>
            val sign1 = st(v)
            val sign2 = st(w)
            val a = sign1 match
              case Zero | PosOrZero => Map.empty
              case NegOrZero | Top  => st.updated(w, NegOrZero)
              case _                => st
            val b = sign2 match
              case Zero | NegOrZero => Map.empty
              case PosOrZero | Top  => st.updated(v, PosOrZero)
              case _                => st
            a ⊓ b
          case (Cmp.Eq, EVar(v), ENum(0)) =>
            st(v) match
              case Bottom => Map.empty
              case _      => st.updated(v, Zero)
          case (Cmp.Eq, EVar(v), EVar(w)) =>
            val sign1 = st(v)
            val sign2 = st(w)
            val a = sign1 match
              case Bottom => Map.empty
              case _      => st.updated(w, sign1)
            val b = sign2 match
              case Bottom => Map.empty
              case _      => st.updated(v, sign2)
            a ⊓ b
          case (Cmp.Neq, EVar(v), ENum(0)) =>
            st(v) match
              case Zero   => Map.empty
              case Bottom => Map.empty
              case _      => st
          case (Cmp.Neq, EVar(v), EVar(w)) =>
            val sign1 = st(v)
            val sign2 = st(w)
            (sign1, sign2) match
              case (Bottom, _) | (_, Bottom) => Map.empty
              case (Zero, Zero)              => Map.empty
              case _                         => st
          case _ => super.transfer(c, st)
      case _ => super.transfer(c, st)
}
