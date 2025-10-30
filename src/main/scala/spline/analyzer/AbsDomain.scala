package spline.analyzer

import spline.frontend.*
import spline.utils.*

trait AbsDomain[Elem] {
  type AbsValue = Elem
  type AbsState = Map[String, AbsValue]
  extension (st: AbsState) {
    def apply(v: String): AbsValue = st.getOrElse(v, top)
  }

  // bottom and top elements
  def bot: AbsValue
  def top: AbsValue

  extension (e: Elem) {
    def isBottom: Boolean
    def isTop: Boolean
  }

  // concretization
  def gamma(x: AbsValue): Set[Value]
  def gamma(st: AbsState): Map[String, Set[Value]] =
    if st.values.exists(_.isBottom) then Map.empty
    else st.view.mapValues(gamma).toMap

  // abstraction
  def alpha(x: Set[Value]): AbsValue
  def alpha(st: Map[String, Set[Value]]): AbsState =
    st.view.mapValues(alpha).toMap

  extension (x: AbsValue) {
    // partial order
    def ⊑(y: AbsValue): Boolean

    // join and meet operations
    def ⊔(y: AbsValue): AbsValue
    def ⊓(y: AbsValue): AbsValue

    // widening operation
    def ▽(y: AbsValue): AbsValue
  }

  extension (x: AbsState) {
    // partial order
    def ⊑(y: AbsState): Boolean =
      (x.keySet ++ y.keySet).forall(k =>
        x.getOrElse(k, top) ⊑ y.getOrElse(k, top),
      )

    // join and meet operations
    def ⊔(y: AbsState): AbsState =
      (x.keySet ++ y.keySet)
        .map(k => k -> (x.getOrElse(k, top) ⊔ y.getOrElse(k, top)))
        .toMap
    def ⊓(y: AbsState): AbsState =
      (x.keySet ++ y.keySet)
        .map(k => k -> (x.getOrElse(k, top) ⊓ y.getOrElse(k, top)))
        .toMap

    // widening operation
    def ▽(y: AbsState): AbsState =
      (x.keySet ++ y.keySet)
        .map(k => k -> (x.getOrElse(k, top) ▽ y.getOrElse(k, top)))
        .toMap
  }

  // abstract transfer functions
  import Expr.*, Cond.*, Stat.*

  def absNeg(v: AbsValue): AbsValue
  def absBOp(bop: BOp, v1: AbsValue, v2: AbsValue): AbsValue

  def transfer(e: Expr, st: AbsState): AbsValue = e match
    case EVar(v)           => st.getOrElse(v, top)
    case ENum(c)           => alpha(Set(c))
    case ENeg(e)           => absNeg(transfer(e, st))
    case EBOp(bop, e1, e2) => absBOp(bop, transfer(e1, st), transfer(e2, st))
    case EInput(c1, c2)    => alpha((c1 to c2).toSet)

  def transfer(c: Cond, st: AbsState): AbsState = c match
    case CCmp(cmp, e1, e2) => st
    case CBool(b)          => if b then st else Map.empty
    case CNot(c) =>
      c match
        case CCmp(cmp, e1, e2) => transfer(CCmp(cmp.negate, e1, e2), st)
        case CBool(b)          => transfer(CBool(!b), st)
        case CNot(c)           => transfer(c, st)
        case CAnd(c1, c2)      => transfer(COr(CNot(c1), CNot(c2)), st)
        case COr(c1, c2)       => transfer(CAnd(CNot(c1), CNot(c2)), st)
    case CAnd(c1, c2) => transfer(c1, st) ⊓ transfer(c2, st)
    case COr(c1, c2)  => transfer(c1, st) ⊔ transfer(c2, st)

  def transfer(s: Stat, st: AbsState): AbsState = s match
    case SAssign(x, e) => st + (x -> transfer(e, st))
    case SSeq(s1, s2)  => transfer(s2, transfer(s1, st))
    case SCond(c, s1, s2) =>
      transfer(s1, transfer(c, st)) ⊔ transfer(s2, transfer(CNot(c), st))
    case SLoop(c, s) =>
      def F(x: AbsState): AbsState = x ▽ (st ⊔ transfer(s, transfer(c, x)))
      transfer(CNot(c), fixpoint(F)(st))
    case SNop       => st
    case SAssert(c) => transfer(c, st)
}
