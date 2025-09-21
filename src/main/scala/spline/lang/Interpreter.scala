package spline.lang

import spline.utils.*

object Interpreter {
  import Expr.*
  def eval(e: Expr, st: State): Set[Value] = e match
    case EVar(v) => Set(st.getOrElse(v, error(s"free variable: $v")))
    case ENum(c) => Set(c)
    case ENeg(e) => eval(e, st).map(v => -v)
    case EBOp(bop, e1, e2) =>
      bop match
        case BOp.Add =>
          for {
            v1 <- eval(e1, st)
            v2 <- eval(e2, st)
          } yield v1 + v2
        case BOp.Sub =>
          for {
            v1 <- eval(e1, st)
            v2 <- eval(e2, st)
          } yield v1 - v2
        case BOp.Mul =>
          for {
            v1 <- eval(e1, st)
            v2 <- eval(e2, st)
          } yield v1 * v2
        case BOp.Div =>
          for {
            v1 <- eval(e1, st)
            v2 <- eval(e2, st)
            if v2 != 0
          } yield v1 / v2
    case EInput(Interval(c1, c2)) =>
      if c1 > c2 then error(s"invalid interval: [$c1, $c2]")
      else (c1 to c2).toSet

  import Cond.*
  def eval(c: Cond, sts: Set[State]): Set[State] = c match
    case CCmp(cmp, e1, e2) =>
      cmp match
        case Cmp.Leq =>
          for {
            st <- sts
            v1 <- eval(e1, st)
            v2 <- eval(e2, st)
            if v1 <= v2
          } yield st
        case Cmp.Geq =>
          for {
            st <- sts
            v1 <- eval(e1, st)
            v2 <- eval(e2, st)
            if v1 >= v2
          } yield st
        case Cmp.Lt =>
          for {
            st <- sts
            v1 <- eval(e1, st)
            v2 <- eval(e2, st)
            if v1 < v2
          } yield st
        case Cmp.Gt =>
          for {
            st <- sts
            v1 <- eval(e1, st)
            v2 <- eval(e2, st)
            if v1 > v2
          } yield st
        case Cmp.Eq =>
          for {
            st <- sts
            v1 <- eval(e1, st)
            v2 <- eval(e2, st)
            if v1 == v2
          } yield st
        case Cmp.Neq =>
          for {
            st <- sts
            v1 <- eval(e1, st)
            v2 <- eval(e2, st)
            if v1 != v2
          } yield st
    case CBool(b)     => if b then sts else Set.empty
    case CNot(c)      => eval(c, sts)
    case CAnd(c1, c2) => eval(c1, sts) intersect eval(c2, sts)
    case COr(c1, c2)  => eval(c1, sts) ++ eval(c2, sts)

  import Stat.*
  def eval(s: Stat, sts: Set[State]): Set[State] = s match
    case SAssign(x, e) =>
      for {
        st <- sts
        v <- eval(e, st)
      } yield st.updated(x, v)
    case SSeq(s1, s2) => eval(s2, eval(s1, sts))
    case SCond(c, s1, s2) =>
      eval(s1, eval(c, sts)) ++ eval(s2, eval(CNot(c), sts))
    case SLoop(c, s) =>
      def F(X: Set[State]): Set[State] = sts ++ eval(s, eval(c, X))
      eval(CNot(c), fixpoint(F)(Set.empty))
    case SNop       => sts
    case SAssert(c) => eval(c, sts)
}
