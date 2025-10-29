package spline.analyzer.octagon

import spline.analyzer.*
import spline.frontend.*
import spline.utils.*
import scala.collection.mutable

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
  def <(other: Bound): Boolean = (self, other) match
    case (Infinity(false), Infinity(false)) => false
    case (Infinity(false), _)               => true
    case (_, Infinity(false))               => false
    case (Infinity(true), _)                => false
    case (_, Infinity(true))                => true
    case (v1: Value, v2: Value)             => v1 < v2
  def <=(other: Bound): Boolean = (self, other) match
    case (Infinity(false), Infinity(false)) => true
    case (Infinity(false), _)               => true
    case (_, Infinity(false))               => false
    case (Infinity(true), _)                => false
    case (_, Infinity(true))                => true
    case (v1: Value, v2: Value)             => v1 <= v2
  def >=(other: Bound): Boolean = other <= self
  def >(other: Bound): Boolean = other < self
}

case class Octagon(
  vars: List[String], // variable names
  dbm: Array[Array[Bound]], // difference bound matrix
) {
  def varIndex(v: String): Option[Int] =
    vars.indexOf(v) match
      case -1 => None
      case i  => Some(i)

  def size: Int = vars.length * 2

  override def toString: String =
    if this.isBottom then "⊥"
    else if this == Octagon.makeTop(vars) then "⊤"
    else {
      val constraints = collection.mutable.ListBuffer[String]()
      for {
        i <- 0 until size
        j <- 0 until size
        if i != j && dbm(i)(j) != Infinity(true)
      } {
        val (vi, signi) =
          if i % 2 == 0 then (vars(i / 2), "") else (vars(i / 2), "-")
        val (vj, signj) =
          if j % 2 == 0 then (vars(j / 2), "-") else (vars(j / 2), "")
        constraints += s"$signi${vi} $signj${vj} ≤ ${dbm(i)(j)}"
      }
      if constraints.isEmpty then "⊤" else constraints.mkString("{", ", ", "}")
    }
}

object Octagon {
  def makeTop(vars: List[String]): Octagon = {
    val size = vars.length * 2
    val dbm = Array.fill(size, size)(Infinity(true): Bound)
    for (i <- 0 until size) dbm(i)(i) = 0
    Octagon(vars, dbm)
  }

  def makeBot(vars: List[String]): Octagon = {
    val size = vars.length * 2
    val dbm = Array.fill(size, size)(Infinity(true): Bound)
    for (i <- 0 until size)
      dbm(i)(i) = -1: Bound // inconsistent: x_i - x_i ≤ -1
    Octagon(vars, dbm)
  }

  // Close the DBM using Floyd-Warshall to enforce transitivity
  def close(oct: Octagon): Octagon = {
    val dbm = oct.dbm.map(_.clone())
    val n = oct.size

    // Floyd-Warshall algorithm
    for {
      k <- 0 until n
      i <- 0 until n
      j <- 0 until n
    } {
      val newBound = dbm(i)(k) + dbm(k)(j)
      if (newBound < dbm(i)(j)) {
        dbm(i)(j) = newBound
      }
    }

    // Strong closure: propagate binary constraints to unary
    for (i <- 0 until n) {
      val i_comp = i ^ 1 // complement: if i is 2k then 2k+1, if 2k+1 then 2k
      val bound = (dbm(i)(i_comp) + dbm(i_comp)(i)) / 2
      if (bound < dbm(i)(i_comp)) {
        dbm(i)(i_comp) = bound
        dbm(i_comp)(i) = bound
      }
    }

    Octagon(oct.vars, dbm)
  }
}
