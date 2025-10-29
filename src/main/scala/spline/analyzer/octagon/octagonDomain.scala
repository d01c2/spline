package spline.analyzer.octagon

import spline.analyzer.*
import spline.frontend.*
import spline.utils.*

import spline.analyzer.octagon.*

given OctagonDomain: AbsDomain[Octagon] with {
  def bot = Octagon.makeBot(List())
  def top = Octagon.makeTop(List())

  // Ensure consistent variable ordering
  private def unifyVars(oct1: Octagon, oct2: Octagon): List[String] = {
    (oct1.vars ++ oct2.vars).distinct.sorted
  }

  // Project octagon to a new set of variables
  private def project(oct: Octagon, newVars: List[String]): Octagon = {
    if (newVars == oct.vars) return oct

    val size = newVars.length * 2
    val dbm = Array.fill(size, size)(Infinity(true): Bound)

    for (i <- 0 until size) dbm(i)(i) = 0

    for {
      i <- 0 until size
      j <- 0 until size
      if i != j
    } {
      val vi = newVars(i / 2)
      val vj = newVars(j / 2)

      oct.varIndex(vi).foreach { oi =>
        oct.varIndex(vj).foreach { oj =>
          val oldI = oi * 2 + (i % 2)
          val oldJ = oj * 2 + (j % 2)
          dbm(i)(j) = min(dbm(i)(j), oct.dbm(oldI)(oldJ))
        }
      }
    }

    Octagon(newVars, dbm)
  }

  extension (matrix: Octagon) {
    def isBottom: Boolean = {
      val oct = Octagon.close(matrix)
      // Check for any negative self-loop
      (0 until oct.size).exists(i => oct.dbm(i)(i) < 0)
    }

    def isTop: Boolean = {
      val t = Octagon.makeTop(matrix.vars)
      (0 until matrix.size).forall { i =>
        (0 until matrix.size).forall { j =>
          matrix.dbm(i)(j) >= t.dbm(i)(j)
        }
      }
    }
  }

  def gamma(matrix: Octagon): Set[Value] = {
    error("gamma not implemented for Octagon - infinite/complex domain")
  }

  def alpha(x: Set[Value]): Octagon = {
    if x.isEmpty then bot
    else {
      val vars = List("_alpha")
      val size = 2
      val dbm = Array.fill(size, size)(Infinity(true): Bound)

      for (i <- 0 until size) dbm(i)(i) = 0

      val minVal = x.min
      val maxVal = x.max
      dbm(0)(1) = 2 * maxVal
      dbm(1)(0) = 2 * (-minVal)

      Octagon.close(Octagon(vars, dbm))
    }
  }

  extension (self: Octagon) {
    def ⊑(other: Octagon): Boolean = {
      if (self.isBottom) return true
      if (other.isBottom) return false

      val vars = unifyVars(self, other)
      val s = Octagon.close(project(self, vars))
      val o = Octagon.close(project(other, vars))

      (0 until s.size).forall { i =>
        (0 until s.size).forall { j =>
          s.dbm(i)(j) <= o.dbm(i)(j)
        }
      }
    }

    def ⊔(other: Octagon): Octagon = {
      if (self.isBottom) return other
      if (other.isBottom) return self

      val vars = unifyVars(self, other)
      val s = project(self, vars)
      val o = project(other, vars)

      val size = vars.length * 2
      val dbm = Array.tabulate(size, size) { (i, j) =>
        max(s.dbm(i)(j), o.dbm(i)(j))
      }

      Octagon.close(Octagon(vars, dbm))
    }

    def ⊓(other: Octagon): Octagon = {
      if (self.isBottom || other.isBottom)
        return Octagon.makeBot(unifyVars(self, other))

      val vars = unifyVars(self, other)
      val s = project(self, vars)
      val o = project(other, vars)

      val size = vars.length * 2
      val dbm = Array.tabulate(size, size) { (i, j) =>
        min(s.dbm(i)(j), o.dbm(i)(j))
      }

      val result = Octagon.close(Octagon(vars, dbm))
      if (result.isBottom) Octagon.makeBot(vars) else result
    }

    def ▽(other: Octagon): Octagon = {
      if (self.isBottom) return other

      val vars = unifyVars(self, other)
      val s = project(self, vars)
      val o = project(other, vars)

      val size = vars.length * 2
      val dbm = Array.tabulate(size, size) { (i, j) =>
        if (i == j) 0: Bound
        else if (s.dbm(i)(j) < o.dbm(i)(j)) Infinity(true)
        else s.dbm(i)(j)
      }

      Octagon(vars, dbm)
    }
  }

  def absNeg(matrix: Octagon): AbsValue = {
    matrix
  }

  def absBOp(bop: BOp, oct1: Octagon, oct2: Octagon): AbsValue = {
    oct1 ⊔ oct2
  }
}
