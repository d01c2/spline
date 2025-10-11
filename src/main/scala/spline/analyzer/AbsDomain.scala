package spline.analyzer

import spline.frontend.*

trait Lattice[Elem] {
  extension (x: Elem) {
    // partial order
    def ⊑(y: Elem): Boolean

    // join and meet operations
    def ⊔(y: Elem): Elem
    def ⊓(y: Elem): Elem
  }
}

trait AbsDomain[Elem] extends Lattice[Elem] {
  // bottom and top elements
  def bot: Elem
  def top: Elem

  // monotonic concretization
  def gamma(x: Elem): Set[Value]

  extension (x: Elem) {
    // widening operation
    def ▽(y: Elem): Elem
  }
}
