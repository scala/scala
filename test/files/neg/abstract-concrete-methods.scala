trait Outer[This <: Outer[This]] {
  self: This =>

  trait Inner
  def score(i: This#Inner): Double
}
class Outer2 extends Outer[Outer2] {
  class Inner extends super.Inner
  def score(i: Outer2#Inner) = 0.0
}
