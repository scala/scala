


object X { def unapplySeq(x: Any): Option[String] = { Some(x.toString.toUpperCase) }}

object Y { def unapplySeq(v: Any) = Option((1, 2, 3)) }

object Test extends App {
  def f(x: Any) = x match { case X(y, z) => }
  def g0(x: Any) = x match { case Y() => }
  def g1(x: Any) = x match { case Y(y) => }
  def g2(x: Any) = x match { case Y(y,z) => }
}
