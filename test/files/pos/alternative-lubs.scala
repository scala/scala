sealed trait A
sealed trait B extends A
case class B1(i: Int) extends B
case class B2(i: Int) extends B

object Test {
  val b1: B1 = B1(0)
  val b2: B2 = B2(0)

  def takesB(b: B): Int = 10
  def takesB1(it: b1.type): Int = 11

  def foo0(a: A) = a match {
    case b @ (`b1` | `b2`) => takesB(b)
    case b @ (_ : `b1`.type | _ : `b2`.type) => takesB(b)
    case b @ (_ : `b1`.type | _ : `b1`.type) => takesB1(b)
    case b @ (B1(_) | B2(_)) => takesB(b)
    case _ => 20
  }

  // weak subtyping, fun...
  def foo1(a: Option[Any]) = (a : @unchecked) match { case Some(x @ (1 | 1)) => x }
  def foo2(a: Option[Any]) = (a : @unchecked) match { case Some(x @ (1 | 1L)) => x }

  foo1(None) : Int
  foo2(None) : Long
}
