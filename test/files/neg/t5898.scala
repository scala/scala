//> using options -Xlint:valpattern -Xfatal-warnings
//
sealed trait T
case class C(i: Int) extends T
case class D(i: Int) extends T

trait Test {
  def t: T = C(42)
  def f() = t match { case x: D => ??? }
  val D(x) = t
  val D(y) = (null: Any)
}
