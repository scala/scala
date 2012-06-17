trait A {
  def foo(i: Int)(b: String): Int
  def foo(u: Unit): Int // not reported
  def foo(x: Float): Int // not reported
}
trait B[X] extends A {
  def foo(x: X): Int
  def foo(u: Unit): Int
  final def foo(x: Float): Int = 0 // not reported
}
trait C extends B[Boolean] {
  override def foo(s: String): Int
  val foo = 0 // not reported
}
