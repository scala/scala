import scala._;
class m1() {
  def n() = 0;
  def foo(i: Int)(j: Int): Unit = ();
  val bar: Int => Unit = foo(n());
}
