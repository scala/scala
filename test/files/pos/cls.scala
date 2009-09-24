import scala._;

package scalac.util {

class A[X1, X2](x1: X1, x2: X2) {}
class B[Y](y1: Y, y2: Y) extends A[Y, Y](y1, y2) {
  def f(x: Y, xs: B[Y]): Unit = {}
  def g() = f(y1, this);
}

object test {
  val b: B[Int] = new B[Int](1, 2);
  val a: A[Int, Int] = b;
  val a1 = new A(1, "hello");
  val b1 = new B(1, "hello");
}
}
