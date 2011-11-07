import scala._;

package scalac.util {

trait A {
  type X1;
  type X2;
  val x1: X1;
  val x2: X2;
}
trait B extends A {
  type Y;
  val y1, y2: Y;
  type X1 = Y;
  type X2 = Y;
  val x1 = y1;
  val x2 = y2;
  def f(x: Y, xs: B): Unit = {}
  def g() = f(y1, this);
}

object test {
  val b: B { type Y = Int } = new B {
    type Y = Int;
    val y1, y2 = 1;
  }
  val a: A { type X1 = Int; type X2 = Int } = b;
  val a1 = new A { 
    type X1 = Int;
    type X2 = String; 
    val x1 = 1;
    val x2 = "hello"
  }
  val b1 = new B {
    type Y = Any;
    val y1 = 1;
    val y2 = "hello";
  }
}
}
