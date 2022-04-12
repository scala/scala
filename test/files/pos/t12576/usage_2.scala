
package usage

import test._

case class A()
case class B(i: Int, s: String)
object B {
  //the order matters in this case - if we swap params it works
  def default(s: String = "defaultString", i: Int): B = new B(i, s)
}

// `a` param is necessary to reproduce the issue, but order, in this case, doesn't matter
case class C(b: B, a: A)

object Wrapped {
  val theC = wrapper(B.default(i = 1))
  val expected = (List.empty[Any].flatMap[B]((z: Any) => List({val y = "hi"; val x = 42; B(x,y)}))): List[Any]
}

object Test extends App {
  println {
    Wrapped.theC
  }
}
