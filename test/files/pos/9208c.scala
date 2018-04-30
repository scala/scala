
import scala.language.implicitConversions

class C {
  implicit def s1(s: String): T = new T { override def t = s }
  implicit def t: T = new T { override def t = "t1" }
}
class D extends C {
  implicit def s1(i: Int): T = new T { override def t = i.toString }
  implicit override def t: T = new T { override def t = "t2" }
  //override def t: T = new T { override def t = "t2" }   // defeats the implicit
  def d: String = "hello".t
  def g(f: String => T): String = f("world").t
  def f: String = g(s1)
  def tly: String = implicitly[T].t
}

trait T {
  def f = implicitly[Ordering[Int]]
  def t: String
}

// similar to mutable.TreeMap
// where naively Y.x didn't shadow X.x
class X[A]()(implicit val x: Ordering[A]) {
  class Y extends X {
    def f = implicitly[Ordering[A]]
  }
}

object Test extends App {
  val x = new X[Int]
  println(new x.Y().f)

  val d = new D
  println(d.d)
  println(d.f)
  println(d.tly)
}

