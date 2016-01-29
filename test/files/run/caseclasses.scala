case class Foo(x: Int)(y: Int)

case class Bar()

abstract class Base
abstract case class Abs(x: Int) extends Base

object M {
  abstract case class C(x: String) {}
  object C extends (String => C) {
    def apply(x: String): C = {
      println("creating C("+x+")")
      new C(x) {}
    }
  }
}

object Test extends App {

  def Abs(x: Int) = new Abs(x * 2){}
  (Abs(2): @unchecked) match {
    case Abs(4) => ;
  }

  def fn[a,b](x: a => b) = x;
  val f = fn(Foo(1))
  (f(2): AnyRef) match {
    case Foo(1) => Console.println("OK")
    case Bar() => Console.println("NO")
  }
  try {
    Bar() productElement 3
    throw new NullPointerException("duh")
  } catch {
    case x:IndexOutOfBoundsException =>
  }

  M.C("hi") match {
    case M.C("hi") => println("OK")
    case _ => println("NO")
  }

  try {
    f(2) productElement 3
    throw new NullPointerException("duh")
  } catch {
    case x:IndexOutOfBoundsException =>
  }

}

