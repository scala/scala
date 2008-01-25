case class Foo(x: int)(y: int)

case class Bar

case class Baz(override val x: Int, y: Int) extends Foo(x)(y)

object M {
  abstract case class C(x: String) {}
  object C extends (String => C) {
    def apply(x: String): C = {
      println("creating C("+x+")")
      new C(x) {}
    }
  }
}

object Test extends Application {

  def fn[a,b](x: a => b) = x;
  val f = fn(Foo(1))
  (f(2): AnyRef) match {
    case Foo(1) => Console.println("OK")
    case Bar() => Console.println("NO")
  }
  (Baz(1, 2): AnyRef) match {
    case Baz(1, 2) => ;
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
