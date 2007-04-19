case class Foo(x: int)(y: int);

case class Bar;

object Test extends Application {

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

  try {
    f(2) productElement 3
    throw new NullPointerException("duh")
  } catch {
    case x:IndexOutOfBoundsException =>
  }

}
