package test;

object Test extends App {
  type Bar;
  trait FooImpl;

  trait Bob {
    def bar : Bar with FooImpl;
  }
  def ifn[A,B](a : A)(f : A => B) =
    if (a != null) f(a) else null;

  val bob : Bob = null;
  val bar = ifn(bob)(_.bar);
  assert(bar == null);
}
