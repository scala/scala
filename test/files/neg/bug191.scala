class A {
  private class C {
    def print = Console.println("A.C");
  }
  def foo(c: C) = c.print;
}
class B extends A {
  class C {
    def show = Console.println("B.C");
  }
  foo(new C);
}
object Main with Application {
  val b = new B;
}
