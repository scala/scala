package test;

class A;

class B extends A {
  def foo: int = 1;
}

object B {
  def view(x: B): B1 = null;
}

class B1 {
  def bar: int = 1
}

object C {
  implicit def view(x: A): B1 = null;
}
object Test {
  import C.view;

  val b: B = null;

  System.out.println(b.bar);
}
