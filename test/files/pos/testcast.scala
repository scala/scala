package test;

class A;

class B {
  def foo: int = 1;
}

class B1 {
  def bar: int = 1
}

object C {
  def view(x: B): B1 = null;
}
object Test {
  import C.view;

  val b: B = null;

  System.out.println(C.view(b).bar);
  System.out.println(b.bar);
}
