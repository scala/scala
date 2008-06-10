trait A {
  def f(x: Int): Unit;
  def f(x: String): Unit;
}

class B extends A {
  def f(x: Int): Unit = ();
  def f(x: String): Unit = ();
}
