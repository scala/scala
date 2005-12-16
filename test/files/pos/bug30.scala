trait A {
  def f(x: int): unit;
  def f(x: String): unit;
}

class B extends A {
  def f(x: int): unit = ();
  def f(x: String): unit = ();
}
