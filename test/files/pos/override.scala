trait A extends Object {
  def f = 1;
  val x: A;
}

trait B extends Object {
  def f = 2;
}

trait C extends Object with A with B {
  override def f = super[B].f;
  val a: A;
  val x: a.type = a;
}
