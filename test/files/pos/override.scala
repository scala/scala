trait A extends AnyRef {
  def f = 1;
  val x: A;
}

trait B extends AnyRef {
  def f = 2;
}

trait C extends AnyRef with A with B {
  override def f = super[B].f;
  val a: A;
  val x: a.type = a;
}
