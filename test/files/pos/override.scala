trait A extends Object {
  def f = 1
}

trait B extends Object {
  def f = 2
}

trait C extends Object with A with B {
  override def f = super.f
}
