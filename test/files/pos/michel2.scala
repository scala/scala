trait A extends Object {
  def f : Int = 1
}

trait B extends Object with A {
  override def f : Int = super.f
}