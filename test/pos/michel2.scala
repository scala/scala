trait A extends Object with {
  def f : Int = 1
}

trait B extends Object with A with {
  override def f : Int = super.f
}