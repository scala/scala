class Foo extends Function2[Int, Int, Int] {
  def apply(x: Int, y: Int) = x + y
  override def tupled: (Int, Int) => Int = super.tupled
}
