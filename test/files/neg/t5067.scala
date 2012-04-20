class Foo extends Function3[Int, Int, Int, Int] {
  def apply(x: Int, y: Int, z: Int) = x + y + z
  override def tupled: (Int, Int, Int) => Int = super.tupled
}
