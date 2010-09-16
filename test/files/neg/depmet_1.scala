object Test {
  def precise0(y: x.type)(x: String): Unit = {}
  def precise1(x: String, y: x.type): Unit = {}
  def precise2[T <: y.type](y: String): Unit = {}
}