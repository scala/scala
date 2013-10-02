class A(a: Any) {
  def this() = {
    this(b)
    def b = new {}
  }

  def this(x: Int) = {
    this(b)
    lazy val b = new {}
  }

  def this(x: Int, y: Int) = {
    this(b)
    val b = new {}
  }

  def this(x: Int, y: Int, z: Int) = {
    this(b)
    println(".")
    def b = new {}
  }
}
