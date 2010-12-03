object Error {
  def f {
    case class X(b: Boolean = false)
    val r = X()
  }
  def g = {
    val x = 0
    var y = 1 // no constant type
    def foo(z: Int = y) = 1
    val z = 2
    foo()
  }
}
