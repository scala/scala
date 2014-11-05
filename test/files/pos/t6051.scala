object Foo1 {
  def foo(x: Int, y: Int = 10) = x*y
  lazy val y = foo(x = 20)
}

object Foo2 {
  def foo(x: Int, y: Int = 10) = x*y
  val y = foo(x = 20)
}

object Foo3 {
  def foo(x: Int, y: Int = 10) = x*y
  def y = foo(x = 20)
}

object Foo4 {
  def foo(x: Int, y: Int = 10) = x*y
  var y = foo(x = 20)
}
