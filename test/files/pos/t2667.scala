object A {
  def foo(x: Int, y: Int*): Int = 45
  def foo[T](x: T*): Int = 55

  val x: Unit = foo(23, 23f)
}