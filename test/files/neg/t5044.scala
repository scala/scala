class T {
  def foo[T](id: T) = 0
  def m(a: Int) = 0

  def f {
    val a = foo(id = 1)
    val id = m(a)
  }
}
