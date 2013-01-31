class Foo_1 {
  def foo(x: AnyRef): Int = {
    val bool = x == null
    if (x != null)
      1
    else
      0
  }
}
