object Test {
  type Id[T] = T
  def foo(x: Int): Id[x.type] = x

  lazy val result = foo(1)
}
