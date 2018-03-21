object Test {
  type Id[T] = T
  def foo(x: Int): Id[x.type] = x

  { lazy val result: 1 = foo(1) }
}
