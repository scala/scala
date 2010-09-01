object Test {
  def f[T](xs: T*) = ()

  val x = "abc"

  f[x.type](x)
}