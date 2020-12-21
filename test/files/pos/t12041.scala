object Test {
  def foo(xs: Int*) = new util.Random().shuffle(xs)
  def dup[T](x: T)(y: x.type) = (x, y)
  def bar(xs: Int*) = dup(xs)(xs)
}