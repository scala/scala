object Test {
  def f1(a: Boolean, b: Boolean) = (a || ???) && (b || ???)
  def f2(a: Boolean, b: Boolean) = (a || ???) && b
  def f3(a: Boolean, b: Boolean) = (a && ???) || b
}
