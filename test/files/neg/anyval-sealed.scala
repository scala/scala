class A {
  def f(x: AnyVal) = x match {
    case _: Boolean => 1
    case _: Int     => 2
  }
}