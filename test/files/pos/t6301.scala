trait LoadedOver[@specialized(Int) A] {
  def foo(x: Any): A
  def foo(xs: String): A
}

object Test {
  def loaded: AnyRef with LoadedOver[Int] = sys.error("")
  loaded.foo("")
}
