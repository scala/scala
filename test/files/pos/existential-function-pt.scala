object Test {
  def foo(a: Function[String, _ <: String]): a.type = a
  foo(x => x)
}
