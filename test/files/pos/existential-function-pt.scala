object Test {
  def foo(a: Function[String, _ <: String]): a.type = a
  foo(x => x)

  def foo(a: PartialFunction[String, _ <: String]): a.type = a
  foo({ case x => x })
}
