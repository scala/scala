object Foo {
  def foo[A <: Product](a: A) { type X = a.type }
}
