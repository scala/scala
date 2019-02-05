object Foo {
  def foo[A <: Product](a: A): Unit = { type X = a.type }
}
