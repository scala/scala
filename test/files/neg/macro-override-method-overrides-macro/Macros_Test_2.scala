class B {
  def foo(x: String) = macro Impls.fooBString
  def foo(x: Int) = macro Impls.fooBInt
  def foo(x: Boolean) = println("fooBBoolean")
}

class D extends B {
  override def foo(x: String) = println("fooDString")
  override def foo(x: Int) = macro Impls.fooDInt
}

class Z extends D {
  override def foo(x: String) = macro Impls.fooZString
  override def foo(x: Boolean) = println("fooZBoolean")
}
