class B {
  def foo(x: String): Unit = macro Impls.fooBString
  def foo(x: Int): Unit = macro Impls.fooBInt
  def foo(x: Boolean): Unit = println("fooBBoolean")
}

class D extends B {
  override def foo(x: String): Unit = println("fooDString")
  override def foo(x: Int): Unit = macro Impls.fooDInt
}

class Z extends D {
  override def foo(x: String): Unit = macro Impls.fooZString
  override def foo(x: Boolean): Unit = println("fooZBoolean")
}
