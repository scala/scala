class B {
  def foo(x: String) = macro Impls.fooBString
  def foo(x: Int) = macro Impls.fooBInt
  def foo(x: Boolean) = println("fooBBoolean")
}

class D extends B {
  //override def foo(x: String) = println("fooDString") => method cannot override a macro
  override def foo(x: Int) = macro Impls.fooDInt
}

class Z extends D {
  override def foo(x: String) = macro Impls.fooZString
  override def foo(x: Boolean) = println("fooZBoolean")
}

object Test extends App {

  val dd: D = new D()
  dd.foo("42")
  dd.foo(42)
  dd.foo(true)

  val db: B = new D()
  db.foo("42")
  db.foo(42)
  db.foo(true)

  val zz: Z = new Z()
  zz.foo("42")
  zz.foo(42)
  zz.foo(true)

  val zd: D = new Z()
  zd.foo("42")
  zd.foo(42)
  zd.foo(true)

  val zb: B = new Z()
  zb.foo("42")
  zb.foo(42)
  zb.foo(true)
}