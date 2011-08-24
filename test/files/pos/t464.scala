class A {
  protected[this] def f() {}
}
class B extends A {
  f()
  super.f()
}
class C extends A {
  override protected[this] def f() = super.f()
}
class D extends C {
  override protected def f() = super.f()
}