class A {
  private[this] def f1(): Unit = {}
  protected[this] def f2(): Unit = {}
  private[A] def f3(): Unit = {}
}
class B extends A {
  f1()
  super.f1()
  def otherb(b2: B) = b2.f2()
  f3()
  super.f3()
}
