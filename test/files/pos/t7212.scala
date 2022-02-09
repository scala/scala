
// scalac: -Xsource:3

class A {
  def f: Option[String] = Some("hello, world")
  def remove(): Unit = ()
}
class B extends A {
  override def f = None
  override def remove() = ???
}
class C extends B {
  override def f: Option[String] = Some("goodbye, cruel world")
  override def remove(): Unit = println("removed! (not really)")
}
