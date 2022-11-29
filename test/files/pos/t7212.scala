
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

trait T { def f: Object }
class K extends T { def f = "" }
object K {
  val k = new K
  val s: Any = k.f
}

trait U extends T { def f = "" }
trait V { var v: Any }
trait W extends V { var v = "" }
