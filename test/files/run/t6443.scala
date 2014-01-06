import scala.language.existentials

class Base
class Derived extends Base

trait A {
  def foo(d: String)(d2: d.type): Base
  val s = ""
  def bar: Unit = foo(s)(s)
}
object B extends A {
  def foo(d: String)(d2: d.type): D forSome { type D <: S; type S <: Derived } = {d2.isEmpty; null} // Bridge method required here!
}

object Test extends App {
  B.bar
}
