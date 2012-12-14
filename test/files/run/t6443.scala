class Base
class Derived extends Base

trait A {
  def foo(d: AnyRef)(d2: d.type): Base
  def bar: Unit = foo(null)(null)
}
object B extends A {
  def foo(d: AnyRef)(d2: d.type): Derived = null // Bridge method required here!
}

object Test extends App {
  B.bar
}
