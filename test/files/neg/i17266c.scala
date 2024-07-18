//> using options -Werror -Xlint:universal-methods

object X

class A(val s: String) extends AnyVal {
  import X._
  def f = eq("hello, world")
  def g = synchronized { println("hello, world") }
}
class B(val s: String) extends AnyVal {
  import Predef._
  def f = eq(s)
  def g = synchronized { println(s) }
}
package object p
class C(val s: String) extends AnyVal {
  import p.`package`._
  def f = eq(s)
  def g = synchronized { println(s) }
}
class Y(val s: String) {
  def f = X.eq("hello, world")
  def g = X.synchronized { println("hello, world") }
}
class Z(val s: String) {
  import X._
  def f = eq("hello, world")
  def g = synchronized { println("hello, world") }
}
