// A trait's `this` is cast to its self type's class only if `invokevirtual` on that class resolves to the selected
// member. Here the member is abstract, so the class has no mixin forwarder for it, and resolution would stop at a
// private method of the same name in a superclass.

class P { private def m(): String = "P.m"; def p = m() }
trait D { def m(): String }
trait T { self: U => def viaThis = m() }
abstract class U extends P with D with T
class C extends U { def m() = "C.m" }

// a private overload with a different descriptor does not get in the way
trait D2 { def n(): String }
trait T2 { self: U2 => def viaThis = n() }
abstract class U2 extends D2 with T2 { private def n(x: Int): String = "U2.n"; def u = n(0) }
class C2 extends U2 { def n() = "C2.n" }

object Test {
  def main(args: Array[String]): Unit = {
    assert(new C().viaThis == "C.m")
    assert(new C2().viaThis == "C2.n")
  }
}
