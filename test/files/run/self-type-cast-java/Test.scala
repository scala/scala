// A trait's `this` is cast to its self type's class only if `invokevirtual` on that class resolves to the selected
// member. Here the members are abstract, so the class has no mixin forwarders for them, and resolution would stop at
// a private or a static method of the same name in a Java superclass.

trait D { def m(): String; def s(): String }
trait T { self: U => def viaThis = m() + s() }
abstract class U extends J with D with T
class C extends U { def m() = "C.m"; def s() = "C.s" }

object Test {
  def main(args: Array[String]): Unit = assert(new C().viaThis == "C.mC.s")
}
