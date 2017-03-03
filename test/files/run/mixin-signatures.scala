trait Base[T, R] {
  def f(x: T): R
  def g(x: T): R
  def h(x: T): R = null.asInstanceOf[R]
}

trait Foo1[T] extends Base[T, String] {
  def f(x: T): String = null
  def g(x: T): String
}
trait Foo2[R] extends Base[String, R] {
  def f(x: String): R = { print(x.length) ; null.asInstanceOf[R] }
  def g(x: String): R
}
abstract class Foo3[T] extends Base[T, String] {
  def f(x: T): String = ""
  def g(x: T): String
}
abstract class Foo4[R] extends Base[String, R] {
  def f(x: String): R = { print(x.length) ; null.asInstanceOf[R] }
  def g(x: String): R
}

object Test {
  object bar1 extends Foo1[String] { def g(x: String): String = { print(x.length) ; "" } }
  object bar2 extends Foo2[String] { def g(x: String): String = { print(x.length) ; "" } }
  object bar3 extends Foo3[String] { def g(x: String): String = { print(x.length) ; "" } }
  object bar4 extends Foo4[String] { def g(x: String): String = { print(x.length) ; "" } }

  // Notice that in bar5, f and g require THREE bridges, because the final
  // implementation is (String)String, but:
  //
  //   inherited abstract signatures: T(R), (T)String, and (String)R
  //   which erase to: (Object)Object, (Object)String, and (String)Object
  //
  // each of which must be bridged to the actual (String)String implementation.
  //
  // public java.lang.String Test$bar5$.g(java.lang.String)
  // public java.lang.Object Test$bar5$.g(java.lang.String) <bridge> <synthetic>
  // public java.lang.Object Test$bar5$.g(java.lang.Object) <bridge> <synthetic>
  // public java.lang.String Test$bar5$.g(java.lang.Object) <bridge> <synthetic>
  object bar5 extends Foo1[String] with Foo2[String] {
    override def f(x: String): String = { print(x.length) ; x }
    def g(x: String): String = { print(x.length) ; x }
  }

  final def m1[T, R](x: Base[T, R], y: T)   = { x.f(y) ; x.g(y) ; x.h(y) }
  final def m2[T](x: Base[T, String], y: T) = { x.f(y) ; x.g(y) ; x.h(y) }
  final def m3[R](x: Base[String, R])       = { x.f("") ; x.g("") ; x.h("") }
  final def m4(x: Base[String, String])     = { x.f("") ; x.g("") ; x.h("") }

  final def m11[T](x: Foo1[T], y: T) = { x.f(y) ; x.g(y) ; x.h(y) }
  final def m12(x: Foo1[String])     = { x.f("") ; x.g("") ; x.h("") }
  final def m21[T](x: Foo2[T], y: T) = { x.f("") ; x.g("") ; x.h("") }
  final def m22(x: Foo2[String])     = { x.f("") ; x.g("") ; x.h("") }
  final def m31[T](x: Foo3[T], y: T) = { x.f(y) ; x.g(y) ; x.h(y) }
  final def m32(x: Foo3[String])     = { x.f("") ; x.g("") ; x.h("") }
  final def m41[T](x: Foo4[T], y: T) = { x.f("") ; x.g("") ; x.h("") }
  final def m42(x: Foo4[String])     = { x.f("") ; x.g("") ; x.h("") }

  def go = {
    m1(bar1, "") ; m2(bar1, "") ; m3(bar1) ; m4(bar1)
    m1(bar2, "") ; m2(bar2, "") ; m3(bar2) ; m4(bar2)
    m1(bar3, "") ; m2(bar3, "") ; m3(bar3) ; m4(bar3)
    m1(bar4, "") ; m2(bar4, "") ; m3(bar4) ; m4(bar4)

    m11(bar1, "") ; m12(bar1)
    m21(bar2, "") ; m22(bar2)
    m31(bar3, "") ; m32(bar3)
    m41(bar4, "") ; m42(bar4)
    ""
  }

  def flagsString(m: java.lang.reflect.Method) = {
    val str = List(
      if (m.isBridge) "<bridge>" else "",
      if (m.isSynthetic) "<synthetic>" else ""
    ) filterNot (_ == "") mkString " "

    if (str == "") "" else " " + str
    //
    // val flags = scala.reflect.internal.ClassfileConstants.toScalaMethodFlags(m.getModifiers())
    // scala.tools.nsc.symtab.Flags.flagsToString(flags)
  }

  def show(clazz: Class[_]) {
    print(clazz + " {")
    clazz.getMethods.sortBy(x => (x.getName, x.isBridge, x.toString)) filter (_.getName.length == 1) foreach { m =>
      print("\n  " + m + flagsString(m))
      if ("" + m != "" + m.toGenericString) {
        print("\n    generic: " + m.toGenericString)
      }
    }
    println("\n}")
    println("")
  }
  def show(x: AnyRef) { show(x.getClass) }
  def show(x: String) { show(Class.forName(x)) }

  def main(args: Array[String]): Unit = {
    List(bar1, bar2, bar3, bar4, bar5) foreach show
    List("Foo1", "Foo2") foreach show
    println(go)
  }
}
