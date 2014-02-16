trait Base[A, B, C] {
  def f(x: A, y: B, z: C): Unit
  def g(x: A, y: B, z: C) = f(x, y, z)
  def h(x: A, y: B, z: C) = g(x, y, z)
}

trait D1[B, C] extends Base[String, B, C]
trait D2[A, B] extends Base[A, B, String]
trait D3[A, C] extends Base[A, String, C]
trait D4[A] extends Base[A, String, String]
trait D5[B] extends Base[String, B, String]
trait D6[C] extends Base[String, String, C]
trait D7 extends Base[String, String, String]

trait E1[B, C] extends Base[String, B, C] { def f(x: String, y: B, z: C): Unit ; override def h(x: String, y: B, z: C) = g(x, y, z) }
trait E2[A, B] extends Base[A, B, String] { def f(x: A, y: B, z: String): Unit ; override def h(x: A, y: B, z: String) = g(x, y, z) }
trait E3[A, C] extends Base[A, String, C] { def f(x: A, y: String, z: C): Unit ; override def h(x: A, y: String, z: C) = g(x, y, z) }
trait E4[A] extends Base[A, String, String] { def f(x: A, y: String, z: String): Unit ; override def h(x: A, y: String, z: String) = g(x, y, z) }
trait E5[B] extends Base[String, B, String] { def f(x: String, y: B, z: String): Unit ; override def h(x: String, y: B, z: String) = g(x, y, z) }
trait E6[C] extends Base[String, String, C] { def f(x: String, y: String, z: C): Unit ; override def h(x: String, y: String, z: C) = g(x, y, z) }
trait E7 extends Base[String, String, String] { def f(x: String, y: String, z: String): Unit ; override def h(x: String, y: String, z: String) = g(x, y, z) }

trait F1[B, C] extends Base[String, B, C] { def f(x: String, y: B, z: C): Unit = println(x.length) }
trait F2[A, B] extends Base[A, B, String] { def f(x: A, y: B, z: String): Unit = println(z.length) }
trait F3[A, C] extends Base[A, String, C] { def f(x: A, y: String, z: C): Unit  = println(y.length) }
trait F4[A] extends Base[A, String, String] { def f(x: A, y: String, z: String): Unit = println(y.length) }
trait F5[B] extends Base[String, B, String] { def f(x: String, y: B, z: String): Unit = println(x.length) }
trait F6[C] extends Base[String, String, C] { def f(x: String, y: String, z: C): Unit = println(x.length) }
trait F7 extends Base[String, String, String] { def f(x: String, y: String, z: String): Unit = println(x.length) }

abstract class DBag extends D1[String, String] with D2[String, String] with D3[String, String] with D4[String] with D5[String] with D6[String] with D7 {
  def f(x: String, y: String, z: String) = println(x.length + y.length + z.length)
}
abstract class EBag extends E1[String, String] with E2[String, String] with E3[String, String] with E4[String] with E5[String] with E6[String] with E7 {
  def f(x: String, y: String, z: String) = println(x.length + y.length + z.length)
}
abstract class FBag extends F1[String, String] with F2[String, String] with F3[String, String] with F4[String] with F5[String] with F6[String] with F7 {
  override def f(x: String, y: String, z: String) = println(x.length + y.length + z.length)
}

abstract class GBag1[A, B] extends Base[A, B, String] with D2[A, B] {
  def f(x: A, y: B, z: String) = println(z.length)
}
abstract class GBag2[A] extends GBag1[A, String] with D4[A] {
  override def f(x: A, y: String, z: String) = println(z.length)
}
abstract class GBag3 extends GBag2[String] with D7 {
  override def f(x: String, y: String, z: String) = println(z.length)
}
class GBag extends GBag3 with D2[String, String] with D3[String, String] with D4[String] with D5[String] with D6[String] with D7 {
}

object Test {
  def f0(x: Base[String, String, String]) = x.f("a", "b", "c")
  def f1(x: D1[String, String])           = x.f("a", "b", "c")
  def f2(x: D2[String, String])           = x.f("a", "b", "c")
  def f3(x: D3[String, String])           = x.f("a", "b", "c")
  def f4(x: D4[String])                   = x.f("a", "b", "c")
  def f5(x: D5[String])                   = x.f("a", "b", "c")
  def f6(x: D6[String])                   = x.f("a", "b", "c")
  def f7(x: D7)                           = x.f("a", "b", "c")

  def main(args: Array[String]): Unit = {
    val x = new DBag { }
    f0(x)
    f1(x)
    f2(x)
    f3(x)
    f4(x)
    f5(x)
    f6(x)
    f7(x)
  }
}

object TestE {
  def f0(x: Base[String, String, String]) = { x.f("a", "b", "c") ; x.g("a", "b", "c") ; x.h("a", "b", "c") }
  def f1(x: E1[String, String])           = { x.f("a", "b", "c") ; x.g("a", "b", "c") ; x.h("a", "b", "c") }
  def f2(x: E2[String, String])           = { x.f("a", "b", "c") ; x.g("a", "b", "c") ; x.h("a", "b", "c") }
  def f3(x: E3[String, String])           = { x.f("a", "b", "c") ; x.g("a", "b", "c") ; x.h("a", "b", "c") }
  def f4(x: E4[String])                   = { x.f("a", "b", "c") ; x.g("a", "b", "c") ; x.h("a", "b", "c") }
  def f5(x: E5[String])                   = { x.f("a", "b", "c") ; x.g("a", "b", "c") ; x.h("a", "b", "c") }
  def f6(x: E6[String])                   = { x.f("a", "b", "c") ; x.g("a", "b", "c") ; x.h("a", "b", "c") }
  def f7(x: E7)                           = { x.f("a", "b", "c") ; x.g("a", "b", "c") ; x.h("a", "b", "c") }

  def main(args: Array[String]): Unit = {
    val x = new EBag { }
    f0(x)
    f1(x)
    f2(x)
    f3(x)
    f4(x)
    f5(x)
    f6(x)
    f7(x)
  }
}


object TestG {
  def f0(x: Base[String, String, String]) = { x.f("a", "b", "c") ; x.g("a", "b", "c") ; x.h("a", "b", "c") }
  def f1(x: GBag1[String, String])        = { x.f("a", "b", "c") ; x.g("a", "b", "c") ; x.h("a", "b", "c") }
  def f2(x: GBag2[String])                = { x.f("a", "b", "c") ; x.g("a", "b", "c") ; x.h("a", "b", "c") }
  def f3(x: GBag3)                        = { x.f("a", "b", "c") ; x.g("a", "b", "c") ; x.h("a", "b", "c") }

  def main(args: Array[String]): Unit = {
    val x = new GBag { }
    f0(x)
    f1(x)
    f2(x)
    f3(x)
  }
}
