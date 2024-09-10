//> using options -deprecation

import scala.tools.testkit.AssertUtil.assertThrows

abstract case class C1(a: Int)
class C2(a: Int) extends C1(a) { override def productPrefix = "C2" }
class C3(a: Int) extends C1(a) { override def productPrefix = "C3" }

case class VCC(x: Int) extends AnyVal

object Test extends App {
  val c2 = new C2(1)
  val c3 = new C3(1)
  assert(c2 != c3)
  assert(c2.hashCode != c3.hashCode)
  assert(!c2.canEqual(c3))

  // should be true -- scala/bug#13034
  assert(!VCC(1).canEqual(VCC(1)))
  // also due to scala/bug#13034
  assertThrows[ClassCastException](VCC(1).canEqual(1))
}
