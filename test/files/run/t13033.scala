//> using options -deprecation

import scala.util.hashing.MurmurHash3.caseClassHash

case class C1(a: Int)
class C2(a: Int) extends C1(a) { override def productPrefix = "C2" }
class C3(a: Int) extends C1(a) { override def productPrefix = "C3" }
case class C4(a: Int) { override def productPrefix = "Sea4" }
case class C5()
case object C6
case object C6b { override def productPrefix = "Sea6b" }
case class C7(s: String) // hashCode forwards to ScalaRunTime._hashCode if there are no primitives
class C8(s: String) extends C7(s) { override def productPrefix = "C8" }

case class VCC(x: Int) extends AnyVal

object Test extends App {
  val c1 = C1(1)
  val c2 = new C2(1)
  val c3 = new C3(1)
  assert(c1 == c2)
  assert(c2 == c1)
  assert(c2 == c3)
  assert(c1.hashCode == c2.hashCode)
  assert(c2.hashCode == c3.hashCode)

  assert(c1.hashCode == caseClassHash(c1))
  // `caseClassHash` mixes in the `productPrefix.hashCode`, while `hashCode` mixes in the case class name statically
  assert(c2.hashCode != caseClassHash(c2))
  assert(c2.hashCode == caseClassHash(c2, c1.productPrefix))

  val c4 = C4(1)
  assert(c4.hashCode != caseClassHash(c4))
  assert(c4.hashCode == caseClassHash(c4, "C4"))

  assert((1, 2).hashCode == caseClassHash(1 -> 2))
  assert(("", "").hashCode == caseClassHash("" -> ""))

  assert(C5().hashCode == caseClassHash(C5()))
  assert(C6.hashCode == caseClassHash(C6))
  assert(C6b.hashCode == caseClassHash(C6b, "C6b"))

  val c7 = C7("hi")
  val c8 = new C8("hi")
  assert(c7.hashCode == caseClassHash(c7))
  assert(c7 == c8)
  assert(c7.hashCode == c8.hashCode)
  assert(c8.hashCode != caseClassHash(c8))
  assert(c8.hashCode == caseClassHash(c8, "C7"))


  // should be true -- scala/bug#13034
  assert(!VCC(1).canEqual(VCC(1)))
  // also due to scala/bug#13034
  assert(VCC(1).canEqual(1))
}
