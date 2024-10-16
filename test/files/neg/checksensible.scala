//> using options -Werror -deprecation
//
final class Bip { def <=(other: Bop) = true }
final class Bop { }
object Bep { }

final class Zing {
  def !=(other: Zing) = false
}

// 7 warnings
class RefEqTest {
  object Shmoopie

  (new AnyRef) eq (new AnyRef)         // w
  (new AnyRef) ne (new AnyRef)         // w
  Shmoopie eq (new AnyRef)             // w
  (Shmoopie: AnyRef) eq (new AnyRef)   // w
  (new AnyRef) eq Shmoopie             // w
  (new AnyRef) eq null                 // w
  null eq new AnyRef                   // w
}

// 13 warnings
class EqEqValTest {
  var c = 0

  (c = 1) == 0 // w
  0 == (c = 1) // w

  1 == "abc"        // w
  1 == ("abc": Any) // n: Any may be a boxed Int
  1 == (1: Any)     // n: as above
  "abc" == 1        // w: string equality is known
  Some(1) == 1      // w: case class equals

  true == java.lang.Boolean.valueOf(true) // n
  java.lang.Boolean.valueOf(true) == true // n

  new AnyRef == 1                      // w: fresh object
  1 == (Integer.valueOf(1): AnyRef)    // n: `AnyRef` could be an Integer, which is handled by cooperative equality
  1 == java.lang.Integer.valueOf(1)    // n: cooperative equality (BoxesRunTime)
  1 == java.lang.Boolean.valueOf(true) // w

  1 != true                          // w
  () == true                         // w
  () == ()                           // w
  () == println()                    // w
  () == scala.runtime.BoxedUnit.UNIT // w
  scala.runtime.BoxedUnit.UNIT != () // w
  (scala.runtime.BoxedUnit.UNIT: java.io.Serializable) != () // n

  (1 != println()) // w
  (1 != 'sym)      // w
}

// 12 warnings
class EqEqRefTest {
  val ref = new Bop
  ((x: Int) => x + 1) == null   // w (fresh object)
  Bep == ((_: Int) + 1)         // w (fresh object)

  new Object == new Object           // w
  new Object == "abc"                // w
  new Exception() != new Exception() // w

  val foo: Array[String] = Array("1","2","3")
  if (foo.length == null) "plante" else "plante pas" // w

  // final classes with default equals
  val x1 = new Bip
  val x2 = new Bop
  (x1 == x2) // w

  class C1 { }
  class C2 extends C1 { }
  final class Z1 extends C2 { }
  final class C3 extends C2 { def !=(other: Z1) = false }
  val z1 = new Z1
  val c3 = new C3

  // these should always warn
  c3 == z1    // w
  z1 == c3    // w
  z1 != c3    // w
  c3 != "abc" // w

  c3 != z1    // n: method != is overridden

  // non-warners
  (null: AnyRef) == (null: AnyRef) // n
  (x1 <= x2)                       // n

  def main(args: Array[String]) = {
    val in = new java.io.FileInputStream(args(0))
    var c = 0
    while ((c = in.read) != -1) // w
      print(c.toChar)

    in.close
  }
}

class AnyEqualsTest {
  1L equals 1 // w: bypasses coopeq
  // ok, because it's between the same numeric types
  1 equals 1 // n
  // ok
  1L equals "string" // n
  // ok
  1L.equals(()) // n
  (1L: Any) equals 1              // w: bypasses coopeq
  (1L: AnyVal) equals 1           // w: bypasses coopeq
  (1L: AnyVal) equals (1: AnyVal) // w: bypasses coopeq
  // ok
  "string" equals 1 // n
  def foo[A](a: A) = a.equals(1)  // w: bypasses coopeq
  // ok
  def bar[A <: AnyRef](a: A) = a.equals(1) // n
}

object eq_refine {
  class E
  class SE extends Serializable
  val e = new E
  if (e == "") ??? // warn about comparing unrelated types

  val se = new SE
  if (se == "") ??? // types are still unrelated
}
