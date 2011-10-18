final class Bip { def <=(other: Bop) = true }
final class Bop { }
object Bep { }

final class Zing {
  def !=(other: Zing) = false
}

// 7 warnings
class RefEqTest {
  object Shmoopie

  (new AnyRef) eq (new AnyRef)
  (new AnyRef) ne (new AnyRef)
  Shmoopie eq (new AnyRef)
  (Shmoopie: AnyRef) eq (new AnyRef)
  (new AnyRef) eq Shmoopie
  (new AnyRef) eq null
  null eq new AnyRef
}

// 13 warnings
class EqEqValTest {
  var c = 0

  (c = 1) == 0
  0 == (c = 1)

  1 == "abc"
  1 == ("abc": Any) // doesn't warn because an Any may be a boxed Int
  1 == (1: Any)     // as above
  "abc" == 1        // warns because the lub of String and Int is Any
  Some(1) == 1      // as above

  true == new java.lang.Boolean(true) // none of these should warn
  new java.lang.Boolean(true) == true

  new AnyRef == 1
  1 == new AnyRef                 // doesn't warn because it could be...
  1 == (new java.lang.Integer(1)) // ...something like this
  1 == (new java.lang.Boolean(true))

  1 != true
  () == true
  () == ()
  () == println
  () == scala.runtime.BoxedUnit.UNIT // these should warn for always being true/false
  scala.runtime.BoxedUnit.UNIT != ()
  (scala.runtime.BoxedUnit.UNIT: java.io.Serializable) != () // shouldn't warn

  (1 != println)
  (1 != 'sym)
}

// 12 warnings
class EqEqRefTest {
  val ref = new Bop
  ((x: Int) => x + 1) == null
  Bep == ((_: Int) + 1)

  new Object == new Object
  new Object == "abc"
  new Exception() != new Exception()

  val foo: Array[String] = Array("1","2","3")
  if (foo.length == null) "plante" else "plante pas"

  // final classes with default equals
  val x1 = new Bip
  val x2 = new Bop
  (x1 == x2)

  class C1 { }
  class C2 extends C1 { }
  final class Z1 extends C2 { }
  final class C3 extends C2 { def !=(other: Z1) = false }
  val z1 = new Z1
  val c3 = new C3

  // these should always warn
  c3 == z1
  z1 == c3
  z1 != c3
  c3 != "abc"
  // this should warn when feeling chatty
  c3 != z1

  // non-warners
  (null: AnyRef) == (null: AnyRef)
  (x1 <= x2)

  def main(args: Array[String]) = {
    val in = new java.io.FileInputStream(args(0))
    var c = 0
    while ((c = in.read) != -1)
      print(c.toChar)

    in.close
  }
}
