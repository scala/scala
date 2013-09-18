trait A {
  lazy val z1 = {
    println("<forced z1>")
    "lazy z1"
  }
}

/** Simple class which mixes in one lazy val. */
class Cls extends AnyRef with A {
  override def toString =
    "z1 = " + z1
}

/** Own lazy val + one mixed in. */
class Cls2 extends AnyRef with A {
  lazy val z2 = {
    println("<forced z2>")
    "lazy z2"
  }

  override def toString =
    "z1 = " + z1 + " z2 = " + z2
}

trait B extends A {
  lazy val zb1 = {
    println("<forced zb1>")
    "lazy zb1"
  }
}

class ClsB extends Object with B {
  lazy val zc1 = {
    println("<forced zc1>")
    "lazy zc1"
  }
  override def toString =
    "z1 = " + z1 + " zb1 = " + zb1 + " zc1 = " + zc1
}

/** Class with 32 lazy fields mixes in one more. */
class OverflownLazyFields extends Object with A {
  lazy val zc00 = { println("<forced zc00>"); "lazy zc00" }
  lazy val zc01 = { println("<forced zc01>"); "lazy zc01" }
  lazy val zc02 = { println("<forced zc02>"); "lazy zc02" }
  lazy val zc03 = { println("<forced zc03>"); "lazy zc03" }
  lazy val zc04 = { println("<forced zc04>"); "lazy zc04" }
  lazy val zc05 = { println("<forced zc05>"); "lazy zc05" }
  lazy val zc06 = { println("<forced zc06>"); "lazy zc06" }
  lazy val zc07 = { println("<forced zc07>"); "lazy zc07" }
  lazy val zc08 = { println("<forced zc08>"); "lazy zc08" }
  lazy val zc09 = { println("<forced zc09>"); "lazy zc09" }
  lazy val zc10 = { println("<forced zc10>"); "lazy zc10" }
  lazy val zc11 = { println("<forced zc11>"); "lazy zc11" }
  lazy val zc12 = { println("<forced zc12>"); "lazy zc12" }
  lazy val zc13 = { println("<forced zc13>"); "lazy zc13" }
  lazy val zc14 = { println("<forced zc14>"); "lazy zc14" }
  lazy val zc15 = { println("<forced zc15>"); "lazy zc15" }
  lazy val zc16 = { println("<forced zc16>"); "lazy zc16" }
  lazy val zc17 = { println("<forced zc17>"); "lazy zc17" }
  lazy val zc18 = { println("<forced zc18>"); "lazy zc18" }
  lazy val zc19 = { println("<forced zc19>"); "lazy zc19" }
  lazy val zc20 = { println("<forced zc20>"); "lazy zc20" }
  lazy val zc21 = { println("<forced zc21>"); "lazy zc21" }
  lazy val zc22 = { println("<forced zc22>"); "lazy zc22" }
  lazy val zc23 = { println("<forced zc23>"); "lazy zc23" }
  lazy val zc24 = { println("<forced zc24>"); "lazy zc24" }
  lazy val zc25 = { println("<forced zc25>"); "lazy zc25" }
  lazy val zc26 = { println("<forced zc26>"); "lazy zc26" }
  lazy val zc27 = { println("<forced zc27>"); "lazy zc27" }
  lazy val zc28 = { println("<forced zc28>"); "lazy zc28" }
  lazy val zc29 = { println("<forced zc29>"); "lazy zc29" }
  lazy val zc30 = { println("<forced zc30>"); "lazy zc30" }
  lazy val zc31 = { println("<forced zc31>"); "lazy zc31" }

  override def toString =
    "\nzc00 = " + zc00 +
    "\nzc01 = " + zc01 +
    "\nzc02 = " + zc02 +
    "\nzc03 = " + zc03 +
    "\nzc04 = " + zc04 +
    "\nzc05 = " + zc05 +
    "\nzc06 = " + zc06 +
    "\nzc07 = " + zc07 +
    "\nzc08 = " + zc08 +
    "\nzc09 = " + zc09 +
    "\nzc10 = " + zc10 +
    "\nzc11 = " + zc11 +
    "\nzc12 = " + zc12 +
    "\nzc13 = " + zc13 +
    "\nzc14 = " + zc14 +
    "\nzc15 = " + zc15 +
    "\nzc16 = " + zc16 +
    "\nzc17 = " + zc17 +
    "\nzc18 = " + zc18 +
    "\nzc19 = " + zc19 +
    "\nzc20 = " + zc20 +
    "\nzc21 = " + zc21 +
    "\nzc22 = " + zc22 +
    "\nzc23 = " + zc23 +
    "\nzc24 = " + zc24 +
    "\nzc25 = " + zc25 +
    "\nzc26 = " + zc26 +
    "\nzc27 = " + zc27 +
    "\nzc28 = " + zc28 +
    "\nzc29 = " + zc29 +
    "\nzc30 = " + zc30 +
    "\nzc31 = " + zc31 +
    "\nz1 = " + z1
}

trait PrivateLazy {
  private lazy val str = "z1"
}

/** Test successful compilation. */
class InheritPrivateLazy extends AnyRef with PrivateLazy {}

/** Test successful compilation, see bug #1287. */
trait LocalLazyVal {
  def foo = {
    lazy val next = 10 + 1
    next
  }
}

/** Test successful compilation (see ticket #39) */
package x.y {

  trait Trait {
    lazy val v = 1
  }

  class OuterClass {
    object InnerObject extends Trait
  }
}

/** Test successful compilation of lazy values of type Unit.  */
class UnitLazy extends A {
  lazy val lz = Console.println("UnitLazy.lz forced")
}

trait UnitLazyT {
  lazy val lzt = Console.println("UnitLazyT.lzt forced")
}

class MixedUnitLazy extends UnitLazy with UnitLazyT {
  override def toString() = {
    lz
    lzt
    "MixedUnitLazy"
  }
}

object Test extends App {

  def test(name: String, v: A) {
    println(name + " test:")
    println(v)
    println(v)
    println(v)
  }

  test("Cls", new Cls)
  test("Cls2", new Cls2)
  test("Cls with B", new ClsB)
  test("OverflownLazyFields with A", new OverflownLazyFields)
  test("Unit lazy values", new MixedUnitLazy)
}
