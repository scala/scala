//> using options -unchecked
import scala.util.Try

object Test {
  class O(val i: Int) {
    class A {
      val aOuter = i
    }

    class B1 extends A {
      val b1Outer = i
    }
  }
  class M(i: Int) extends O(i) {
    class B2 extends m2.A {
      val b2Outer = i
    }

    def pat1(a: M.this.A) = a match {
      case b: M.this.B1 => // can elide outer check, (a : m1.A) && (a : O#B1) implies (a : m1.B1)
        assertOuter(m1.i, b.b1Outer)
        true
      case _ =>
        false
    }
    def pat2(a: m2.A) = a match {
      case b: M.this.B2 => // needs runtime outer check
        assertOuter(m1.i, b.b2Outer)
        true
      case _ =>
        false
    }
    def pat3(a: M.this.B1) = a match {
      case b: M.this.A => // can elide outer check, (a : m1.B1) && (a : O#A) implies (a : m1.B1)
        assertOuter(m1.i, b.aOuter)
        true
      case _ =>
        false
    }
    def pat4(a: M.this.B2) = a match {
      case b: m2.A => // can elide outer check, (a : m1.B2) implies (a : m2.A)
        assertOuter(m2.i, b.aOuter)
        true
      case _ =>
        false
    }
  }

  val m1 = new M(1);
  val m2 = new M(2);

  def pat1(a: m1.A) = a match {
    case b: m1.B1 => // can elide outer check, (a : m1.A) && (a : O#B1) implies (a : m1.B1)
      assertOuter(m1.i, b.b1Outer)
      true
    case _ =>
      false
  }
  def pat2(a: m2.A) = a match {
    case b: m1.B2 => // needs runtime outer check
      assertOuter(m1.i, b.b2Outer)
      true
    case _ =>
      false
  }
  def pat3(a: m1.B1) = a match {
    case b: m1.A => // can elide outer check, (a : m1.B1) && (a : O#A) implies (a : m1.B1)
      assertOuter(m1.i, b.aOuter)
      true
    case _ =>
      false
  }
  def pat4(a: m1.B2) = a match {
    case b: m2.A => // can elide outer check, (a : m1.B2) implies (a : m2.A)
      assertOuter(m2.i, b.aOuter)
      true
    case _ =>
      false
  }

  def pat5(a: M#B2) = a match {
    case b: m2.A => // can elide outer check, (a : A#B2) implies (a : m2.A)
      assertOuter(m2.i, b.aOuter)
      true
    case _ =>
      false
  }

  trait ScalaProvider { def loader: Int }
  type ScalaProvider2 = { def loaderLibraryOnly: Int }
  import scala.language.reflectiveCalls

  def cb1400(provider: ScalaProvider) = try {
    provider match {
      case p: ScalaProvider2 @unchecked => p.loaderLibraryOnly
    }
  } catch {
    case _: NoSuchMethodException => provider.loader
  }

  def assertOuter(expected: Int, actual: Int): Unit = {
    if (expected != actual) throw WrongOuter(expected, actual)
  }
  case class WrongOuter(expected: Int, actual: Int) extends RuntimeException(s"expected: $expected, actual: $actual")

  def main(args: Array[String]): Unit = {
    assert(pat1(new m1.B1))
    assert(m1.pat1(new m1.B1))
    assert(Try(pat1((new m2.B1).asInstanceOf[m1.B1])).failed.get == WrongOuter(m1.i, m2.i))
    assert(Try(m1.pat1((new m2.B1).asInstanceOf[m1.B1])).failed.get == WrongOuter(m1.i, m2.i))

    assert(!pat2(new m2.B2))
    assert(!m1.pat2(new m2.B2))
    assert(pat2(new m1.B2))
    assert(m1.pat2(new m1.B2))

    assert(pat3(new m1.B1))
    assert(m1.pat3(new m1.B1))
    assert(Try(pat3((new m2.B1).asInstanceOf[m1.B1])).failed.get == WrongOuter(m1.i, m2.i))
    assert(Try(m1.pat3((new m2.B1).asInstanceOf[m1.B1])).failed.get == WrongOuter(m1.i, m2.i))

    assert(pat4(new m1.B2))
    assert(m1.pat4(new m1.B2))
    assert(pat4((new m2.B2).asInstanceOf[m1.B2]))
    assert(m1.pat4((new m2.B2).asInstanceOf[m1.B2]))

    assert(pat5(new m1.B2))
    assert(pat5(new m2.B2))

    class SP1 extends ScalaProvider { def loader = 1 }
    class SP2 extends ScalaProvider { def loader = 1; def loaderLibraryOnly = 2 }
    assert(cb1400(new SP1()) == 1)
    assert(cb1400(new SP2()) == 2)
  }
}
