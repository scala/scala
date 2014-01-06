import scala.language.existentials

object Test {
  trait ToS { final override def toString = getClass.getName }

  def f1 = { case class Bar() extends ToS; Bar }
  def f2 = { case class Bar() extends ToS; Bar() }
  def f3 = { class Bar() extends ToS; object Bar extends ToS; Bar }
  def f4 = { class Bar() extends ToS; new Bar() }
  def f5 = { object Bar extends ToS; Bar }
  def f6 = { () => { object Bar extends ToS ; Bar } }
  def f7 = { val f = { () => { object Bar extends ToS ; Bar } } ; f }

  def f8 = { trait A ; trait B extends A ; class C extends B with ToS; new C { } }
  def f9 = { trait A ; trait B ; class C extends B with A with ToS; new C { } }

  def f10 = { class A { type T1 } ; List[A#T1]() }
  def f11 = { abstract class A extends Seq[Int] ; List[A]() }
  def f12 = { abstract class A extends Seq[U forSome { type U <: Int }] ; List[A]() }

  val g1 = { case class Bar() extends ToS; Bar }
  val g2 = { case class Bar() extends ToS; Bar() }
  val g3 = { class Bar() extends ToS; object Bar extends ToS; Bar }
  val g4 = { class Bar() extends ToS; new Bar() }
  val g5 = { object Bar extends ToS; Bar }
  val g6 = { () => { object Bar extends ToS ; Bar } }
  val g7 = { val f = { () => { object Bar extends ToS ; Bar } } ; f }

  val g8 = { trait A ; trait B extends A ; class C extends B with ToS; new C { } }
  val g9 = { trait A ; trait B ; class C extends B with A with ToS; new C { } }

  val g10 = { class A { type T1 } ; List[A#T1]() }
  val g11 = { abstract class A extends Seq[Int] ; List[A]() }
  val g12 = { abstract class A extends Seq[U forSome { type U <: Int }] ; List[A]() }

  def m[T: Manifest](x: T) = println(manifest[T])

  // manifests don't work for f10/g10
  def main(args: Array[String]): Unit = {
    m(f1)
    m(f2)
    m(f3)
    m(f4)
    m(f5)
    m(f6)
    m(f7)
    m(f8)
    m(f9)
    // m(f10)
    m(f11)
    m(f12)
    m(g1)
    m(g2)
    m(g3)
    m(g4)
    m(g5)
    m(g6)
    m(g7)
    m(g8)
    m(g9)
    // m(g10)
    m(g11)
    m(g12)
  }
}

object Misc {
  trait Bippy { def bippy = "I'm Bippy!" }
  object o1 {
    def f1 = { trait A extends Seq[U forSome { type U <: Bippy }] ; abstract class B extends A ; trait C extends B ; (null: C) }
    def f2 = f1.head.bippy
  }
  def g1 = o1.f1 _
  def g2 = o1.f2 _
}
