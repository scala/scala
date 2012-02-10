import scala.tools.partest.ReplTest

object Test extends ReplTest {  
  def code = """
trait ToS { final override def toString = getClass.getName }

// def f1 = { case class Bar() extends ToS; Bar }
def f2 = { case class Bar() extends ToS; Bar() }
def f3 = { class Bar() extends ToS; object Bar extends ToS; Bar }
def f4 = { class Bar() extends ToS; new Bar() }
def f5 = { object Bar extends ToS; Bar }
def f6 = { () => { object Bar extends ToS ; Bar } }
def f7 = { val f = { () => { object Bar extends ToS ; Bar } } ; f }

// def f8 = { trait A ; trait B extends A ; class C extends B with ToS; new C { } }
// def f9 = { trait A ; trait B ; class C extends B with A with ToS; new C { } }

def f10 = { class A { type T1 } ; List[A#T1]() }
def f11 = { abstract class A extends Seq[Int] ; List[A]() }
def f12 = { abstract class A extends Seq[U forSome { type U <: Int }] ; List[A]() }

trait Bippy { def bippy = "I'm Bippy!" }
object o1 {
  def f1 = { trait A extends Seq[U forSome { type U <: Bippy }] ; abstract class B extends A ; trait C extends B ; (null: C) }
  def f2 = f1.head.bippy
}
o1.f1 _
o1.f2 _

""".trim
}
