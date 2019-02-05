trait Bar0[+A]
trait Bar1[+This]
class CBar extends Bar0[Int] with Bar1[CBar] { }

trait GSVL[+A, +This <: Bar0[A] with Bar1[This]] {
  // There has to be a method in Foo
  trait Foo { def f = ??? }

  // There has to be a private method with a closure in Reversed,
  // and it has to be a trait.
  trait Reversed extends Foo {
    private def g = { List(1) map (_ + 1) ; ??? }
  }
}
