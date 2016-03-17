trait NoAbstract

trait TwoAbstract { def ap(a: Int): Int; def pa(a: Int): Int }

trait Base // check that the super class constructor isn't considered.

trait MultipleMethodLists { def ap(a: Int)(): Int }

trait ImplicitMethodParam { def ap(a: Int)(implicit b: String): Int }

trait PolyClass[T] { def ap(a: T): T }

trait PolyMethod { def ap[T](a: T): T }

trait OneAbstract { def ap(a: Int): Any }
trait DerivedOneAbstract extends OneAbstract

// restrictions

// must be an interface
abstract class NotAnInterface[T, R]{ def apply(x: T): R }

trait A[T, R]{ def apply(x: T): R }

// must not capture
class Nested {
  trait F[T, U] { def apply(x: T): U }

  def app[T, U](x: T)(f: F[T, U]): U = f(x)
}


object Test {
  implicit val s: String = ""
  type NonClassType = DerivedOneAbstract with OneAbstract

  (() => 0)      : NoAbstract            // error expected
  ((x: Int) => 0): TwoAbstract           // error expected
  ((x: Int) => 0): DerivedOneAbstract
  ((x: Int) => 0): NonClassType
  ((x: Int) => 0): MultipleMethodLists   // error expected
  ((x: Int) => 0): ImplicitMethodParam   // error expected

  ((x: Int) => 0): PolyClass[Int]
  ((x: Int) => 0): PolyMethod            // error expected

  (x => x + 1): NotAnInterface[Int, Int] // error expected (not an interface)
  ((x: String) => 1): A[Object, Int]     // error expected (type mismatch)

  val n = new Nested
  n.app(1)(x => List(x)) // error expected: n.F is not a SAM type (it does not have a no-arg ctor since it has an outer pointer)
}
