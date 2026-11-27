import scala.collection.{immutable, IterableFactory}
import scala.collection.mutable.Builder

trait A[X] { def get: X }
class B[X] extends A[Int] { def get = 1 }          // does not pass its parameter on
class Bounded[X <: String] extends A[X] { def get = ??? } // tightens the bound
class Wrap[X] extends A[List[X]] { def get = Nil }
class Sub[X] extends A[X] { def get = ??? }

trait A2[P, Q]
class Swap[P, Q] extends A2[Q, P]                  // permutes its parameters
class Sub2[P, Q] extends A2[P, Q]
trait K2[+F[_, _]]
trait K3[+F[X <: String]]

trait K[+F[_]] { def mk[X]: F[X] }

// a collection of rows, with a factory in its companion
class Rows[E](val rows: List[List[E]]) extends immutable.Iterable[List[E]] {
  def iterator = rows.iterator
}
object Rows extends IterableFactory[Rows] {
  def from[E](source: IterableOnce[E]): Rows[E] = new Rows(source.iterator.map(_ :: Nil).toList)
  def empty[E]: Rows[E] = new Rows(Nil)
  def newBuilder[E]: Builder[E, Rows[E]] = List.newBuilder[E].mapResult(from(_))
}

object Test {
  def firstOf(f: IterableFactory[immutable.Iterable]): String = f.from(List("a")).head

  def test(kb: K[B], kw: K[Wrap], ksub: K[Sub]): Unit = {
    val a1: K[A] = kb                // error
    val a3: K[A] = kw                // error
    val a4: K[A] = ksub             // ok
    val a5: K[A] = (??? : K[A])     // ok
    val s: String = a1.mk[String].get
    val b1: K2[A2] = (??? : K2[Swap])   // error
    val b2: K2[A2] = (??? : K2[Sub2])   // ok
    val c1: K3[A] = (??? : K3[Bounded]) // error: the bound of Bounded's parameter is tighter than that of A's
    firstOf(Rows)                    // error
    firstOf(List)                    // ok
    firstOf(immutable.Iterable)      // ok
  }
}
