//> using options -Werror

// The classes are compiled in the current run, so whether `A` and `B` are irreconcilable is decided in refchecks,
// also when they are type arguments. The warnings are the same as when compiling `A` and `B` separately.

sealed trait A
final class A1 extends A
trait B
trait C
object E { def unapply(b: B): Option[Int] = None }

object T {
  def f(x: A) = x.isInstanceOf[B]
  def g(x: Some[A]) = x.isInstanceOf[Option[B]]
  def h(x: List[A]) = x match { case _: Seq[B] => 0; case _ => 1 }
  def r(x: A) = x.isInstanceOf[B with C]
  def s(x: A) = x match { case _: B with C => 0; case _ => 1 }
  def u(x: A) = x match { case E(_) => 0; case _ => 1 }
  def v(x: Option[A]) = x match { case Some(E(_)) => 0; case _ => 1 }

  def unchecked(x: List[A]) = x match { case _: Seq[A1] => 0; case _ => 1 }
}
