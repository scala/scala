//> using options -Werror

// The classes are compiled in the current run, so whether `A` and `B` are irreconcilable is decided at the end of
// typer, also when they are type arguments. The warnings are the same as when compiling `A` and `B` separately.

sealed trait A
final class A1 extends A
trait B

object T {
  def f(x: A) = x.isInstanceOf[B]
  def g(x: Some[A]) = x.isInstanceOf[Option[B]]
  def h(x: List[A]) = x match { case _: Seq[B] => 0; case _ => 1 }

  def unchecked(x: List[A]) = x match { case _: Seq[A1] => 0; case _ => 1 }
}
