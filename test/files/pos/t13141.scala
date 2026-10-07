trait SF[F[+_, +_], +E, +A]

trait QuasiFunctor[F[_]]

trait catsFunctor[R[_[_]]]

trait Lifecycle[+F[_], +A]
object Lifecycle {
  implicit def catsFunctorForLifecycle[F[_], Functor[_[_]]](implicit e1: QuasiFunctor[F], e2: catsFunctor[Functor]): Functor[({type L[A] = Lifecycle[F, A]})#L] = ???
}

object Scenario {
  def fromLifecycle[F[+_, +_], E, A, E1](conv: E => E1)(l: Lifecycle[({type L[A] = F[E, A]})#L, A]): Lifecycle[({type L[A] = SF[F, E1, A]})#L, A] = ???
  def fromLifecycle[F[+_, +_], E, A](l: Lifecycle[({type L[A] = F[E, A]})#L, A]): Lifecycle[({type L[A] = SF[F, E, A]})#L, A] = ???
}

object app extends App {
  def x[F[+_, +_]]: Lifecycle[({type L[A] = SF[F, Throwable, A]})#L, Unit] = {
    Scenario.fromLifecycle {
      ??? : Lifecycle[({type L[A] = F[Throwable, A]})#L, Unit]
    }
  }
  locally {
    lazy val _ = x
    println("Compiled")
  }
}
