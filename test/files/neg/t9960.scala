import scala.language.higherKinds

object NNN {
  class Validate[A]
  class Task[A]
  class Fx2[M[_], N[_]]
  class Fx1[M[_]]
  class Eff[R, A]
  class Reader[A]
  final case class FxAppend[L, R](left: L, right: R)
  trait Member[T[_], R]{
    type Out
  }

  type Aux[T[_], R, U] = Member[T, R] { type Out = U }

  implicit def Member3L[T[_], L[_],  R[_]]: Aux[T, FxAppend[Fx1[T], Fx2[L, R]], Fx2[L, R]] =
    new Member[T, FxAppend[Fx1[T], Fx2[L,  R]]] { outer =>
      type Out = Fx2[L,  R]
    }

  object TheTest {
    def runReader[R, U, B](r: Eff[R, B])(implicit m: Aux[Reader, R, U]): Eff[U, B] = ???

    def helper(): Unit = {
      val gggg: Eff[FxAppend[Fx1[Task], Fx2[Validate, Reader]], Unit] = ???
      val hhhh: Eff[Fx2[Task, Validate], Unit] = runReader(gggg)
    }
  }
}
