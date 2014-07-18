sealed trait \/[+A, +B]

sealed trait EitherT[F[+_], +A, +B]
object EitherT {
  def apply[F[+_], A, B](a: F[A \/ B]): EitherT[F, A, B] = new EitherT[F, A, B] { val run = a }
}
