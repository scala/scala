package test

// Cats Xor, Scalaz \/, scala.util.Either
sealed abstract class Xor[+A, +B] extends Product with Serializable
object Xor {
  final case class Left[+A](a: A) extends (A Xor Nothing)
  final case class Right[+B](b: B) extends (Nothing Xor B)
}

object TestXor {
  import Xor._
  def meh[F[_], A, B](fa: F[A])(f: A => B): F[B] = ???
  meh(new Right(23): Xor[Boolean, Int])(_ < 13)
  meh(new Left(true): Xor[Boolean, Int])(_ < 13)
}
