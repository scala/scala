import language.higherKinds

object Test {
  sealed trait Coin[+A, +B]
  case class Heads[+A](face: A) extends Coin[A, Nothing]
  case class Tails[+B](back: B) extends Coin[Nothing, B]

  def flip[F[_, _], G[x] <: F[x, String]](f: F[Int, String], g: G[Int]) = ???
  def flop[F[_], G <: F[String]](f: F[String], g: G) = ???

  flip(Tails("scala"): Coin[Int, String], Heads(50))
  flop(Option("scala"), None)
}
