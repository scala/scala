//> using options -Werror

object Test {
  sealed trait Gadt[F[_], O, R]
  final case class Output[F[_], O](values: List[O]) extends Gadt[F, O, Unit]

  final case class Algebra[F[_], A]()
  final case class Free[F[_], A](fa: F[A])

  def values[F[_], O, R](gadt: Gadt[F, O, R]): List[O] =
    gadt match { case o: Output[F, O] => o.values }

  def free[F[_], A](f: Free[({ type G[x] = Algebra[F, x] })#G, A]): Algebra[F, A] = f.fa
  def pure[F[_], A]: Free[({ type G[x] = Algebra[F, x] })#G, A] = Free(Algebra())

  val vs = values(Output[Option, String](List("GADT")))
  val a: Algebra[Option, Int] = free(pure)
}
