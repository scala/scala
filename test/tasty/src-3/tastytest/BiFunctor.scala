package tastytest

trait BiFunctor[F[_,_]] {
  def bimap[A,B,C,D](fab: F[A,B])(f: A => C, g: B => D): F[C,D]
}
