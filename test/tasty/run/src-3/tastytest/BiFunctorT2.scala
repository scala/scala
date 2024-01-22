package tastytest

trait BiFunctorT2[F[X,Y] <: Tuple2[X,Y]] {
  def bimap[A,B,C,D](fab: F[A,B])(f: A => C, g: B => D): F[C,D]
}
