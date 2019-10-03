package tastytest

trait BiFunctorT2[F[_,_] <: Tuple2[_,_]] {
  def bimap[A,B,C,D](fab: F[A,B])(f: A => C, g: B => D): F[C,D]
}
