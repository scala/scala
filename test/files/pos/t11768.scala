object A {
  trait B[T]
  implicit def c[T <: Singleton]: B[T] = ???
  implicit def d[T1: B, T2: B]: B[Tuple2[T1, T2]] = ???
  implicit def e[T: B]: B[Option[T]] = ???
  implicit def f[C[_] <: Iterable[_], T](implicit r: B[T]): B[C[T]] = ???
}

object G {
  class H[T: A.B, V: A.B](t: Option[(V, T)]){
    implicitly[A.B[Option[(V, T)]]]
  }
  def h[T: A.B, V: A.B](t: Option[(V, T)]){
    implicitly[A.B[Option[(V, T)]]]
  }
}
