object TypeUtil0 {
  trait Type[+T];
  implicit def WithType[S,T](implicit tpeS : Type[S], tpeT : Type[T]) : Type[S with T] = null
  as[Any](null);
  def as[T](x : Any)(implicit tpe : Type[T]) = null;
}
