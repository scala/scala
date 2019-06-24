object RefinementTest {
  trait A[T]
  trait B[T] extends A[T]

  def takesPartial
      (first: PartialFunction[Any, Unit])
      (second: () => Unit): Unit = ???

  def foo[T](a: A[T]) {
    a match {
      case b: B[t] =>
        takesPartial({ case _ => })(() => implicitly[t =:= T])
    }
  }
}
