object Test {
  trait A[T]
  def f(implicit p: A[T] forSome { type T } ) = null
  implicit val x: A[T] forSome { type T } = null
  println(f)
}
