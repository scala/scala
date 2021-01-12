import scala.language.implicitConversions

// scala/bug#3281
object Test1 {
  class A[T]
  class B[T]
  class C

  implicit def f(a: A[_]): B[AnyRef] = null
  implicit def g[T <: AnyRef](src: A[T]): B[T] = null

  def c[T <: AnyRef](x: B[T]) = null
  def c(x: C) = null

  c(null: A[_])
}

// scala/bug#12044
object Test2 {
  class Bar
  def f[F[_], A](v: F[A]) = v
  implicit def barToList(b: Bar): List[Int] = List(42)
  val x = f(new Bar)
  x: List[Int]
}
