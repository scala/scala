
object A {
  def a[F](x: Int) = 0
  def a[F](x: String) = 0
}

class C {
  def f  = A.a[Int][String](0)
}
