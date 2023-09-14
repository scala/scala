
class Apply {
  def apply[A](x: Int) = 1
}

object A {
  def a[F] = new Apply
  def a[F](x: String) = 0
}

class C {
  def f = A.a[String]("a") // 0
  def g = A.a[String](2)   // 1
  def k = A.a[String][Int](3)
}
