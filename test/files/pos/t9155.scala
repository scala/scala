class A[T]
object A {
  type T = A[_]
  manifest[T]
}

class B[T]
object B {
  type Any = B[ _ <: String]
  manifest[B[_ <: String]]
  manifest[B.Any]
}

class C[T]
object C {
  def f[T](implicit m: Manifest[T]) = 0
  type CAlias = C[_]
  val x = f[CAlias]
}
