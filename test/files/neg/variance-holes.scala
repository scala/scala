object Test {
  type Lower1[-A] >: List[A]

  class Lower2[F[-_]] {
    type G[+x] >: F[x]
  }

  class Lower3[F[-_, -_]](v: F[Int, Int]) {
    def asWiden[F2[+x, +y] >: F[x, y]]: F2[Int, Int] = v
  }

  trait Refined1[+A] {
    def foo: { type T >: A }
  }

  trait Refined2[+A] {
    def foo(x: { type T <: A }): Unit
  }

  class RefinedLower[+A, x <: { type T >: A }]
  private[this] class PrivateThis[+A](var x: A)
}