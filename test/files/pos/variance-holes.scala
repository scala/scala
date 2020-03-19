object Test {
  type Lower1[+A] >: List[A]

  class Lower2[F[+_]] {
    type G[+x] >: F[x]
  }

  class Lower3[F[+_, +_]](v: F[Int, Int]) {
    def asWiden[F2[+x, +y] >: F[x, y]]: F2[Int, Int] = v
  }

  trait Refined1[+A] {
    def foo: { type T <: A }
  }

  trait Refined2[+A] {
    def foo(x: { type T >: A }): Unit
  }

  class Refined3[+A] {
    generic[{ type T >: A } => Int]
  }

  class Refined4[+A] {
    generic[{ type T <: A } => Int]
  }

  class RefinedUpper1[+A, x <: { type T <: A }]
  class RefinedUpper2[+A, x <: { type T[_ <: A] }]
  trait RefinedLower[+A, x <: { type T[_ >: A] }]

  class PrivateThis1[+A] {
    private[this] object Foo { var x: A = _ }
  }

  class PrivateThis2[-A] {
    private[this] val x: Set[A] = Set.empty
    private[this] var y: Set[A] = Set.empty

    class Escape {
      println(x)
      println(y)
    }
  }

  def generic[A]: Unit = ()
}