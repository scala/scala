package blam {

  package foo {

    trait F[T] {
      def f(d: Double, t: T): T = ???
      def f(d: Int, t: T): T = ???
      def f(d: String, t: T): T = ???

      def g[A](a: T): T = ???
      def g(a: Int) = ???
    }
  }

  package object foo extends foo.F[Double] {
    override def f(d: Double, t: Double): Double = ???
  }
}

object Test {
  import blam._
  foo.f("3", 4.0)
  foo.g[Any](1d) : Double
}
