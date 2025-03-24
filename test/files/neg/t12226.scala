//> using options -Xlint:implicit-recursion -Werror

import language.implicitConversions

object X {
  implicit class Elvis[A](alt: => A) { def ?:(a: A): A = if (a ne null) a else alt } // warn
}
object Y {
  implicit def f[A](a: A): String = if (a ne null) a else "nope" // warn
}
object YY {
  def idt[A](n: Int = 1, x: A): A = x
  implicit def f[A](a: A): String = if (idt(x = a) ne null) "yup" else "nope" // warn
}
object Z {
  implicit class StringOps(val s: String) extends AnyVal {
    def crazy: String = s.reverse
    def normal: String = s.crazy.crazy // nowarn value class
    def join(other: String): String = crazy + other.crazy // nowarn
  }
}
object ZZ {
  implicit class StringOps(s: String) {
    def crazy: String = s.reverse
    def normal: String = s.crazy.crazy // warn
    def join(other: String): String = crazy + other.crazy // nowarn
  }
}

object ZZZ {
  class C { def f: C = this }
  implicit class E(c: C) {
    def bar: Int = c.f.bar // nowarn
  }
}

object sd893 {
  case class C(a: Int, b: Int) {
    implicit class Enrich(c2: C) {
      def foo: C = c2.copy(b = 0).foo // nowarn
    }
  }
}
