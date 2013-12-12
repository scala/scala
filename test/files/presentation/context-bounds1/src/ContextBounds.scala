object ContextBound {
  val blubb = new Blubb/*#*/

  def work[A: Foo/*#*/](f: Blubb/*#*/ => A/*#*/): A/*#*/ = f(blubb/*#*/) ensuring {
    implicitly[Foo/*#*/[A/*#*/]].foo/*#*/(_) >= 42
  }
}

trait Foo[A] {
  def foo(a: A/*#*/): Int
}

class Blubb