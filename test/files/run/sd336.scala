//AbstractMethodError with missing bridge for path-dependent type
object Test {
  final def main(args: Array[String]): Unit = {
    val f: A => Any = { a =>
      case class Case(abc: a.b.C)
      foo(Case, new a.b.C)
    }
    f(new A(new B))
  }

  def foo[X, Y](f: X => Y, x: X): Y = f(x)
}

class A(val b: B)
class B {
  class C
}
