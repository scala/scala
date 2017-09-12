object Test {
  final def main(args: Array[String]): Unit = {
    val f: A => Any = { a =>
      case class Case(abc: a.b.C)
      foo(Case, new a.b.C)
    }
    f(new A(new B))
  }

  def foo[A, B](f: A => B, a: A): B = f(a)
}

class A(val b: B)
class B {
  class C
}
