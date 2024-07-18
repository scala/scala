//> using options -Xfatal-warnings -Xlint:strict-unsealed-patmat
//

object Test {
  sealed trait Foo
  final class Bar extends Foo

  object Bar {
    def unapply(o: Bar): true = true
  }

  def f(foo: Foo) = foo match {
    case Bar() => println("Bar")
  }
}