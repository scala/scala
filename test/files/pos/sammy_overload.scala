trait Consumer[T] {
  def consume(x: T): Unit
}

object Test {
  def foo(x: String): Unit = ???
  def foo(): Unit = ???
  val f: Consumer[_ >: String] = foo
}

trait A[A, B] { def apply(a: A): B }

class ArityDisambiguate {
  object O {
    def m(a: A[Int, Int]) = 0
    def m(f: (Int, Int) => Int) = 1
  }

  O.m(x => x) // ok
  O.m((x, y) => x) // ok
}

class InteractionWithImplicits {
  object O {
    class Ev
    implicit object E1 extends Ev
    implicit object E2 extends Ev
    def m(a: A[Int, Int])(implicit ol: E1.type) = 0
    def m(a: A[String, Int])(implicit ol: E2.type) = 1
  }

  O.m((x: Int) => 1) // ok
  O.m((x: String) => 1) // ok
}
