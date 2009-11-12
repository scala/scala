trait Monad[M[_]] {
  def foo[A](a: M[A]): M[A]
}

class Bar[A, B]
class Bar1[A] { type And[B] = Bar[A, B] }

object Test {
  // the combination of partial applications and anonymous class is essential to reproduce the bug
  // problem: missing bridge method
  //  --> abstractmethoderror `Main$$anon$1.foo(Ljava/lang/Object;)Ljava/lang/Object;`
  // the anonymous class only gets `public Bar foo(Bar a)`
  def BarMonad[X] = new Monad[Bar1[X]#And] {
    def foo[A](a: Bar[X, A]) = a
  }

  def main(as: Array[String]) { BarMonad[Int] foo (new Bar[Int, Int]) }
}