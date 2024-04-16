//> using options -Werror

package t12408 {
  class Renderer[A]
  class Test[A](a: Any) {
    def f1[B] = a match { case _: B => }                        // warn
    def f2[B] = a match { case _: Renderer[B] => }              // warn
    def f3[B](xs: List[A]) = xs match { case _: List[Int] => }  // warn
    def g = a match { case _: Renderer[A] => }                  // now also warn
  }

  trait T[A,B,C,D,E,F,G,H,I,J,K,L,M] {
    def f(a: Any) = a match {
      case _: (A,B,C,D,E,F,G,H,I,J,K,L,M) =>
    }
    def g[A,B,C,D,E,F,G,H,I,J,K,L,M] = (null: Any) match {
      case _: (A,B,C,D,E,F,G,H,I,J,K,L,M) =>
    }
  }
  class C[A,B,C,D,E,F,G,H,I,J,K,L,M] {
    def f(a: Any) = a match {
      case _: (A,B,C,D,E,F,G,H,I,J,K,L,M) =>
    }
  }
}

package t12408b {
  // trait's type params align with class C
  sealed trait T[A, B]
  final case class C[A, B](a: A, b: B) extends T[A, B]

  class Test[A, B] {
    def test(t: T[A, B]) = t match { case _: C[A, B] => }       // nowarn
  }
  object Test extends App {
    println {
      new Test[String, Int]().test(C("hi", 42))
    }
  }
}

package t12408c {
  sealed trait T[A]
  final case class C[A, B](a: A, b: B) extends T[A]

  class Test[A, B] {
    def test(t: T[A]) = t match { case _: C[A, B] => }          // warn on B
  }
  object Test extends App {
    println {
      new Test[String, Int]().test(C("hi", 42))
    }
  }
}

package reported {
  sealed trait Action[Page]
  final case class Renderer[Page, Props]() extends Action[Page]
  sealed trait Redirect[Page] extends Action[Page]

  final class RouterLogic[Page, Props] {

    def hmm1(a: Action[Page]): Int =
      a match {
        case r: Renderer[Page, Props] => 1    // warn as above
        case _                        => 2
      }

    def hmm2(a: Action[Page]): Int =
      a match {
        case r: Redirect[Page] => 2           // nowarn
        case _                 => 1
      }
  }
}

package regression {
  object unchecked3 {
  /* nowarn */ def tparamLeakage1(x: Any) = x match { case Array() => 1 }
  /* nowarn */ def tparamLeakage2(x: Any) = x match { case List() => 1 }
  }
}
