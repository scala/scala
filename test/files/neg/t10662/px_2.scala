
package p {

  trait T[A]

  class X {
    override def toString() = "p.X"
  }
  object X {
    implicit val tx: T[X] = new T[X] { }
  }

  package q {
    //import p.X  // "permanently hidden"
    object Test {
      // previously, picked p.q.X
      // This file compiles by itself;
      // from our perspective, the other X renders our X ambiguous
      implicitly[T[X]]   // ambiguous
    }
  }
}
