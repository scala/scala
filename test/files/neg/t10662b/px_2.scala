
package p {

  trait T[A]

  class X {
    override def toString() = "p.X"
  }
  object X {
    implicit val tx: T[X] = new T[X] { }
  }

  package q {
    import r.X
    object Test {
      implicitly[T[X]]   // ambiguous
    }
  }

  package r {
    class X
  }
}
