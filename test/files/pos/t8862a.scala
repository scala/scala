package p {

  abstract class C[A] {
    def x: A
    implicit def oops: A = x
    implicit def oopso: Option[A] = None
  }

  package q {

    class Oops

    object `package` extends C[Oops] {
      override def x = new Oops
    }

    object Blah {
      oops
      oopso

      // implicits found in enclosing context
      implicitly[Oops]
      implicitly[Option[Oops]]
    }
  }
}

package other {

  object Blah {
    // implicits found through this import
    import p.q._

    oops
    oopso

    implicitly[Oops]
    implicitly[Option[Oops]]
  }


  object Blee {
    // implicits found through the companion implicits
    implicitly[p.q.Oops]
    implicitly[Option[p.q.Oops]]
  }
}
