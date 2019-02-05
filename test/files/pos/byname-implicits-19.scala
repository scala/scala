object Test {
  class A
  class B
  class C
  class D

  {
    implicit def parentA(implicit arg: => B): A = ???
    implicit def parentB(implicit arg: C): B = ???
    implicit def parentC(implicit arg: D): C = ???
    implicit def parentD(implicit arg: A): D = ???

    implicitly[A]
  }

  {
    implicit def parentA(implicit arg: B): A = ???
    implicit def parentB(implicit arg: => C): B = ???
    implicit def parentC(implicit arg: D): C = ???
    implicit def parentD(implicit arg: A): D = ???

    implicitly[A]
  }

  {
    implicit def parentA(implicit arg: B): A = ???
    implicit def parentB(implicit arg: C): B = ???
    implicit def parentC(implicit arg: => D): C = ???
    implicit def parentD(implicit arg: A): D = ???

    implicitly[A]
  }

  {
    implicit def parentA(implicit arg: B): A = ???
    implicit def parentB(implicit arg: C): B = ???
    implicit def parentC(implicit arg: D): C = ???
    implicit def parentD(implicit arg: => A): D = ???

    implicitly[A]
  }
}
