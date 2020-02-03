package tastytest

object HKNest {

  class Box[A]

  class Foo[F[_]] {
    override def toString(): String = "Foo"
  }

}
