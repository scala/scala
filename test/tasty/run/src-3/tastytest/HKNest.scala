package tastytest

object HKNest {
    case class Box[A](x: A)

    class Foo[F[_]] {
      override def toString(): String = "Foo"
    }
}
