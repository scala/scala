// need to test both orders
object A1 {
  implicit def i: Equiv[Boolean] = error("")
  implicit def div[T, A](implicit f: T => A, eq: Equiv[A]): Equiv[T] = error("")

  implicitly[Equiv[Boolean]]
}

object A2 {
  implicit def div[T, A](implicit f: T => A, eq: Equiv[A]): Equiv[T] = error("")
  implicit def i: Equiv[Boolean] = error("")

  implicitly[Equiv[Boolean]]
}

