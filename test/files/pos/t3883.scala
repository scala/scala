// need to test both orders
object A1 {
  implicit def i: Equiv[Boolean] = sys.error("")
  implicit def div[T, A](implicit f: T => A, eq: Equiv[A]): Equiv[T] = sys.error("")

  implicitly[Equiv[Boolean]]
}

object A2 {
  implicit def div[T, A](implicit f: T => A, eq: Equiv[A]): Equiv[T] = sys.error("")
  implicit def i: Equiv[Boolean] = sys.error("")

  implicitly[Equiv[Boolean]]
}

