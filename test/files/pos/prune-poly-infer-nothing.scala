object Test {
  trait Pure[+A]
  trait Stream[+F[_], +O]
  object Stream {
    implicit def covaryPure[F[_], O, O2 >: O](s: Stream[Pure, O]): Stream[F, O2] = ???
    def empty: Stream[Pure, Nothing] = ???
  }

  type EntityBody[+F[_]] = Stream[F, Byte]

  val EmptyBody: EntityBody[Nothing] = Stream.empty
}
