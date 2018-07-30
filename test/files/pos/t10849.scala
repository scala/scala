import language.higherKinds

object Test {
  class TcHk[F[_]]
  trait Foo { type T }
  implicit val tcHkOpt: TcHk[Option] = new TcHk
  implicit def foo[F[_], A](implicit F: TcHk[F]): Foo { type T = F[A] } =
    new Foo { type T = F[A] }
  implicitly[Foo { type T = Option[Int] }]
}
