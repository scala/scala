object example extends App {
  trait MonoIO[F[_]]
  trait BifunctorIO[F[+_, _]]

  case class IO[+A]()
  object IO {
    implicit val monoInstance: MonoIO[IO] = new MonoIO[IO]{}
  }

  trait AnyIO[+F[_]]
  object AnyIO {
    implicit def fromMono[F[_]: MonoIO]: AnyIO[F] = new AnyIO[F]{}
    implicit def fromBIO[F[+_, _]: BifunctorIO]: AnyIO[({ type l[A] = F[Nothing, A]})#l] =
      new AnyIO[({ type l[A] = F[Nothing, A]})#l]{}
  }

  class SomeAlg[+F[_]]
  type SomeAlg2[F[_, _]] = SomeAlg[({ type l[A] = F[Nothing, A]})#l]
  object SomeAlg {
    def make[F[_]: AnyIO](): SomeAlg[F] = new SomeAlg[F]
  }

  val alg: SomeAlg[IO] = SomeAlg.make[IO]()
  val alg1: SomeAlg[IO] = SomeAlg.make()
}

object simplified {
  class MonoIO[F] // it works if you make this covariant

  class IO
  object IO { implicit val monoInstance: MonoIO[IO] = new MonoIO[IO] }

  class AnyIO[+F] // also works if you drop covariance here (and don't add it above)
  object AnyIO { implicit def fromMono[F](implicit mono: MonoIO[F]): AnyIO[F] = new AnyIO[F] }

  object Test {
    // if we provide this explicitly, it works even for any mix of variance:
    // implicit val ev = AnyIO.fromMono[IO]

    implicitly[AnyIO[IO]] //(AnyIO.fromMono) even with this hint, type inference still fails
  }
}
