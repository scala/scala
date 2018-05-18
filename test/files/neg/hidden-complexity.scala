trait Foo[F[_]]
object Foo {
  implicit def mkFoo[F[_]](implicit ff: Foo[({ type λ[t] = F[F[t]] })#λ]): Foo[F] = ???
}

trait Bar[F[_]]
object Bar {
  implicit def mkBar[F[_]](implicit bb: Bar[λ forSome { type λ[t] <: F[t] }]): Bar[F] = ???
}

object Test {
  implicitly[Foo[List]]
  implicitly[Bar[List]]
}
