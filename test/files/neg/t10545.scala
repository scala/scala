class Foo[F[_]]
object Foo {
  // Prior to this fix these two are ambiguous
  implicit def fooF0[F[_]]: Foo[F] = new Foo[F]
  implicit def fooF1: Foo[Option] = new Foo[Option]
}

class Bar[F[_]]
object Bar extends Bar0 {
  // Prior to this fix these two aren't selected because there is no
  // Foo[F] due to the ambiguity above
  // After this fix these two are ambiguous
  implicit def barF0[F[_]](implicit fooF: Foo[F]): Bar[F] = new Bar[F]
  implicit def barF1[F[_]](implicit fooF: Foo[F]): Bar[F] = new Bar[F]
}

trait Bar0 {
  // Prior to this fix we fall back to here
  implicit def barF2[F[_]]: Bar[F] = new Bar[F]
}

object Test {
  // Prior to this fix Bar.barF1[Option]
  // After this fix,
  // error: ambiguous implicit values:
  //   both method barF0 in object Bar of type [F[_]](implicit fooF: Foo[F])Bar[F]
  //   and method barF1 in object Bar of type [F[_]](implicit fooF: Foo[F])Bar[F]
  //   match expected type Bar[Option]
  //    implicitly[Bar[Option]]
  //              ^
  // one error found
  implicitly[Bar[Option]]
}
