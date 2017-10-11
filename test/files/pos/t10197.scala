import scala.language.higherKinds

final case class Getter[S, A](get: S => A)

final case class Wrap[F[_], A](value: F[A])

object Wrap {
  // Helper to defer specifying second argument to Wrap.
  // Basically a type lambda specialized for Wrap.
  // Wr[F]#ap[A] =:= Wrap[F, A]
  type Wr[F[_]] = { type ap[A] = Wrap[F, A] }

  implicit def unwrapper[F[_], A]: Getter[Wrap[F, A], F[A]] =
    Getter(w => w.value)
}

object Test {
  import Wrap._

  type Foo[A] = List[A]
  type Bar[A] = String

  type WrapFoo1[A] = Wrap[Foo, A]
  type WrapBar1[A] = Wrap[Bar, A]

  implicitly[Getter[WrapFoo1[Int], Foo[Int]]]
  implicitly[Getter[WrapBar1[Int], Bar[Int]]]

  type WrapFoo2[A] = Wr[Foo]#ap[A]
  type WrapBar2[A] = Wr[Bar]#ap[A]

  // here's evidence that the new types are the same as the old ones
  implicitly[WrapFoo2[Int] =:= WrapFoo1[Int]]
  implicitly[WrapBar2[Int] =:= WrapBar1[Int]]

  implicitly[Getter[WrapFoo2[Int], Foo[Int]]]
  implicitly[Getter[WrapBar2[Int], Bar[Int]]]
}
