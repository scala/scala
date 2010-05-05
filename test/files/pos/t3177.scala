trait InvariantFunctor[F[_]] {
  def xmap[A, B](ma: F[A], f: A => B, g: B => A): F[B]
}

object InvariantFunctor {
  import Endo._

  implicit val EndoInvariantFunctor = new InvariantFunctor[Endo] {
    def xmap[A, B](ma: Endo[A], f: A => B, g: B => A): Endo[B] = (b: B) => f(ma(g(b)))
  }

  // The definition about fails with:
  // anon-type.scala:9: error: not found: value b
  //       def xmap[A, B](ma: Endo[A], f: A => B, g: B => A): Endo[B] = (b: B) => f(ma(g(b)))
  //                                                                                     ^
  //   anon-type.scala:8: error: not found: type $anon
  //     implicit val EndoInvariantFunctor = new InvariantFunctor[Endo] {
  //                                         ^


  // These both work:
  // implicit val EndoInvariantFunctorAscribed: InvariantFunctor[Endo] = new InvariantFunctor[Endo] {
  //   def xmap[A, B](ma: Endo[A], f: A => B, g: B => A): Endo[B] = (b: B) => f(ma(g(b)))
  // }
  //
  // implicit val EndoInvariantFunctorStubbed = new InvariantFunctor[Endo] {
  //   def xmap[A, B](ma: Endo[A], f: A => B, g: B => A): Endo[B] = error("stub")
  // }
}

trait Endo[X]

object Endo {
  implicit def EndoTo[A](f: A => A): Endo[A] = new Endo[A] {
    def apply(a: A) = f(a)
  }

  implicit def EndoFrom[A](e: Endo[A]): A => A = e.apply(_)
}