object Test {

  // Data types

  type Id[A] = A

  class MaybeT[F[_], A]

  type Maybe[A] = MaybeT[Id, A]

  type MaybeMaybe[A] = MaybeT[Maybe, A]


  // Typeclass

  trait Monad[F[_]]


  // Instances

  implicit val monadId: Monad[Id] = ???

  implicit def monadMaybeT[F[_]: Monad]: Monad[({ type λ[A] = MaybeT[F, A] })#λ] = ???

  implicit val monadOption: Monad[Option] = ???


  // Implicit search tests

  implicitly[Monad[Id]]
  implicitly[Monad[({ type λ[A] = A })#λ]]
  implicitly[Monad[Maybe]]
  implicitly[Monad[({ type λ[A] = MaybeT[Id, A] })#λ]]
  implicitly[Monad[MaybeMaybe]]
  implicitly[Monad[({ type λ[A] = MaybeT[Maybe, A] })#λ]]
}
