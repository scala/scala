object Test {
  abstract class Recursive[T <: AnyKind, Base <: AnyKind, ~~>[_ <: AnyKind, _ <: AnyKind]] {
    def project: T ~~> Base
  }

  object Recursive {
    type Aux[T <: AnyKind, Base <: AnyKind, ~~>[_ <: AnyKind, _ <: AnyKind]] = Recursive[T, Base, ~~>]
  }

  trait ~>[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  // single-sorted recursion
  final case class Fix[F[_]](unFix: F[Fix[F]])

  object Fix {
    implicit def recursive[F[_]]: Recursive.Aux[Fix[F], F[Fix[F]], Function1] =
      new Recursive[Fix[F], F[Fix[F]], Function1] {
        def project: Fix[F] => F[Fix[F]] = _.unFix
      }
  }

  // multi-sorted recursion (comments have kind-projected version)
  final case class FixH[F[_[_], _], A](hunFix: F[({ type λ[α] = FixH[F, α] })#λ, A])

  object FixH {
    //       def recursive[F[_[_], _]]: Recursive.Aux[FixH[F, ?], F, ~>] =
    implicit def recursive[F[_[_], _]]: Recursive.Aux[
      ({ type λ[α] = FixH[F, α] })#λ
    , ({type λ[α] = F[({ type λ[α] = FixH[F, α] })#λ, α] })#λ
    , ~>] = {

      type In[A] = FixH[F, A]
      type Out[A] = F[In, A]

      new Recursive[In, Out, ~>] {
        def project: In ~> Out = new (In ~> Out) {
          def apply[A](fa: In[A]): Out[A] = fa.hunFix
        }
      }
    }
  }

  trait Foo[F[_], A]
  type FixHFoo[A] = FixH[Foo, A]
  FixH.recursive[Foo].project(FixH[Foo, Int](new Foo[FixHFoo, Int] {}))

}