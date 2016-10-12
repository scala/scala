import scala.language.higherKinds

object Test extends App {


  trait X[F[_] <: AnyKind] { type L = F[Int]; def a: L = ??? }
  new X[List] { }

  trait X2[A <: AnyKind, B <: AnyKind] { def run[F[_ <: AnyKind]]: F[A] => F[B] }

  val x21 = {
    new X2[Int, Int] { def run[F[_]]: F[Int] => F[Int] = identity[F[Int]] }.run[List]
  }


  trait X3[A <: AnyKind, B <: AnyKind, C <: AnyKind] { def run[F[_ <: AnyKind, _ <: AnyKind]]: F[A, C] => F[B, C] }

  val x31 = {
    new X3[Int, Int, String] { def run[F[_, _]]: F[Int, String] => F[Int, String] = identity[F[Int, String]] }.run[Map]
  }


  trait X4[A <: AnyKind, B <: AnyKind, C] { def run[F[_ <: AnyKind, _]]: F[A, C] => F[B, C] }

  trait Foo4[A]
  trait Bar4[A]

  trait Toto4[F[_], A]

  val x41 = {
    new X4[Foo4, Foo4, Int] { def run[F[_[_], A]]: F[Foo4, Int] => F[Foo4, Int] = identity[F[Foo4, Int]] }
      .asInstanceOf[X4[Bar4, Bar4, Int]].run[Toto4]
  }

  trait X5[A[_ <: AnyKind], B[_ <: AnyKind]] { def run[F[_[_ <: AnyKind]]]: F[A] => F[B] }

  trait Foo5[A[_]]
  trait Bar5[A[_]]

  trait Toto[F[_[_]]]

  val x51 = {
    new X5[Foo5, Foo5] { def run[F[_[_ <: AnyKind]]]: F[Foo5] => F[Foo5] = identity[F[Foo5]] }
      .asInstanceOf[X5[Bar5, Bar5]].run[Toto]
  }

  x51(new Toto[Bar5] {})


}






/*
class Functor (p :: k1 -> k2) (r :: k1 -> k1 -> *) (s :: k2 -> k2 -> *) where
  fmap :: a `r` b -> (p a) `s` (p b)
type Nat (cat :: k1 -> k1 -> *) (f :: k2 -> k1) (g :: k2 -> k1) = forall (a :: k2). (f a) `cat` (g a)
newtype HFreeK (cat :: k -> k -> *) (c :: (k -> *) -> Constraint) (f :: k -> *) a =
  HFree { runHFree :: forall (g :: k -> *). (c g, Functor g cat (->)) => Nat (->) f g -> g a }
*/

// sealed trait Functor[K1<:AnyKind, K2[_<:K1]<:AnyKind, ->[_<:K1, _<:K1], ~>[_<:K2, _<:K2]] {
//   def fmap[A <: K1, B <: K1]: A -> B => K2[A] ~> K2[B]
// }

/*
  data HFree t m a where
    Lift :: m a -> HFree t m a
    Gen :: t m a -> HFree t m a
    Hoist :: (forall x . n x -> m x) -> HFree t n a -> HFree t m a
    Embed :: (forall x . n x -> HFree t m x) -> HFree t n a -> HFree t m a
*/

// trait UnapplyApply[FA, F <: AnyKind, G <: AnyKind, GA, M <: Morphism[F, G]] {
//   def apply(m: M)(g: FA): GA
// }

// /** a kind-polymorphic morphism */
// sealed trait Morphism[F <: AnyKind, G <: AnyKind] {
//   def apply[FApp, GApp](v: FApp)(implicit unapplyApply: UnapplyApply[FApp, F, G, GApp, this.type]): GApp = unapplyApply(this)(v)
// }

// sealed trait HFree[M <: AnyKind, ->[_<:AnyKind, _<:AnyKind]] {
//   type MA
//   type A
// }

// case class Lift[M <: AnyKind, ->[_<:AnyKind, _<:AnyKind], MA0, A0](value: A0) extends HFree[M, ->] {
//   type MA = MA0
//   type A = A0
// }

// case class Gen[M <: AnyKind, ->[_<:AnyKind, _<:AnyKind], MA0, A0](value: MA0) extends HFree[M, ->] {
//   type MA = MA0
//   type A = A0
// }

// case class Hoist[N <: AnyKind, ->[_<:AnyKind, _<:AnyKind], NA, A, M <: AnyKind, MA](
//   nat: N -> M
// , free: HFree[N, ->]
// ) extends HFree[M, ->] {
//   type MA = 
// }

// case class Embed[M <: AnyKind](
//   nat: N -> HFree[M, ->]
// , free: HFree[N, ->]
// ) extends HFree[M, ->]

// trait Lifter[->[_<:AnyKind, _<:AnyKind], M <: AnyKind, T <: AnyKind] {
//   type TM
//   def lift: M -> TM
// }

// new Lifter[~>, M[_], T[_[_]]] {
//   type TM = TM[M]
// }

// def foldMap[M <: AnyKind, N <: AnyKind, -> <: Morphism[M, N]](nat: M -> N): HFree[M, ->] -> MA = free match {
//   case Lift(a) => Lift()
// }

// trait ~>[F[_], G[_]] {
//   def apply[A](fa: F[A]): G[A]
// }

// type Free[F[_], A] = HFree[F, ~>, F[A], A]

