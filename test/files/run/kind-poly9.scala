import scala.language.higherKinds

object Test extends App {

  // PKList or the Any-Order heterogenous list
  sealed trait PKList[Args <: AnyKind]

  trait PKNil[Args <: AnyKind] extends PKList[Args]

  sealed trait PKCons[Args <: AnyKind, HA, TA <: PKList[Args]] extends PKList[Args] {
    def head: HA
    def tail: TA
  }

  object PKList {

    trait PKConsBuilder[Args <: AnyKind, HA, UL <: PKList[Args]] {
      type OutA <: PKList[Args]
      def ::(l: UL)(ha: HA): OutA
    }

    implicit class PKListOps[
      Args <: AnyKind
    ](l: PKNil[Args]) {
      def ::[HA0](ha: HA0)(implicit unap: PKConsBuilder[Args, HA0, PKNil[Args]]): unap.OutA = unap.::(l)(ha)
    }


    implicit class PKListOps2[
      Args <: AnyKind
    , HA
    , T <: PKList[Args]
    ](l: PKCons[Args, HA, T]) {
      def ::[HA0](ha: HA0)(implicit unap: PKConsBuilder[Args, HA0, PKCons[Args, HA, T]]): unap.OutA = unap.::(l)(ha)
    }

    trait Contains[Args <: AnyKind, H, L <: PKList[Args]] {
      def apply(l: L): H
    }

    object Contains {
      def apply[Args <: AnyKind, H, L2 <: PKList[Args]](implicit is: Contains[Args, H, L2]) = is

      implicit def head[Args <: AnyKind, H, L2 <: PKList[Args]] = new Contains[Args, H, PKCons[Args, H, L2]] {
        def apply(l: PKCons[Args, H, L2]): H = l.head
      }

      implicit def corec[Args <: AnyKind, H, H2, L2 <: PKList[Args]] (
        implicit next: Contains[Args, H, L2]
      ) = new Contains[Args, H, PKCons[Args, H2, L2]]  {
        def apply(l: PKCons[Args, H2, L2]): H = next(l.tail)
      }
    }

    trait IsSubPKList[Args <: AnyKind, L1 <: PKList[Args], L2 <: PKList[Args]] {
      def sub(l2: L2): L1
    }

    object IsSubPKList {
      def apply[Args <: AnyKind, L1 <: PKList[Args], L2 <: PKList[Args]](implicit is: IsSubPKList[Args, L1, L2]) = is

      implicit def nil[Args <: AnyKind, L2 <: PKList[Args]] = new IsSubPKList[Args, PKNil[Args], L2] {
        def sub(l2: L2): PKNil[Args] = new PKNil[Args] {}
      }

      implicit def head[Args <: AnyKind, H, L1 <: PKList[Args], L2 <: PKList[Args]](
        implicit c: Contains[Args, H, L2], next: IsSubPKList[Args, L1, L2]
      ) = new IsSubPKList[Args, PKCons[Args, H, L1], L2]  {
        def sub(l2: L2): PKCons[Args, H, L1] = new PKCons[Args, H, L1] {
          val head = c(l2)
          val tail = next.sub(l2)
        }
      }
    }

  }

  object Lawful {
    import PKList._

    case class HNilF[F <: AnyKind]() extends PKNil[F]

    implicit def buildMCons[M[_ <: AnyKind], F <: AnyKind, T <: PKList[F]] =
      new PKConsBuilder[F, M[F], T] {
        type OutA = PKCons[F, M[F], T]

        def ::(l: T)(h: M[F]): OutA =
          new PKCons[F, M[F], T] {
            val head = h
            val tail = l
          }
      }

    trait Laws[Scope, F <: AnyKind, L <: PKList[F]] {
      def laws: L
    }

    object Laws {
      // type Aux[F <: AnyKind, L0 <: PKList[F]] = Laws[F] { type L = L0 }

      class Builder[Scope, F] {
        def apply[L0 <: PKList[F]](l0: L0): Laws[Scope, F, L0] = new Laws[Scope, F, L0] {
          val laws = l0
        }
      }

      def apply[Scope, F <: AnyKind] = new Builder[Scope, F]

    }

    trait HasLaws[Scope, Args <: AnyKind, LS <: PKList[Args]] {
      def laws: LS
      def apply[HA](implicit c: Contains[Args, HA, LS]): HA = c(laws)
    }

    object HasLaws {
      def apply[Scope, Args <: AnyKind, LS <: PKList[Args]](implicit hl: HasLaws[Scope, Args, LS]): HasLaws[Scope, Args, LS] = hl

      implicit def hasLaws[Scope, Args <: AnyKind, LS <: PKList[Args], LS2 <: PKList[Args]](
        implicit la: Laws[Scope, Args, LS], issub: IsSubPKList[Args, LS2, LS]
      ): HasLaws[Scope, Args, LS2] = new HasLaws[Scope, Args, LS2] {
        val laws = issub.sub(la.laws)
      }
    }

    trait HasLaw[Scope, Args <: AnyKind, L] {
      def law: L
    }

    object HasLaw {
      def apply[Scope, Args <: AnyKind, L](implicit hasLaws: HasLaws[Scope, Args, PKCons[Args, L, PKNil[Args]]]) =
        new HasLaw[Scope, Args, L] {
          val law = hasLaws.laws.head
        }

      implicit def hasLaw[Scope, Args <: AnyKind, L](implicit hasLaws: HasLaws[Scope, Args, PKCons[Args, L, PKNil[Args]]]) =
        new HasLaw[Scope, Args, L] {
          val law = hasLaws.laws.head
        }

    }

    case class Scope[S](s: S)
    case class WithScope[S](s: S) {
      val scope = Scope(s)
      def apply[A](f: Scope[S] => A): A = f(scope)
    }
  }
  
  import PKList._
  import Lawful._

  // simple-order monoid typeclass
  trait Monoid[F] {
    def append(a: F, b: F): F
  }

  // append depending on a scope (could be made more idiomatic with some macro/annoation/scalameta)
  def append[F, S](a: F, b: F)(implicit scope: Scope[S], hasMonoid: HasLaw[S, F, Monoid[F]]): F = hasMonoid.law.append(a, b)

  // Scope for Numeric Sum monoid
  object SumScope { self =>
    def monoid[T](implicit numeric: Numeric[T]): Monoid[T] = new Monoid[T] {
      def append(a: T, b: T): T = numeric.plus(a, b)
    }

    implicit def laws[T](implicit numeric: Numeric[T]) = Laws[self.type, T](monoid[T] :: HNilF[T]())
  }

  // Scope for Numeric Product monoid
  object ProdScope { self =>
    def monoid[T](implicit numeric: Numeric[T]): Monoid[T] = new Monoid[T] {
      def append(a: T, b: T): T = numeric.times(a, b)
    }

    implicit def laws[T](implicit numeric: Numeric[T]) = Laws[self.type, T](monoid[T] :: HNilF[T]())
  }


  WithScope(SumScope)  { implicit s => 
    assert(append(5, 3) == 8)
    assert(append(5.234, 9.876) == 15.11)
  }
  
  WithScope(ProdScope) { implicit s =>
    assert(append(5, 3) == 15)
    assert(append(5.234, 9.876) == 51.690984)
  }
  
  class NumericField[N](
    implicit
        N : Numeric[N]
      , Sum: HasLaw[SumScope.type, N, Monoid[N]]
      , Product: HasLaw[ProdScope.type, N, Monoid[N]]
  ) {
    def sum(lhs: N, rhs: N) = Sum.law.append(lhs, rhs)
    def product(lhs: N, rhs: N) = Product.law.append(lhs, rhs)
  }

  val intField = new NumericField[Int]

  assert(intField.sum(3, 5) == 8)
  assert(intField.product(3, 5) == 15)

}
