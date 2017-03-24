object shapeless {
  sealed trait HList extends Product with Serializable

  final case class ::[+H, +T <: HList](head : H, tail : T) extends HList {
    def ::[HH](h : HH) : HH :: H :: T = shapeless.::(h, this)

    override def toString = head match {
      case _: ::[_, _] => "("+head.toString+") :: "+tail.toString
      case _ => head.toString+" :: "+tail.toString
    }
  }

  sealed trait HNil extends HList {
    def ::[H](h : H) = shapeless.::(h, this)
    override def toString = "HNil"
  }

  case object HNil extends HNil

  final class HListOps[L <: HList](l : L) {
    def ::[H](h : H) : H :: L = shapeless.::(h, l)
  }

  object HList {
    implicit def hlistOps[L <: HList](l: L): HListOps[L] = new HListOps(l)
  }

  abstract class Case[P, L <: HList] extends Serializable {
    type Result
    val value : L => Result

    def apply(t : L) = value(t)
    def apply()(implicit ev: HNil =:= L) = value(HNil)
    def apply[T](t: T)(implicit ev: (T :: HNil) =:= L) = value(t :: HNil)
    def apply[T, U](t: T, u: U)(implicit ev: (T :: U :: HNil) =:= L) = value(t :: u :: HNil)
  }

  trait FnToProduct[-F] extends Serializable {
    type Out
    def apply(f: F): Out
  }

  trait FnToProductInstances {
    type Aux[F, Out0] = FnToProduct[F] { type Out = Out0 }

    implicit def fnToProduct1
      [A, Res]
    : Aux[
      ((A) => Res),
      (A::HNil) => Res
    ] =
      new FnToProduct[(A) => Res] {
        type Out = (A::HNil) => Res
        def apply(fn: (A) => Res): Out
          = (l : A::HNil)
            => l match { case a::HNil => fn(a) }
      }

    implicit def fnToProduct2
      [A, B, Res]
    : Aux[
      ((A, B) => Res),
      (A::B::HNil) => Res
    ] =
      new FnToProduct[(A, B) => Res] {
        type Out = (A::B::HNil) => Res
        def apply(fn: (A, B) => Res): Out
          = (l : A::B::HNil)
            => l match { case a::b::HNil => fn(a, b) }
      }
  }

  object FnToProduct extends FnToProductInstances {
    def apply[F <: AnyRef](implicit fntop: FnToProduct[F]): Aux[F, fntop.Out] = fntop
  }

  trait CaseInst {
    implicit def inst1
      [Fn <: Poly, A, Res]
      (cse : Case[Fn, A::HNil] { type Result = Res })
    : (A) => Res =
      (a:A)
        => cse.value(a::HNil)

    implicit def inst2
      [Fn <: Poly, A, B, Res]
      (cse : Case[Fn, A::B::HNil] { type Result = Res })
    : (A, B) => Res =
      (a:A, b:B)
        => cse.value(a::b::HNil)
  }

  object Case extends CaseInst {
    type Aux[P, L <: HList, Result0] = Case[P, L] { type Result = Result0 }
    type Hom[P, T] = Aux[P, T :: HNil, T]

    def apply[P, L <: HList, R](v : L => R): Aux[P, L, R] = new Case[P, L] {
      type Result = R
      val value = v
    }
  }

  trait PolyInst {
    implicit def inst1
      [A]
      (fn : Poly)(implicit cse : fn.ProductCase[A::HNil])
    : (A) => cse.Result =
      (a:A)
        => cse(a::HNil)

    implicit def inst2
      [A, B]
      (fn : Poly)(implicit cse : fn.ProductCase[A::B::HNil])
    : (A, B) => cse.Result =
      (a:A, b:B)
        => cse(a::b::HNil)
  }

  object Poly extends PolyInst {
    implicit def inst0(p: Poly)(implicit cse : p.ProductCase[HNil]) : cse.Result = cse()
  }

  trait PolyApply {
    def apply
      [A]
      (a:A)
      (implicit cse : Case[this.type, A::HNil])
    : cse.Result =
      cse(a::HNil)

    def apply
      [A, B]
      (a:A, b:B)
      (implicit cse : Case[this.type, A::B::HNil])
    : cse.Result =
      cse(a::b::HNil)
  }

  trait Poly extends PolyApply with Serializable {
    /** The type of the case representing this polymorphic function at argument types `L`. */
    type ProductCase[L <: HList] = Case[this.type, L]
    object ProductCase extends Serializable {
      /** The type of a case of this polymorphic function of the form `L => R` */
      type Aux[L <: HList, Result0] = ProductCase[L] { type Result = Result0 }

      /** The type of a case of this polymorphic function of the form `T => T` */
      type Hom[T] = Aux[T :: HNil, T]

      def apply[L <: HList, R](v : L => R) = new ProductCase[L] {
        type Result = R
        val value = v
      }
    }

    def use[T, L <: HList, R](t : T)(implicit cb: CaseBuilder[T, L, R]) = cb(t)

    trait CaseBuilder[T, L <: HList, R] extends Serializable {
      def apply(t: T): ProductCase.Aux[L, R]
    }

    trait LowPriorityCaseBuilder {
      implicit def valueCaseBuilder[T]: CaseBuilder[T, HNil, T] =
        new CaseBuilder[T, HNil, T] {
          def apply(t: T) = ProductCase((_: HNil) => t)
        }
    }

    object CaseBuilder extends LowPriorityCaseBuilder {
      implicit def fnCaseBuilder[F, H, T <: HList, Result]
        (implicit fntp: FnToProduct.Aux[F, ((H :: T) => Result)]): CaseBuilder[F, H :: T, Result] =
          new CaseBuilder[F, H :: T, Result] {
            def apply(f: F) = ProductCase((l : H :: T) => fntp(f)(l))
          }
    }

    def caseAt[L <: HList](implicit c: ProductCase[L]) = c

    def apply[R](implicit c : ProductCase.Aux[HNil, R]) : R = c()
  }

  object smear extends Poly {
    implicit val caseIntInt    = use((x: Int) => x)
    //implicit val caseIntInt    = use((x: Int) => x)
    //implicit val caseStringInt = use((x: String, y: Int) => x.toInt + y)
    //implicit val caseIntString = use((x: Int, y: String) => x + y.toInt)
  }

  smear(13)
}
