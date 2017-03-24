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

  trait Prepend[P <: HList, S <: HList] {
    type Out <: HList
    def apply(p: P, s: S): Out
  }

  trait LowestPriorityPrepend {
    type Aux[P <: HList, S <: HList, Out0 <: HList] = Prepend[P, S] { type Out = Out0 }

    implicit def hlistPrepend[PH, PT <: HList, S <: HList]
     (implicit pt : Prepend[PT, S]): Prepend.Aux[PH :: PT, S, PH :: pt.Out] =
      new Prepend[PH :: PT, S] {
        type Out = PH :: pt.Out
        def apply(prefix : PH :: PT, suffix : S): Out = prefix.head :: pt(prefix.tail, suffix)
      }
  }

  trait LowPriorityPrepend extends LowestPriorityPrepend {
    /**
     * Binary compatibility stub
     * This one is for https://github.com/milessabin/shapeless/issues/406
     */
    override type Aux[P <: HList, S <: HList, Out0 <: HList] = Prepend[P, S] { type Out = Out0 }

    implicit def hnilPrepend0[P <: HList, S <: HNil]: Aux[P, S, P] =
      new Prepend[P, S] {
        type Out = P
        def apply(prefix : P, suffix : S): P = prefix
      }
  }

  object Prepend extends LowPriorityPrepend {
    def apply[P <: HList, S <: HList](implicit prepend: Prepend[P, S]): Aux[P, S, prepend.Out] = prepend

    implicit def hnilPrepend1[P <: HNil, S <: HList]: Aux[P, S, S] =
      new Prepend[P, S] {
        type Out = S
        def apply(prefix : P, suffix : S): S = suffix
      }
  }

  abstract class Case[P, L <: HList] extends Serializable {
    type Result
    val value : L => Result

    def apply(t : L) = value(t)
    def apply()(implicit ev: HNil =:= L) = value(HNil)
    def apply[T](t: T)(implicit ev: (T :: HNil) =:= L) = value(t :: HNil)
    def apply[T, U](t: T, u: U)(implicit ev: (T :: U :: HNil) =:= L) = value(t :: u :: HNil)
  }

  trait CaseInst {
    implicit def inst1
      [Fn <: Poly, A, Res]
      (cse : Case[Fn, A::HNil] { type Result = Res })
    : (A) => Res =
      (a:A)
        => cse.value(a::HNil)
  }

  object Case extends CaseInst {
    type Aux[P, L <: HList, Result0] = Case[P, L] { type Result = Result0 }
    type Hom[P, T] = Aux[P, T :: HNil, T]

    def apply[P, L <: HList, R](v : L => R): Aux[P, L, R] = new Case[P, L] {
      type Result = R
      val value = v
    }
  }

  type Case1[Fn, A]
    = Case[Fn, A::HNil]

  object Case1 {
    type Aux[Fn, A, Result0]
      = Case[Fn, A::HNil] { type Result = Result0 }

    def apply
      [Fn, A, Result0]
      (fn: (A) => Result0)
    : Aux[Fn, A, Result0] =
      new Case[Fn, A::HNil] {
        type Result = Result0
        val value = (l: A::HNil)
          => l match {
            case a::HNil =>
              fn(a)
          }
      }
  }

  trait PolyInst {
    implicit def inst1
      [A]
      (fn : Poly)(implicit cse : fn.ProductCase[A::HNil])
    : (A) => cse.Result =
      (a:A)
        => cse(a::HNil)
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
  }

  trait Poly1 extends Poly { outer =>
    type Case[A]
      = shapeless.Case[this.type, A::HNil]

    object Case {
      type Aux[A, Result0]
        = shapeless.Case[outer.type, A::HNil] { type Result = Result0 }
    }

    class CaseBuilder[A] {
      def apply[Res]
        (fn: (A) => Res) = new Case[A] {
        type Result = Res
        val value = (l: A::HNil)
          => l match { case a::HNil => fn(a) }
      }
    }

    def at[A]
      = new CaseBuilder[A]
  }

  trait FlatMapper[HF, In <: HList] {
    type Out <: HList
    def apply(l: In): Out
  }

  object FlatMapper {
    def apply[F, L <: HList](implicit mapper: FlatMapper[F, L]): Aux[F, L, mapper.Out] = mapper

    type Aux[HF, In <: HList, Out0 <: HList] = FlatMapper[HF, In] { type Out = Out0 }

    implicit def hnilFlatMapper1[HF]: Aux[HF, HNil, HNil] =
      new FlatMapper[HF, HNil] {
        type Out = HNil
        def apply(l : HNil): Out = HNil
      }

    implicit def hlistFlatMapper1[HF, InH, OutH <: HList, InT <: HList, OutT <: HList, Out0 <: HList]
      (implicit
        hc : Case1.Aux[HF, InH, OutH],
        mt : FlatMapper.Aux[HF, InT, OutT],
        prepend : Prepend.Aux[OutH, OutT, Out0]
      ): Aux[HF, InH :: InT, Out0] =
        new FlatMapper[HF, InH :: InT] {
          type Out = Out0
          def apply(l : InH :: InT): Out = prepend(hc(l.head), mt(l.tail))
        }
  }
}

object FlattenExample {
  import shapeless._

  trait LowPriorityFlatten extends Poly1 {
    implicit def default = at[Int](_ :: HNil)
  }
  object flatten extends LowPriorityFlatten {
    implicit def caseTuple[L <: HList](implicit lfm: FlatMapper[flatten.type, L]) =
      at[L](lfm(_))
  }

  val t1 = (2 :: HNil) :: 4 :: HNil
  val f1 = flatten(t1)
  val f2: Int :: Int :: HNil = f1
}
