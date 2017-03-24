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

  sealed trait Coproduct extends Product with Serializable

  /** Like Either, the :+: type defines a new type that can contain either H or T.
    */
  sealed trait :+:[+H, +T <: Coproduct] extends Coproduct {
    /**
     * Non-recursive fold (like Either#fold)
     */
    def eliminate[A](l: H => A, r: T => A): A
  }

  /** `H :+: T` can either be `H` or `T`.
    * In this case it is `H`.
    */
  final case class Inl[+H, +T <: Coproduct](head : H) extends :+:[H, T] {
    override def eliminate[A](l: H => A, r: T => A) = l(head)
  }

  /** `H :+: T` can either be `H` or `T`.
    * In this case it is `T`.
    */
  final case class Inr[+H, +T <: Coproduct](tail : T) extends :+:[H, T] {
    override def eliminate[A](l: H => A, r: T => A) = r(tail)
  }

  sealed trait CNil extends Coproduct {
    def impossible: Nothing
  }

  trait Inject[C <: Coproduct, I] extends Serializable {
    def apply(i: I): C
  }

  object Inject {
    def apply[C <: Coproduct, I](implicit inject: Inject[C, I]): Inject[C, I] = inject

    implicit def tlInject[H, T <: Coproduct, I](implicit tlInj : Inject[T, I]): Inject[H :+: T, I] = new Inject[H :+: T, I] {
      def apply(i: I): H :+: T = Inr(tlInj(i))
    }

    implicit def hdInject[H, T <: Coproduct]: Inject[H :+: T, H] = new Inject[H :+: T, H] {
      def apply(i: H): H :+: T = Inl(i)
    }
  }
}

import shapeless._

sealed trait ToCoproductCodecs[C <: Coproduct, L <: HList]

object ToCoproductCodecs {
  implicit val base: ToCoproductCodecs[CNil, HNil] = new ToCoproductCodecs[CNil, HNil] {}

  implicit def step[A, CT <: Coproduct, LT <: HList](
    implicit tailAux: ToCoproductCodecs[CT, LT],
    inj: Inject[A :+: CT, A]
  ): ToCoproductCodecs[A :+: CT, A :: LT] = new ToCoproductCodecs[A :+: CT, A :: LT] {}
}

final class CoproductCodecBuilder[C <: Coproduct, L <: HList, R](codecs: L)(implicit aux: ToCoproductCodecs[C, L]) {
  def :+:[A](left: A): CoproductCodecBuilder[A :+: C, A :: L, A :+: C] =
    CoproductCodecBuilder(::(left, codecs))
}

object CoproductCodecBuilder {
  def apply[C <: Coproduct, L <: HList](l: L)(implicit aux: ToCoproductCodecs[C, L]): CoproductCodecBuilder[C, L, C] =
    new CoproductCodecBuilder[C, L, C](l)
}
