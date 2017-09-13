package shapeless {
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

  trait Case1[HF, In] {
    type Result
    def apply(t: In): Result
  }

  object Case1 {
    type Aux[HF, In, Result0] = Case1[HF, In] { type Result = Result0 }
  }

  @annotation.inductive
  trait Mapper[HF, In <: HList] {
    type Out <: HList
    def apply(in: In): Out
  }

  object Mapper {
    def apply[F, L <: HList](implicit mapper: Mapper[F, L]): Aux[F, L, mapper.Out] = mapper

    type Aux[HF, In <: HList, Out0 <: HList] = Mapper[HF, In] { type Out = Out0 }

    implicit def hnilMapper1[HF]: Aux[HF, HNil, HNil] =
      new Mapper[HF, HNil] {
        type Out = HNil
        def apply(l : HNil): Out = HNil
      }

    implicit def hlistMapper1[HF, InH, InT <: HList]
      (implicit hc : Case1[HF, InH], mt : Mapper[HF, InT]): Aux[HF, InH :: InT, hc.Result :: mt.Out] =
        new Mapper[HF, InH :: InT] {
          type Out = hc.Result :: mt.Out
          def apply(l : InH :: InT): Out = hc(l.head) :: mt(l.tail)
        }
  }
}

import shapeless._

object Test extends App {
  type L = Int :: String :: Boolean :: HNil

  object Fn {
    implicit val caseInt: Case1.Aux[Fn.type, Int, Int] = new Case1[Fn.type, Int] { type Result = Int ; def apply(t: Int): Int = t }
    //implicit val caseString: Case1.Aux[Fn.type, String, String] = new Case1[Fn.type, String] { type Result = String ; def apply(t: String): String = t }
    implicit val caseBoolean: Case1.Aux[Fn.type, Boolean, Boolean] = new Case1[Fn.type, Boolean] { type Result = Boolean ; def apply(t: Boolean): Boolean = t }
  }

  val map = Mapper[Fn.type, L]
  map: Mapper.Aux[Fn.type, L, L]
}
