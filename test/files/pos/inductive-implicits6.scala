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

  trait ~>[F[_], G[_]]

  trait NatTRel[L1 <: HList, F1[_], L2 <: HList, F2[_]] extends Serializable {
    def map(nt: F1 ~> F2, fa: L1): L2
  }

  object NatTRel {
    def apply[L1 <: HList, F1[_], L2 <: HList, F2[_]](implicit natTRel: NatTRel[L1, F1, L2, F2]) = natTRel

    implicit def hnilNatTRel1[F1[_], F2[_]] = new NatTRel[HNil, F1, HNil, F2] {
      def map(f: F1 ~> F2, fa: HNil): HNil = HNil
    }

    implicit def hlistNatTRel1[H, F1[_], F2[_], T1 <: HList, T2 <: HList](implicit nt : NatTRel[T1, F1, T2, F2]) =
      new NatTRel[F1[H] :: T1, F1, F2[H] :: T2, F2] {
        def map(f: F1 ~> F2, fa: F1[H] :: T1): F2[H] :: T2 = ???
      }
  }

  type N = Nothing
  implicitly[NatTRel[Option[N] :: HNil, Option, List[N] :: HNil, List]]
}
