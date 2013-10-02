sealed trait HList
final case class HCons[H, T <: HList](head : H, tail : T) extends HList
case object HNil extends HList

object HList {
  type ::[H, T <: HList] = HCons[H, T]
  type HNil = HNil.type

  implicit def hlistOps[L <: HList](l : L) = new {
    def ::[H](h : H) : H :: L = HCons(h, l)
    def last(implicit last : Last[L]) {}
  }

  class Last[L <: HList]
  implicit def hsingleLast[H] = new Last[H :: HNil]
  implicit def hlistLast[H, T <: HList](implicit lt : Last[T]) = new Last[H :: T]

  type III = Int :: Int :: Int :: HNil
  val iii : III = 0 :: 0 :: 0 :: HNil
  val l = iii.last
}
