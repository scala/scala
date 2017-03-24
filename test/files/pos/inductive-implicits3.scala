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
    def union[M <: HList](s: M)(implicit union: Union[L, M]): union.Out = union(l, s)
  }

  object HList {
    implicit def hlistOps[L <: HList](l: L): HListOps[L] = new HListOps(l)
  }

  trait FilterNot[L <: HList, U] {
    type Out <: HList
    def apply(l: L): Out
  }

  object FilterNot {
    def apply[L <: HList, U](implicit filter: FilterNot[L, U]): Aux[L, U, filter.Out] = filter

    type Aux[L <: HList, U, Out0 <: HList] = FilterNot[L, U] { type Out = Out0 }

    implicit def hlistFilterNot[L <: HList, U, LPrefix <: HList, LSuffix <: HList](
      implicit partition: Partition.Aux[L, U, LPrefix, LSuffix]
    ): Aux[L, U, LSuffix] = new FilterNot[L, U] {
      type Out = LSuffix

      def apply(l: L): Out = partition.filterNot(l)
    }
  }

  trait Remove[L <: HList, E] {
    type Out
    def apply(l: L): Out
    def reinsert(out: Out): L
  }

  trait LowPriorityRemove {
    type Aux[L <: HList, E, Out0] = Remove[L, E] { type Out = Out0 }

    implicit def recurse[H, T <: HList, E, OutT <: HList](implicit r : Aux[T, E, (E, OutT)]): Aux[H :: T, E, (E, H :: OutT)] =
      new Remove[H :: T, E] {
        type Out = (E, H :: OutT)
        def apply(l : H :: T): Out = {
          val (e, tail) = r(l.tail)
          (e, l.head :: tail)
        }

        def reinsert(out: (E, H :: OutT)): H :: T = out._2.head :: r.reinsert((out._1, out._2.tail))
      }
  }

  object Remove extends LowPriorityRemove {
    def apply[L <: HList, E](implicit remove: Remove[L, E]): Aux[L, E, remove.Out] = remove

    implicit def remove[H, T <: HList]: Aux[H :: T, H, (H, T)] =
      new Remove[H :: T, H] {
        type Out = (H, T)
        def apply(l : H :: T): Out = (l.head, l.tail)

        def reinsert(out: (H, T)): H :: T = out._1 :: out._2
      }
  }

  object typeops {
    trait =:!=[A, B] extends Serializable

    implicit def neq[A, B] : A =:!= B = new =:!=[A, B] {}
    implicit def neqAmbig1[A] : A =:!= A = ???
    implicit def neqAmbig2[A] : A =:!= A = ???
  }

  trait Partition[L <: HList, U] {
    type Prefix <: HList
    type Suffix <: HList
    type Out = (Prefix, Suffix)

    def apply(l: L): Out = toTuple2(product(l))
    def product(l: L): Prefix :: Suffix :: HNil = filter(l) :: filterNot(l) :: HNil
    def filter(l: L): Prefix
    def filterNot(l: L): Suffix

    def toTuple2[Prefix, Suffix](l: Prefix :: Suffix :: HNil): (Prefix, Suffix) = (l.head, l.tail.head)
  }

  object Partition {
    import typeops._

    def apply[L <: HList, U]
      (implicit partition: Partition[L, U]): Aux[L, U, partition.Prefix, partition.Suffix] = partition

    type Aux[L <: HList, U, Prefix0 <: HList, Suffix0 <: HList] = Partition[L, U] {
      type Prefix = Prefix0
      type Suffix = Suffix0
    }

    implicit def hlistPartitionNil[U]: Aux[HNil, U, HNil, HNil] = new Partition[HNil, U] {
      type Prefix = HNil
      type Suffix = HNil

      def filter(l: HNil): HNil = HNil
      def filterNot(l: HNil): HNil = HNil
    }

    implicit def hlistPartition1[H, L <: HList, LPrefix <: HList, LSuffix <: HList](
      implicit p: Aux[L, H, LPrefix, LSuffix]
    ): Aux[H :: L, H, H :: LPrefix, LSuffix] = new Partition[H :: L, H] {
      type Prefix = H :: LPrefix
      type Suffix = LSuffix

      def filter(l: H :: L): Prefix    = l.head :: p.filter(l.tail)
      def filterNot(l: H :: L): Suffix = p.filterNot(l.tail)
    }

    implicit def hlistPartition2[H, L <: HList, U, LPrefix <: HList, LSuffix <: HList](
      implicit p: Aux[L, U, LPrefix, LSuffix], e: U =:!= H
    ): Aux[H :: L, U, LPrefix, H :: LSuffix] = new Partition[H :: L, U] {
      type Prefix = LPrefix
      type Suffix = H :: LSuffix

      def filter(l: H :: L): Prefix    = p.filter(l.tail)
      def filterNot(l: H :: L): Suffix = l.head :: p.filterNot(l.tail)
    }
  }

  trait Union[L <: HList, M <: HList] {
    type Out <: HList
    def apply(l: L, m: M): Out
  }

  trait LowPriorityUnion {
    type Aux[L <: HList, M <: HList, Out0 <: HList] = Union[L, M] { type Out = Out0 }

    // buggy version; let (H :: T) ∪ M  =  H :: (T ∪ M)
    @deprecated("Incorrectly witnesses that {x} ∪ {x} = {x, x}", "2.3.1")
    def hlistUnion1[H, T <: HList, M <: HList]
      (implicit u: Union[T, M]): Aux[H :: T, M, H :: u.Out] =
        new Union[H :: T, M] {
          type Out = H :: u.Out
          def apply(l: H :: T, m: M): Out = l.head :: u(l.tail, m)
        }
  }

  object Union extends LowPriorityUnion {
    def apply[L <: HList, M <: HList](implicit union: Union[L, M]): Aux[L, M, union.Out] = union

    // let ∅ ∪ M = M
    implicit def hlistUnion[M <: HList]: Aux[HNil, M, M] =
      new Union[HNil, M] {
        type Out = M
        def apply(l: HNil, m: M): Out = m
      }

    // let (H :: T) ∪ M  =  H :: (T ∪ M) when H ∉ M
    implicit def hlistUnion1[H, T <: HList, M <: HList]
      (implicit
       u: Union[T, M],
       f: FilterNot.Aux[M, H, M]
      ): Aux[H :: T, M, H :: u.Out] =
        new Union[H :: T, M] {
          type Out = H :: u.Out
          def apply(l: H :: T, m: M): Out = l.head :: u(l.tail, m)
        }

    // let (H :: T) ∪ M  =  H :: (T ∪ (M - H)) when H ∈ M
    implicit def hlistUnion2[H, T <: HList, M <: HList, MR <: HList]
      (implicit
        r: Remove.Aux[M, H, (H, MR)],
        u: Union[T, MR]
      ): Aux[H :: T, M, H :: u.Out] =
        new Union[H :: T, M] {
          type Out = H :: u.Out
          def apply(l: H :: T, m: M): Out = l.head :: u(l.tail, r(m)._2)
        }
  }

  // should be in neg
  //implicitly[Union.Aux[Int :: HNil, Int :: HNil, Int :: String :: HNil]]

  val u = Union[Int :: HNil, Int :: HNil]
  val v: Union.Aux[Int :: HNil, Int :: HNil, Int :: HNil] = u

  val u1 = Union[String :: Long :: HNil, Int :: String :: Boolean :: HNil]
  val v1: Union.Aux[String :: Long :: HNil, Int :: String :: Boolean :: HNil, String :: Long :: Int :: Boolean :: HNil] = u1

  type L1 = String :: Long :: HNil
  val l1: L1 = "foo" :: 3L :: HNil

  type L2 = Int :: String :: Boolean :: HNil
  val l2: L2 = 2 :: "bar" :: true :: HNil

  val l12 = l1.union(l2)
}
