object Test {
  case class Bar[A](a: A)
  trait Toto[A, B]

  //////
  ////////////////////////////////////////////////
  // Extract Kind from a type
  trait Kinder[MA] { type M <: AnyKind }
  object Kinder extends KinderLowerImplicits {
    type Aux[MA, M0 <: AnyKind] = Kinder[MA] { type M = M0 }

    implicit def kinder1[M0[_], A0]: Kinder.Aux[M0[A0], M0] = new Kinder[M0[A0]] { type M[t] = M0[t] }
    implicit def kinder2[M0[_, _], A0, B0]: Kinder.Aux[M0[A0, B0], M0] = new Kinder[M0[A0, B0]] { type M[t, u] = M0[t, u]; type Args = A0 :: B0 :: HNil }
  }
  trait KinderLowerImplicits {
    implicit def kinder0[A]: Kinder.Aux[A, A] = new Kinder[A] { type M = A; type Args = HNil }    
  }

  // Classic Heterogenous List used in KindPolymorphic List
  sealed trait HList
  final case class ::[+H, +T <: HList](head : H, tail : T) extends HList
  sealed trait HNil extends HList {
    def ::[H](h : H) = Test.::(h, this)
  }
  final case object HNil extends HNil

  trait SemiGroup[M <: AnyKind] {
    // Just a mirror type of itself to ensure the owning of AppendFunction...
    type Self
    def append[MA](m1: MA, m2: MA)(implicit appender: SemiGroup.AppendFunction[Self, MA, M]) = appender(m1, m2)
  }

  object SemiGroup {
    type Aux[M <: AnyKind, Self0] = SemiGroup[M] { type Self = Self0 }

    trait AppendFunction[P, FA, F <: AnyKind] {
      def apply(m1: FA, m2: FA): FA
    }
  }

  implicit object SemiGroupInt extends SemiGroup[Int] {
    type Self = this.type
    implicit val appender = new SemiGroup.AppendFunction[Self, Int, Int] {
      def apply(m1: Int, m2: Int) = m1 + m2
    }
  }

  implicit object SemiGroupList extends SemiGroup[List] {
    type Self = this.type
    implicit def appender[A] = new SemiGroup.AppendFunction[Self, List[A], List] {
      def apply(m1: List[A], m2: List[A]) = m1 ++ m2
    }
  }

  implicit object SemiGroupMap extends SemiGroup[Map] {
    type Self = this.type
    implicit def appender[A, B] = new SemiGroup.AppendFunction[Self, Map[A, B], Map] {
      def apply(m1: Map[A, B], m2: Map[A, B]) = m1 ++ m2
    }
  }

  // Searching a semigroup and using it
  def semiGroup[M <: AnyKind](implicit sg: SemiGroup[M]): SemiGroup.Aux[M, sg.Self] = sg

  semiGroup[Int].append(5, 8)
  semiGroup[List].append(List(1), List(3))
  semiGroup[Map].append(Map("toto" -> 1L), Map("tata" -> 3L))

  // higher level append function
  def append[MA, M <: AnyKind, Self](m1: MA, m2: MA)(
    implicit kinder: Kinder.Aux[MA, M], semiGroup: SemiGroup.Aux[M, Self], appender: SemiGroup.AppendFunction[Self, MA, M]
  ): MA = semiGroup.append(m1, m2)

  val r1: Int = append(5, 8)   // OK
  val r2: List[Int] = append(List(1), List(3))    // KO -> certainly a bug in propagating an implicit with an AnyKind type
  val r3: Map[String, Long] = append(Map("toto" -> 1L), Map("tata" -> 3L))

}
