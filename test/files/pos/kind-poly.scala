object Test {

  case class Bar[A](a: A)
  trait Toto[A, B]

  ////////////////////////////////////////////////
  // PoC of controlled KindPolymorphism in Scala
  //
  // The idea is NOT to provide universal kind-polymorphism that would be a bad idea anyway
  // but to bring a "controlled" kind-polymorphism relying on accepted kinds defined by typeclass implicits
  // Thus, kind-polymorphism is strictly scoped to your domain and is what you expect to be, nothing else.
  //
  // `Ykind-polymorphism` flag aims at deferring just a bit Scalac type inference when encountering AnyKind higher bounds
  // without losing any strictness in the final typing.
  // `<: AnyKind` type-bound is purely technicaland totally eliminated after erasure. There is not type associated to it.
  // 
  // Here are code-samples that work now:
  //    - basic kind polymorphism controlled by implicits
  //    - Kindness proofs based on typeclasses (specially SameKind)
  //    - Kind-Polymorphic list (on type & value) (2 different implementations)
  //    - Some weird cases we don't want the compiler to authorize

  ////////////////////////////////////////////////
  // Basic Kind polymorphism sample
  trait Foo[T <: AnyKind] { type Out ; def id(t: Out): Out = t }

  object Foo {
    implicit def foo0[T] = new Foo[T] { type Out = T }
    implicit def foo1[T[_]] = new Foo[T] { type Out = T[Any] }
    implicit def foo2[T[_, _]] = new Foo[T] { type Out = T[Any, Any] }
  }

  def foo[T <: AnyKind](implicit f: Foo[T]): f.type = f
  foo[Int].id(23)
  foo[List].id(List[Any](1, 2, 3))
  foo[Map].id(Map[Any, Any](1 -> "toto", 2 -> "tata", 3 -> "tutu"))

  ////////////////////////////////////////////////
  // Is a type M Kinded as you want ?
  trait Kinded[M <: AnyKind] { type Out <: AnyKind }
  object Kinded {
    type Aux[M <: AnyKind, Out0 <: AnyKind] = Kinded[M] { type Out = Out0 }

    implicit def kinded0[M]: Aux[M, M] = new Kinded[M] { type Out = M }
    implicit def kinded1[M[_]]: Aux[M, M] = new Kinded[M] { type Out[t] = M[t] }
    implicit def kinded2[M[_, _]]: Aux[M, M] = new Kinded[M] { type Out[t, u] = M[t, u] }
  }

  implicitly[Kinded.Aux[Int, Int]]
  implicitly[Kinded.Aux[List, List]]
  implicitly[Kinded.Aux[Map, Map]]

  ////////////////////////////////////////////////
  // Extract Kind from a type
  trait Kinder[MA] { type M <: AnyKind; type Args <: HList }
  object Kinder extends KinderLowerImplicits {
    type Aux[MA, M0 <: AnyKind, Args0 <: HList] = Kinder[MA] { type M = M0; type Args = Args0 }

    implicit def kinder2[M0[_, _], A0, B0]: Kinder.Aux[M0[A0, B0], M0, A0 :: B0 :: HNil] = new Kinder[M0[A0, B0]] { type M[t, u] = M0[t, u]; type Args = A0 :: B0 :: HNil }
    implicit def kinder1[M0[_], A0]: Kinder.Aux[M0[A0], M0, A0 :: HNil] = new Kinder[M0[A0]] { type M[t] = M0[t]; type Args = A0 :: HNil }
  }

  trait KinderLowerImplicits {
    implicit def kinder0[A]: Kinder.Aux[A, A, HNil] = new Kinder[A] { type M = A; type Args = HNil }    
  }

  ////////////////////////////////////////////////
  //IsoKindness Test
  trait SameKind[M <: AnyKind, M2 <: AnyKind]
  object SameKind {

    implicit def sameKind0[A, B] = new SameKind[A, B] {}
    implicit def sameKind01[M1[_], M2[_]] = new SameKind[M1, M2] {}
    implicit def sameKind02[M1[_, _], M2[_, _]] = new SameKind[M1, M2] {}
  }

  def sameKind[M1 <: AnyKind, M2 <: AnyKind](implicit sameKind: SameKind[M1, M2]) = sameKind

  sameKind[Int, String]     // OK
  sameKind[List, Bar]       // OK
  sameKind[Map, Toto]       // OK

  // sameKind[List, String] // KO
  // sameKind[Map, List]    // KO
  // sameKind[Map, Boolean] // KO



  ////////////////////////////////////////////////
  // Kind-Polymorphic List style

  // Classic Heterogenous List used in KindPolymorphic List
  sealed trait HList
  final case class ::[+H, +T <: HList](head : H, tail : T) extends HList
  sealed trait HNil extends HList
  final case object HNil extends HNil

  object New {
    // The Kind Polymorphic List
    sealed trait KPList

    sealed trait KPNil extends KPList
    case object KPNil extends KPNil {
      def :::[H, M <: AnyKind, HL <: HList](h:H)(implicit kinder: Kinder.Aux[H, M, HL]) =
        New.:::(h, KPNil)
    }
    
    sealed case class :::[H, T <: KPList, M <: AnyKind, HL0 <: HList](
      head: H
    , tail: T
    )(implicit val kinder: Kinder.Aux[H, M, HL0]) extends KPList

    final case class KPListOps[L <: KPList](l : L) {
      def :::[H, M <: AnyKind, HL <: HList](h:H)(implicit kinder: Kinder.Aux[H, M, HL]) =
        New.:::(h, l)
    }

    implicit def kplistOps[L <: KPList](l: L): KPListOps[L] = new KPListOps(l)

    val kl = Bar(5) ::: "toto" ::: List(1, 2, 3) ::: Map("toto" -> 1L, "tata" -> 2L) ::: KPNil

    val h: Bar[Int] = kl.head
    val h2: String = kl.tail.head
    val h3: List[Int] = kl.tail.tail.head
    val h4: Map[String, Long] = kl.tail.tail.tail.head

  }


  ////////////////////////////////////////////////
  // SPECIAL CASES
  def foo0[F <: AnyKind]: F = null.asInstanceOf[F]
  val i = foo0[Int]             // OK
  val li = foo0[List[Int]]      // OK
  // foo0[List]                // KO -> neg
  // val l = foo0[List]        // KO -> neg

  // def foo1[F <: AnyKind, A <: AnyKind]: F[A] = ??? // KO

  // def foo2: AnyKind = ??? // KO


  // Older implementation Kind-Polymorphic List but I prefer the one above
  object Old {

    // The Kind Polymorphic List
    sealed trait KPList

    sealed trait KPNil extends KPList
    case object KPNil extends KPNil
    
    sealed trait :::[H <: AnyKind, T <: KPList] extends KPList
    trait KPCons[M <: AnyKind, T <: KPList] extends :::[M, T]  {
      type HL <: HList
      type H
      def head: H
      def tail: T
    }

    object KPCons {
      type Aux[M <: AnyKind, T <: KPList, H0, HL0 <: HList] = KPCons[M, T] { type H = H0; type HL = HL0 }
      // Polymorphic 
      trait Apply[M <: AnyKind, A <: HList] { type Out }
      object Apply {
        type Aux[M <: AnyKind, A <: HList, Out0] = Apply[M, A] { type Out = Out0 }
        implicit def apply0[M]: Aux[M, HNil, M] = new Apply[M, HNil] { type Out = M }
        implicit def apply1[M[_], A]: Aux[M, A :: HNil, M[A]] = new Apply[M, A :: HNil] { type Out = M[A] }
        implicit def apply2[M[_, _], A, B]: Aux[M, A :: B :: HNil, M[A, B]] = new Apply[M, A :: B :: HNil] { type Out = M[A, B] }
      }

      trait Unapply[M <: AnyKind, O] { type Out <: HList }
      object Unapply {
        type Aux[M <: AnyKind, O, Out0 <: HList] = Unapply[M, O] { type Out = Out0 }

        implicit def unapply0[M]: Aux[M, M, HNil] = new Unapply[M, M] { type Out = HNil }
        implicit def unapply1[M[_], A0]: Unapply.Aux[M, M[A0], A0 :: HNil] = new Unapply[M, M[A0]] { type Out = A0 :: HNil }
        implicit def unapply2[M[_, _], A0, B0]: Aux[M, M[A0, B0], A0 :: B0 :: HNil] = new Unapply[M, M[A0, B0]] { type Out = A0 :: B0 :: HNil }
      }

      // the list builder
      trait KPConsBuilder[M <: AnyKind] {
        def apply[H0, HL0 <: HList, T <: KPList](head0: H0, tail0: T)(implicit unap: Unapply.Aux[M, H0, HL0]): KPCons.Aux[M, T, H0, HL0] = new KPCons[M, T] {
          type HL = HL0
          type H = H0
          val head: H = head0
          val tail: T = tail0
        }
      }

      def apply[M <: AnyKind] = new KPConsBuilder[M] {}
    }


    // Let's create some kind-polymorphic list
    val kl = 
      KPCons[Bar](
        Bar(5)
      , KPCons[String](
          "toto"
        , KPCons[List](
            List(1, 2, 3)
          , KPCons[Map](
              Map("toto" -> 1L, "tata" -> 2L)
            , KPNil
            )
          )
        )
      )

    val h: Bar[Int] = kl.head
    val h2: String = kl.tail.head
    val h3: List[Int] = kl.tail.tail.head
    val h4: Map[String, Long] = kl.tail.tail.tail.head

  }

}
