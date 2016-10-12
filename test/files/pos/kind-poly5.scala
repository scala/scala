object Test {

  // PKList or the Any-Order heterogenous list
  sealed trait PKList[Args <: AnyKind]

  trait PKNil[Args <: AnyKind] extends PKList[Args]

  sealed trait PKCons[Args <: AnyKind, HA, TA <: PKList[Args]] extends PKList[Args] {
    def head: HA
    def tail: TA
  }

  object PKList {

    trait PKConsBuilder[Args <: AnyKind, HA, UL <: PKList[Args]] {
      type OutA
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

  }


  object PKConsBuilder1 {
    import PKList._

    sealed trait HNil extends PKNil[Nothing] {
      def ::[H](h : H) = PKConsBuilder1.::(h, this)
    }
    final case object HNil extends HNil
    final case class ::[H, T <: PKList[Nothing]](head : H, tail : T) extends PKCons[Nothing, H, T] {
      def ::[H2](h : H2) = PKConsBuilder1.::(h, this)
    }

    // implicit def build0cons[H0, A, T <: PKList[Nothing]] =
    //   new PKConsBuilder[Nothing, H0, T] {
    //     type OutA = PKCons[Nothing, H0, T]

    //     def ::(l: T)(h: H0): OutA = new PKCons[Nothing, H0, T] {
    //       val head = h
    //       val tail = l
    //     }
    //   }

    implicit def build1cons[H0, A, T <: PKList[HNil]] =
      new PKConsBuilder[HNil, H0, T] {
        type OutA = PKCons[HNil, H0, T]

        def ::(l: T)(h: H0): OutA = new PKCons[HNil, H0, T] {
          val head = h
          val tail = l
        }
      }

    implicit def build2cons[H0[_], A, T <: PKList[A :: HNil]] =
      new PKConsBuilder[A :: HNil, H0[A], T] {
        type OutA = PKCons[A :: HNil, H0[A], T]

        def ::(l: T)(h: H0[A]): OutA = new PKCons[A :: HNil, H0[A], T] {
          val head = h
          val tail = l
        }
      }

    implicit def build3cons[H0[_, _], A, B, T <: PKList[A :: B :: HNil]] =
      new PKConsBuilder[A :: B :: HNil, H0[A, B], T] {
        type OutA = PKCons[A :: B :: HNil, H0[A, B], T]

        def ::(l: T)(h: H0[A, B]): OutA = new PKCons[A :: B :: HNil, H0[A, B], T] {
          val head = h
          val tail = l
        }
      }

    implicit def buildMCons[M[_[_]], F[_], T <: PKList[F]] =
      new PKConsBuilder[F, M[F], T] {
        type OutA = PKCons[F, M[F], T]

        def ::(l: T)(h: M[F]): OutA = new PKCons[F, M[F], T] {
          val head = h
          val tail = l
        }
      }


    case class Bar[A](a: A)
    class Toto[A, B]


    val l0 : Int :: String :: HNil = 5 :: "toto" :: HNil


    // List of *
    case object HNil1 extends PKNil[HNil]

    val l1 = 5 :: "toto" :: HNil1
    val i1: Int = l1.head
    val s1: String = l1.tail.head
    val t: PKNil[HNil] = l1.tail.tail

    // List of * -> *
    case class HNil2[A]() extends PKNil[A :: HNil]

    val l2 = List("tutu") :: Bar("tata") :: HNil2[String]()
    val h21: List[String] = l2.head
    val h22: Bar[String] = l2.tail.head
    val t2: PKNil[String :: HNil] = l2.tail.tail

    // val l21 = List(5) :: Bar("tata") :: HNil2() // KO

    // List of * -> * -> *
    case class HNil3[A, B]() extends PKNil[A :: B :: HNil]

    val l3 = Map("tutu" -> 42L) :: (new Toto[String, Long]) :: HNil3[String, Long]()
    val h31: Map[String, Long] = l3.head
    val h32: Toto[String, Long] = l3.tail.head
    val t3: PKNil[String :: Long :: HNil] = l3.tail.tail

    // val l31 = Map("tutu" -> 42L) :: HNil3[String, Int]() // KO 
    // val l32 = Map("tutu" -> 42L) :: (new Toto[String, Int]) :: HNil3() // KO


    // List of (* -> *) -> *
    case class HNilM[F[_]]() extends PKNil[F]

    trait Functor[F[_]]
    trait Traverse[F[_]]
    trait Foldable[F[_]]
    
    trait Foo[A]
    implicit val functor: Functor[Foo] = ???
    implicit val traverse: Traverse[Foo] = ???
    implicit val foldable: Foldable[Foo] = ???

    val m = functor :: traverse :: foldable :: HNilM[Foo]()

    val f: Functor[Foo] = m.head

    def build[F[_] : Functor : Traverse : Foldable] =
      implicitly[Functor[F]] :: implicitly[Traverse[F]] :: implicitly[Foldable[F]] :: HNilM[F]()

    build[Foo]
  }


}
