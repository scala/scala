object Test {
  case class Bar[A](a: A)
  trait Toto[A, B]

  /** A Typeclass indexed by a kind of morphism and able to:
    * 1/ extract a F[_*] from FA
    * 2/ apply M on it
    * 3/ return a GA which is a G[_*]
    */
  trait UnapplyApply[FA, F <: AnyKind, G <: AnyKind, GA, M <: Morphism[F, G]] {
    def apply(m: M)(g: FA): GA
  }

  /** a kind-polymorphic morphism */
  sealed trait Morphism[F <: AnyKind, G <: AnyKind] {
    def apply[FApp, GApp](v: FApp)(implicit unapplyApply: UnapplyApply[FApp, F, G, GApp, this.type]): GApp = unapplyApply(this)(v)
  }

  /** order-1 morphism AKA function */
  class Morphism1[A, B](val f: A => B) extends Morphism[A, B]

  object Morphism1 {
    /** the unapplyApply for morphism1 */
    implicit def unapplyApply1[A, B, M <: Morphism1[A, B]] = new UnapplyApply[A, A, B, B, M] {
      def apply(m: M)(a: A): B = m.f(a)
    }
  }

  /** order-2 morphism AKA FunctionK */
  trait Morphism2[F[_], G[_]] extends Morphism[F, G] {
    def apply[A](fa: F[A]): G[A]
  }

  object Morphism2 {
    /** the unapplyApply for morphism2 */
    implicit def unapplyApply2[F[_], G[_], A, M <: Morphism2[F, G]] = new UnapplyApply[F[A], F, G, G[A], M] {
      def apply(m: M)(fa: F[A]): G[A] = m.apply(fa)
    }
  }

  // Morphism sample
  val m1 = new Morphism1( (i:Int) => i.toString )
  m1(1)

  val m2 = new Morphism2[Bar, List] {
    def apply[A](fa: Bar[A]): List[A] = List(fa.a)
  }
  m2(Bar(5))

  /** A simple Product (X1, X2) indexed by the type of morphism and respecting the categorical product representation
    * described on Wikipedia https://en.wikipedia.org/wiki/Product_(category_theory):
    * "Let C be a category with some objects X1 and X2. A product of X1 and X2 is an object X (often denoted X1 × X2)
    *  together with a pair of morphisms π1 : X → X1, π2 : X → X2 that satisfy the following universal property:
    *     -for every object Y and pair of morphisms f1 : Y → X1, f2 : Y → X2 there exists a unique morphism f : Y → X
    * "
    * In this model:
    * - A Product[Morphism1] is a simple tuple
    * - A Product[Morphism2] is a order-2 product [A] => (F[A], G[A])
    * - etc...
    */
  trait Prod[M[a<:AnyKind, b<:AnyKind] <: Morphism[a , b]] {
    // synthetic poly-kinded representation of own type for be used in π1 & π2 morphisms
    type Self <: AnyKind

    // the types in the product
    type X1 <: AnyKind
    type X2 <: AnyKind

    // the projection morphism
    def π1 : M[Self, X1]
    def π2 : M[Self, X2]
  }

  /** the Product[Morphism1] AKA a simple tuple */
  case class Prod1[A, B](a: A, b: B) extends Prod[Morphism1] {
    type Self = Prod1[A, B]
    type X1 = A
    type X2 = B

    val π1 = new Morphism1[Self, A](_ => a)
    val π2 = new Morphism1[Self, B](_ => b)

    val prj1: A = π1(this)
    val prj2: B = π2(this)
  }

  /** The Builder represents the unique morphism from categorical representation:
    * "for every object Y and pair of morphisms f1 : Y → X1, f2 : Y → X2 there exists a unique morphism f : Y → X"
    */
  class ProdBuilder1[Y, A, B](f1: Morphism1[Y, A], f2: Morphism1[Y, B]) extends Morphism1[Y, Prod1[A, B]](
    (y:Y) => new Prod1(f1(y), f2(y))
  )

  /** the Product[Morphism2] AKA a [A] => (F[A], G[A]) */
  case class Prod2[F[_], G[_], A](fa: F[A], ga: G[A]) extends Prod[Morphism2] {
    type Self[A] = Prod2[F, G, A]
    type X1[A] = F[A]
    type X2[A] = G[A]
    val π1 = new Morphism2[Self, F] {
      def apply[A](x: Self[A]): F[A] = x.fa
    }
    val π2 = new Morphism2[Self, G] {
      def apply[A](x: Self[A]): G[A] = x.ga
    }

    val prj1: F[A] = π1(this)
    val prj2: G[A] = π2(this)
  }

  /** The Builder represents the unique morphism from categorical representation:
    * "for every object Y and pair of morphisms f1 : Y → X1, f2 : Y → X2 there exists a unique morphism f : Y → X"
    */
  class ProdBuilder2[Y[_], F[_], G[_]](f1: Morphism2[Y, F], f2: Morphism2[Y, G])
    extends Morphism2[Y, ({ type l[t] = Prod2[F, G, t] })#l] {
    def apply[A](ya: Y[A]): Prod2[F, G, A] = new Prod2(f1(ya), f2(ya))
  }



  // Prod1 Sample (ok it's a bit less simple to write :D)
  // Y is here Unit for convenience but it could be anything
  val prodb1 = new ProdBuilder1(
    new Morphism1((y:Unit) => 5)
  , new Morphism1((y:Unit) => "toto")
  )
  val prod1 = prodb1()
  val i: Int = prod1.π1(prod1)
  val s: String = prod1.π2(prod1)


  // Prod2 Sample (ok it's a bit less simple to write :D)
  // ID is used for having a Y in order-2
  type Id[A] = A
  val prodb2 = new ProdBuilder2(
    new Morphism2[Id, List] {
      def apply[A](x: Id[A]): List[A] = List(x)
    }
  , new Morphism2[Id, Option] {
      def apply[A](x: Id[A]): Option[A] = Option(x)
    }
  )

  val prod2Int = prodb2(5)
  val l1: List[Int] = prod2Int.π1(prod2Int)
  val l11: List[Int] = prod2Int.prj1
  val o1: Option[Int] = prod2Int.π2(prod2Int)
  val o11: Option[Int] = prod2Int.prj2

  val prod2String = prodb2("toto")
  val l2: List[String] = prod2String.π1(prod2String)
  val l21: List[String] = prod2String.prj1
  val o2: Option[String] = prod2String.π2(prod2String)
  val o21: Option[String] = prod2String.prj2


}
