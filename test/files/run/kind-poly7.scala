import scala.language.higherKinds

object Test extends App {
  /** An poly-kinded representation of morphism
    * UnapplyApply is used to recollect kinds and their order-1  derived types
    */
  trait UnapplyApply[FA, F <: AnyKind, G <: AnyKind, GA, M] {
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

  /** The PolyKinded representation of Functor */
  trait Functor[F <: AnyKind] extends {
    /** the functor domain morphism (A => B for simple Functor) */
    type M <: AnyKind
    /** the functor co-domain morphism (F[A] => F[B] for simple Functor) */
    type MK <: AnyKind

    /** the Functor Morphism of Morphism (A => B) => (F[A] => F[B]) */
    def mapper[MA, MKA](ma: MA)(implicit unap: UnapplyApply[MA, M, MK, MKA, this.type]): MKA = unap.apply(this)(ma)
  }

  object Functor {
    type Aux[F <: AnyKind, M0 <: AnyKind, MK0 <: AnyKind] = Functor[F] { type M = M0; type MK = MK0 }
    
    def apply[F <: AnyKind](implicit functor: Functor[F]): functor.type = functor
  }

  type Morphism1F[F[_], A, B] = Morphism1[F[A], F[B]]

  /** Just a helper to make it simpler to write Functor of F[_] (like Functor[List]) */
  abstract class Functor1[F[_]] extends Functor[F] {
    type M[A, B] = Morphism1[A, B]
    type MK[A, B] = Morphism1F[F, A, B]

    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor1 {
    def apply[F[_]](implicit functor: Functor1[F]): functor.type = functor

    implicit def unap1[F[_], A, B, F1 <: Functor1[F]] =
      new UnapplyApply[Morphism1[A, B], Morphism1, ({ type l[a, b] = Morphism1F[F, a, b] })#l, Morphism1F[F, A, B], F1] {
        def apply(m: F1)(ma: Morphism1[A, B]): Morphism1F[F, A, B] = new Morphism1[F[A], F[B]](
          (fa: F[A]) => m.map(fa)(a => ma.f(a))
        )
      }   
  }

  /** Just a helper to make it simpler to write Functor of TC[_[_], _] (like Functor[TC[_[_], _]]) */
  abstract class Functor21[TC[_[_], _]] extends Functor[TC] {
    type M[F[_], G[_]] = Morphism2[F, G]
    type MK[F[_], G[_]] = Morphism2[({ type l[t] = TC[F, t] })#l, ({ type l[t] = TC[G, t] })#l]

    def map[F[_], G[_], A](fa: TC[F, A])(f: Morphism2[F, G]): TC[G, A]
  }

  type Morphism2F[TC[_[_], _], F[_], G[_]] = Morphism2[({ type l[t] = TC[F, t] })#l, ({ type l[t] = TC[G, t] })#l]

  object Functor21 {
    implicit def unap21[TC[_[_], _], F[_], G[_], FU <: Functor21[TC]] = {
      new UnapplyApply[Morphism2[F, G], Morphism2, ({ type l[f[_], g[_]] = Morphism2F[TC, f, g] })#l, Morphism2F[TC, F, G], FU] {
        def apply(fu: FU)(ma: Morphism2[F, G]): Morphism2F[TC, F, G] =
          new Morphism2[({ type l[t] = TC[F, t] })#l, ({ type l[t] = TC[G, t] })#l] {
            def apply[A](tca: TC[F, A]) : TC[G, A] = fu.map(tca)(ma)
          }
      }
    }
  }

  /** Just a helper to make it simpler to write Functor of TC[_[_], _] restricted by Functor (like Functor[Free]) */
  abstract class Functor21Functor[TC[_[_], _]] extends Functor[TC] {
    type M[F[_], G[_]] = Morphism2[F, G]
    type MK[F[_], G[_]] = Morphism2[({ type l[t] = TC[F, t] })#l, ({ type l[t] = TC[G, t] })#l]

    def map[F[_]:Functor1, G[_]:Functor1, A](fa: TC[F, A])(f: Morphism2[F, G]): TC[G, A]
  }

  object Functor21Functor {
    implicit def unap21Functor[TC[_[_], _], F[_]: Functor1, G[_]: Functor1, FU <: Functor21Functor[TC]] = {
      new UnapplyApply[Morphism2[F, G], Morphism2, ({ type l[f[_], g[_]] = Morphism2F[TC, f, g] })#l, Morphism2F[TC, F, G], FU] {
        def apply(fu: FU)(ma: Morphism2[F, G]): Morphism2F[TC, F, G] =
          new Morphism2[({ type l[t] = TC[F, t] })#l, ({ type l[t] = TC[G, t] })#l] {
            def apply[A](tca: TC[F, A]) : TC[G, A] = fu.map(tca)(ma)
          }
      }
    }
  }

  // Sample1: List/Option Functor
  implicit val listFunctor = new Functor1[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  val l: List[String] = Functor[List].mapper(new Morphism1((i:Int) => i.toString)).apply(List(5))
  assert(l == List("5"))

  implicit val optionFunctor = new Functor1[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B ]= fa.map(f)
  }

  val l2: Option[String] = Functor[Option].mapper(new Morphism1((i:Int) => i.toString)).apply(Some(5):Option[Int])
  assert(l2 == Some("5"))

  // Sample2: TC[_[_], _] Functor
  case class TC[F[_], A](fa: F[A])
    
  object TC {
    implicit val tcFunctor = new Functor21[TC] {
      def map[F[_], G[_], A](tca: TC[F, A])(m: Morphism2[F, G]): TC[G, A] = new TC[G, A](m.apply(tca.fa))
    }
  }

  case class Foo[A](a: A)
  case class Bar[A](a: A)
  val tc: TC[Bar, Int] = Functor[TC].mapper(new Morphism2[Foo, Bar] {
    def apply[A](foo: Foo[A]): Bar[A] = Bar(foo.a)
  }).apply(TC(Foo(15)))

  assert(tc == TC(Bar(15)))


  // Sample3: Free[_[_], _] Functor
  sealed abstract class Free[F[_]: Functor1, A]
  case class Pure[F[_]: Functor1, A](a: A) extends Free[F, A]
  case class Suspend[F[_]: Functor1, A](a: F[Free[F, A]]) extends Free[F, A]

  object Free {
    implicit val freeFunctor = new Functor21Functor[Free] {
      self =>
      def map[F[_]:Functor1, G[_]:Functor1, A](free: Free[F, A])(m2: Morphism2[F, G]): Free[G, A] = free match {
        case Pure(a)      => Pure[G, A](a)
        case Suspend(fa)  => Suspend(m2.apply(Functor[F].map(fa){ (free: Free[F, A]) => self.map(free)(m2) }))
      }
    }
  }

  val free: Free[Option, Int] = Functor[Free].mapper(new Morphism2[List, Option] {
    def apply[A](l: List[A]): Option[A] = l.headOption
  }).apply(Pure[List, Int](15))

  assert(free == Pure[Option, Int](15))
}

