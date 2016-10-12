import scala.language.higherKinds
// import scala.reflect.ClassTag
// import scala.collection.{ GenMap, GenTraversable }

object Test extends App {

  // Scala Conversion of haskell code in http://blog.functorial.com/posts/2012-02-02-Polykinded-Folds.html
  // to study more samples of kind polymorphism in Scala
  // 
  // It provides the implementation of Kind-Polymorphism Category, Functor & Rec for generic folding

  // HASKELL CODE
  // class Category hom where
  //   ident :: hom a a
  //   compose :: hom a b -> hom b c -> hom a c
  trait Category[->[_<:AnyKind, _<:AnyKind]] {
    def ident[A <: AnyKind] : A -> A
    def compose[A <: AnyKind, B <: AnyKind, C <: AnyKind] : (A -> B) => (B -> C) => (A -> C)
  }

  object Category {
    def apply[->[_<:AnyKind, _<:AnyKind]](implicit cat: Category[->]): cat.type = cat
  }

  /** Category for => */
  implicit val Function1Category = new Category[Function1] {
    def ident[A] : A => A = identity
    def compose[A, B, C] = (f: A => B) => (g: B => C) => g.compose(f)
  }

  // Test

  /** Category for ~> Natural Transformation */
  trait ~>[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  implicit val NatCategory = new Category[~>] {
    def ident[A[_]] : A ~> A = new (A ~> A) {
      def apply[T](a: A[T]) = a
    }
    
    def compose[A[_], B[_], C[_]] = (f: A ~> B) => (g: B ~> C) => new (A ~> C) {
      def apply[T](a: A[T]): C[T] = g(f(a))
    }
  }

  // Test
  // assert(implicitly[Category[~>]].ident[List](List(5)) == List(5))
  
  val list2Option = new (List ~> Option) {
    def apply[A](l: List[A]) = l.headOption
  }
  val option2List = new (Option ~> List) {
    def apply[A](l: Option[A]) = l.toList
  }
  // assert(Category[~>].compose(list2Option)(option2List)(List(5, 6)) == List(5))
/*
  // class HFunctor hom f where
  //   hmap :: hom a b -> hom (f a) (f b)  
  /**
    * HFunctor is a higher-kind functor transforming a morphism A -> B into (F[A] -> F[B])
    * where F[A] is A applied to polykinded type F (considered as curried type function)
    * for example: if F is TC[?[_], ?] and we apply A[_] to F, it gives F[A] = TC[A, ?]
    */
  trait HFunctor[F <: AnyKind, ->[_<:AnyKind, _<:AnyKind]] {
    /** In Scala, types are not naturally currified type function so we need to help scalac with this HMorph structure
      * that transform the morphism A -> B into morphism F[A] -> F[B] where F[A] means A applied to F 
      */ 
    def hmap[A <: AnyKind, B <: AnyKind](f: => A -> B)(implicit hmorph: HMorph[F, ->, A, B]): hmorph.Out = hmorph(f)
  }

  object HFunctor {
    def apply[F <: AnyKind, ->[_<:AnyKind, _<:AnyKind]](implicit hf: HFunctor[F, ->]): hf.type = hf
  }


  /** Transform a morphism A -> B into another morphism (F[A] -> F[B]) where F[A] means type A applied to type function F
    *
    * For example, for TC[M[_], A] and Natural Transformation ~>, 
    * it could transform (F ~> G) into (TC[F, ?] ~> TC[G, ?]) applying F and G on TC
    */
  trait HMorph[F <: AnyKind, ->[_<:AnyKind, _<:AnyKind], A <: AnyKind, B <: AnyKind] {
    type OutA <: AnyKind
    type OutB <: AnyKind
    type Out = OutA -> OutB
    def apply(f: => A -> B): Out
  }

  // class Rec hom f t where
  //   _in :: hom (f t) t
  //   out :: hom t (f t)
  /** A Generic Recursion structure used in classic (un)folding techniques (like cata/ana)... cf matryoschka */
  trait Rec[F <: AnyKind, ->[_<:AnyKind, _<:AnyKind], T <: AnyKind, FT <: AnyKind] {
    def in: FT -> T
    def out: T -> FT
  }

  // SAMPLE with Tree
  // data FCTree f a = FCLeaf a | FCBranch (f (a, a))
  sealed trait FCTree[F[_], A]
  case class FCLeaf[F[_], A](a: A) extends FCTree[F, A]
  case class FCBranch[F[_], A](fa: F[(A, A)]) extends FCTree[F, A]
  
  // data CTree a = CLeaf a | CBranch (CTree (a, a))
  sealed trait CTree[A]
  case class CLeaf[A](a: A) extends CTree[A]
  case class CBranch[A](fa: CTree[(A, A)]) extends CTree[A]

  object FCTree {
    /** the Morphism from A ~> Bto FCTree[A, ?] ~> FCTree[B, ?] */
    implicit def hmorph[A[_], B[_]] = new HMorph[FCTree, ~>, A, B] {
      type OutA[t] = FCTree[A, t]
      type OutB[t] = FCTree[B, t]
      def apply(f: => A ~> B): (OutA ~> OutB) = new (OutA ~> OutB) {
        def apply[T](fa: OutA[T]) = fa match {
          case FCLeaf(a) => FCLeaf(a)
          case FCBranch(fa) => FCBranch(f(fa))
        }
      }
    }

    /** Functor is trivial as HMorph does the job */
    implicit val hfunctor = new HFunctor[FCTree, ~>] {}

    /** Rec */
    implicit val rec = new Rec[FCTree, ~>, CTree, ({ type l[t] = FCTree[CTree, t] })#l] {
      type FT[t] = FCTree[CTree, t]
      val in = new (FT ~> CTree) {
        def apply[A](ft: FT[A]): CTree[A] = ft match {
          case FCLeaf(a)    => CLeaf(a)
          case FCBranch(aa) => CBranch(aa)
        }
      }
      val out = new (CTree ~> FT) {
        def apply[A](ct: CTree[A]): FT[A] = ct match {
          case CLeaf(a)    => FCLeaf(a)
          case CBranch(aa) => FCBranch(aa)
        }
      }
    }
  }

  val mapper = HFunctor[FCTree, ~>].hmap(list2Option)
  assert(mapper(FCLeaf[List, Int](5)) == FCLeaf[Option, Int](5))
  assert(mapper(FCBranch(List((5, 6)))) == FCBranch(Some((5, 6))))

  /** Generic fold function (like cata) */
  // fold :: (Category hom, HFunctor hom f, Rec hom f rec) => hom (f t) t -> hom rec t
  // fold phi = compose out (compose (hmap (fold phi)) phi)
  def fold[F <: AnyKind, ->[_<:AnyKind, _<:AnyKind], T <: AnyKind, R <: AnyKind, FT <: AnyKind, FR <: AnyKind](phi: FT -> T)(
    implicit  category: Category[->]
            , hfunctor: HFunctor[F, ->]
            , rec: Rec[F, ->, R, FR]
            , hmorph: HMorph[F, ->, FT, T]
  ): R -> T = category.compose(rec.out)(category.compose(hfunctor.hmap(fold(phi)))(phi))

  /** Generic unfold function (like cata) */
  // unfold :: (Category hom, HFunctor hom f, Rec hom f rec) => hom t (f t) -> hom t rec
  // unfold phi = compose phi (compose (hmap (unfold phi)) _in)
  def unfold[F <: AnyKind, ->[_<:AnyKind, _<:AnyKind], T <: AnyKind, R <: AnyKind, FT <: AnyKind, FR <: AnyKind](phi: T -> FT)(
    implicit  category: Category[->]
            , hfunctor: HFunctor[F, ->]
            , rec: Rec[F, ->, R, FR]
            , hmorph: HMorph[F, ->, FT, T]
  ): T -> R = category.compose(phi)(category.compose(hfunctor.hmap(unfold(phi)))(rec.in))


  // cdepth :: CTree a -> Int
  // cdepth c = let (K d) = nu (fold (Nat phi)) c in d where
  //   phi :: FCTree (K Int) a -> K Int a
  //   phi (FCLeaf a) = K 1
  //   phi (FCBranch (K n)) = K (n + 1)

  case class K[A, B](a: A)
  def cdepth[A](c: CTree[A]): Int = {
    type KInt[A] = K[Int, A]
    type FK[A] = FCTree[KInt, A]
    type FC[A] = FCTree[CTree, A]

    val phi = new (FK ~> KInt) {
      def apply[A](f: FCTree[KInt, A]): KInt[A] = f match {
        case FCLeaf(a) => K(1)
        case FCBranch(K(n)) => K(n + 1)
      }
    }

    fold[FCTree, ~>, KInt, CTree, FK, FC](phi).apply(c).a
  }

  assert(cdepth(CBranch(CBranch(CLeaf((5, 6),(7, 8))))) == 3)
*/

  /** Monoid forms a Category
    * FYI, for convenience of the sample, that Monoid representation can't represent Monads
    * as "mult" can't be used to represent Functor Composition M[M[?]]
    */
  trait Monoid[M <: AnyKind] {
    type ->[_<:AnyKind, _<:AnyKind]
    type I <: AnyKind

    def unit: ->[I, M]
    def mult(a: I -> M, b: I -> M): I -> M
  }

  object Monoid {
    type Aux[M <: AnyKind, M0[_<:AnyKind, _<:AnyKind], I0 <: AnyKind] = Monoid[M] { type ->[a <: AnyKind, b <: AnyKind] = M0[a, b]; type I = I0 }

    def apply[M <: AnyKind](implicit monoid: Monoid[M]): monoid.type = monoid
  }

  /** Sample with Int & integer multiplication */
  implicit val MonoidIntMul: Monoid.Aux[Int, Function1, Unit] = new Monoid[Int] {
    type ->[a, b] = Function1[a, b]
    type I = Unit

    val unit: Unit => Int = _ => 1
    def mult(a: Unit => Int, b: Unit => Int): Unit => Int = _ => a(()) * b(())
  }

  assert(Monoid[Int].unit(()) == 1)
  assert(Monoid[Int].mult(_ => 5, _ => 10)(()) == 50)

  type Id[A] = A
  implicit val MonoidList: Monoid.Aux[List, ~>, Id] = new Monoid[List] {
    type ->[a[_], b[_]] = ~>[a, b]
    type I[a] = Id[a]

    val unit: Id ~> List = new (Id ~> List) {
      def apply[A](id: Id[A]): List[A] = List()
    }
    def mult(a: Id ~> List, b: Id ~> List): Id ~> List = new (Id ~> List) {
      def apply[A](id: Id[A]): List[A] = a(id) ++ b(id)
    }
  }

  assert(Monoid[List].unit(0:Id[Int]) == List[Int]())
  val l = new (Id ~> List) {
    def apply[A](id: Id[A]): List[A] = List(id)
  }
  assert(Monoid[List].mult(l, l)(5: Id[Int]) == List(5, 5))

  def MonoidCategory[M <: AnyKind](implicit monoid: Monoid[M]) = {
    import monoid._
    new Category[({ type l[A <: AnyKind, B <: AnyKind] = I -> M })#l] {
      def ident[A <: AnyKind]: I -> M = monoid.unit
      def compose[A <: AnyKind, B <: AnyKind, C <: AnyKind] =
        (f: I -> M) => (g: I -> M) => monoid.mult(f, g)
    }
  }

  assert(MonoidCategory[List].ident(0) == List())
  assert(MonoidCategory[List].compose(l)(l)(5: Id[Int]) == List(5, 5))


/*
  // The rest of the code is far better in Haskell and in Scala we wouldn't write it as in the article
  // showing once again that it's useless to try to write Haskell in Scala as both have different pros/cons
  // It's better to use the right techniques in the context of its language and not try to imitate
  // Yet it's clear that naturally curried types in Haskell makes syntax much cleaner...

  // data Choice = Fst | Snd
  sealed trait Choice
  case object Fst extends Choice
  case object Snd extends Choice

  trait Applied[F <: AnyKind, A <: AnyKind] {
    type Out <: AnyKind
  }

  // newtype PHom h1 h2 p1 p2 = PHom { runPHom :: forall r. (h1 (p1 Fst) (p2 Fst) -> h2 (p1 Snd) (p2 Snd) -> r) -> r }
  trait PHom[H1[_ <: AnyKind, _ <: AnyKind], H2[_ <: AnyKind, _ <: AnyKind], P1<:AnyKind, P2<:AnyKind] {
    def apply[R](h: H1[Applied[P1, Fst.type], Applied[P2, Fst.type]] => H2[Applied[P1, Snd.type], Applied[P2, Snd.type]] => R): R
  }

  // instance (Category h1, Category h2) => Category (PHom h1 h2) where
  //  ident = mkPHom ident ident
  //  compose p1 p2 = mkPHom (compose (fstPHom p1) (fstPHom p2)) (compose (sndPHom p1) (sndPHom p2))
  object PHom {
    implicit def category[H1[_ <: AnyKind, _ <: AnyKind], H2[_ <: AnyKind, _ <: AnyKind]](
      implicit  cat1: Category[H1], cat2: Category[H2]
    ) = new Category[({ type l[p1<:AnyKind, p2<:AnyKind] = PHom[H1, H2, p1, p2] })#l] {
      def ident[A <: AnyKind] : PHom[H1, H2, A, A] = new PHom[H1, H2, A, A] {
        def apply[R](h: H1[Applied[A, Fst.type], Applied[A, Fst.type]] => H2[Applied[A, Snd.type], Applied[A, Snd.type]] => R): R =
          h(cat1.ident[A])(cat2.ident[A])
      }

      def compose[A <: AnyKind, B <: AnyKind, C <: AnyKind] : PHom[H1, H2, A, B] => PHom[H1, H2, B, C] => PHom[H1, H2, A, C] =
        p1 => p2 => {
          val fst1 = p1( (fst: H1[Applied[A, Fst.type], Applied[B, Fst.type]]) => (snd: H2[Applied[A, Snd.type], Applied[B, Snd.type]]) => fst )
          val snd1 = p1( (fst: H1[Applied[A, Fst.type], Applied[B, Fst.type]]) => (snd: H2[Applied[A, Snd.type], Applied[B, Snd.type]]) => snd )
          val fst2 = p2( (fst: H1[Applied[B, Fst.type], Applied[C, Fst.type]]) => (snd: H2[Applied[B, Snd.type], Applied[C, Snd.type]]) => fst )
          val snd2 = p2( (fst: H1[Applied[B, Fst.type], Applied[C, Fst.type]]) => (snd: H2[Applied[B, Snd.type], Applied[C, Snd.type]]) => snd )
          
          new PHom[H1, H2, A, C] {
            def apply[R](h: H1[Applied[A, Fst.type], Applied[C, Fst.type]] => H2[Applied[A, Snd.type], Applied[C, Snd.type]] => R): R =
              h(cat1.compose(fst1)(fst2))(cat2.compose(snd1)(snd2))
          }
        }
    }
  }


  // data FAlt :: * -> (Choice -> *) -> Choice -> * where
  //   FZero :: FAlt a p Fst
  //   FSucc1 :: a -> (p Snd) -> FAlt a p Fst
  //   FSucc2 :: a -> (p Fst) -> FAlt a p Snd
  sealed trait FAlt[A, P[C <: Choice], C <: Choice]
  case class FZero[A, P[C <: Choice]]() extends FAlt[A, P, Fst.type]
  case class FSucc1[A, P[C <: Choice]](h: A, t: P[Snd.type]) extends FAlt[A, P, Fst.type]
  case class FSucc2[A, P[C <: Choice]](h: A, t: P[Fst.type]) extends FAlt[A, P, Snd.type]

  // data Alt :: * -> Choice -> * where
  //   Zero :: Alt a Fst
  //   Succ1 :: a -> Alt a Snd -> Alt a Fst
  //   Succ2 :: a -> Alt a Fst -> Alt a Snd
  sealed trait Alt[A, C <: Choice]
  case class Zero[A]() extends Alt[A, Fst.type]
  case class Succ1[A](h: A, t: Alt[A, Snd.type]) extends Alt[A, Fst.type]
  case class Succ2[A](h: A, t: Alt[A, Fst.type]) extends Alt[A, Snd.type]


  // instance Rec (PHom (->) (->)) (FAlt a) (Alt a) where
  //   _in = mkPHom f g where
  //     f FZero = Zero
  //     f (FSucc1 a b) = Succ1 a b
  //     g (FSucc2 a b) = Succ2 a b
  //   out = mkPHom f g where
  //     f Zero = FZero
  //     f (Succ1 a b) = FSucc1 a b
  //     g (Succ2 a b) = FSucc2 a b

  // instance HFunctor (PHom (->) (->)) (FAlt a) where
  //   hmap p = mkPHom hf hg where
  //     hf FZero = FZero
  //     hf (FSucc1 a x) = FSucc1 a (sndPHom p x)
  //     hg (FSucc2 a x) = FSucc2 a (fstPHom p x)

  // object FAlt {
  //   implicit def hmorph[A] = new HMorph[FAlt, ({ type l[p1<:AnyKind, p2<:AnyKind] = PHom[Function1, Function1, p1, p2] })#l, A, B] {
  //     type OutA[P[C <: Choice], C <: Choice] = FAlt[A, P, C]
  //     type OutB[P[C <: Choice], C <: Choice] = FAlt[A, P, t]
  //     def apply(f: => PHom[Function1, Function1, A, B]): PHom[->, ->, FAlt[A, P, C], FAlt[A, P, C]] = {
  //       def hf(fa: FAlt[A, P1, C]) = fa match {
  //         case FZero() => FZero[A, P2, C]
  //         case FSucc1(a, t) => FSucc1(a, f((fst: Applied[P1, Fst.type] => Applied[P2, Fst.type]]) => (snd: H2[Applied[A, Snd.type], Applied[B, Snd.type]]) => fst )))
  //       }
  //     }
  //   }

  //   implicit def hfunctor[A] = new HFunctor[({ type l[P[C <: Choice], C <: Choice] = FAlt[A, P, C] })#l, ({ type l[p1<:AnyKind, p2<:AnyKind] = PHom[->, ->, p1, p2] })#l] {}
  // }
*/
}



