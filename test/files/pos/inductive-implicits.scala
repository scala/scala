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

  @annotation.inductive
  trait Selector[L <: HList, U] {
    def apply(l: L): U
  }

  object Selector {
    def apply[L <: HList, U](implicit selector: Selector[L, U]): Selector[L, U] = selector

    implicit def inHead[H, T <: HList]: Selector[H :: T, H] =
      new Selector[H :: T, H] {
        def apply(l : H :: T) = l.head
      }

    implicit def inTail[H, T <: HList, U]
      (implicit st : Selector[T, U]): Selector[H :: T, U] =
        new Selector[H :: T, U] {
          def apply(l : H :: T) = st(l.tail)
        }
  }

  trait Reverse[L <: HList] {
    type Out <: HList
    def apply(l: L): Out
  }

  object Reverse {
    def apply[L <: HList](implicit reverse: Reverse[L]): Aux[L, reverse.Out] = reverse

    type Aux[L <: HList, Out0 <: HList] = Reverse[L] { type Out = Out0 }

    implicit def reverse[L <: HList, Out0 <: HList](implicit reverse : Reverse0[HNil, L, Out0]): Aux[L, Out0] =
      new Reverse[L] {
        type Out = Out0
        def apply(l : L) : Out = reverse(HNil, l)
      }

    @annotation.inductive
    trait Reverse0[Acc <: HList, L <: HList, Out <: HList] {
      def apply(acc : Acc, l : L) : Out
    }

    object Reverse0 {
      implicit def hnilReverse[Out <: HList]: Reverse0[Out, HNil, Out] =
        new Reverse0[Out, HNil, Out] {
          def apply(acc : Out, l : HNil) : Out = acc
        }

      implicit def hlistReverse[Acc <: HList, InH, InT <: HList, Out <: HList]
        (implicit rt : Reverse0[InH :: Acc, InT, Out]): Reverse0[Acc, InH :: InT, Out] =
          new Reverse0[Acc, InH :: InT, Out] {
            def apply(acc : Acc, l : InH :: InT) : Out = rt(l.head :: acc, l.tail)
          }
    }
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

  trait Nat {
    type N <: Nat
  }

  case class Succ[P <: Nat]() extends Nat {
    type N = Succ[P]
  }

  class _0 extends Nat with Serializable {
    type N = _0
  }

  object nats {
    type _1 = Succ[_0]
    val _1: _1 = new _1

    type _2 = Succ[_1]
    val _2: _2 = new _2

    type _3 = Succ[_2]
    val _3: _3 = new _3

    type _4 = Succ[_3]
    val _4: _4 = new _4

    type _5 = Succ[_4]
    val _5: _5 = new _5

    type _6 = Succ[_5]
    val _6: _6 = new _6

    type _7 = Succ[_6]
    val _7: _7 = new _7

    type _8 = Succ[_7]
    val _8: _8 = new _8

    type _9 = Succ[_8]
    val _9: _9 = new _9

    type _10 = Succ[_9]
    val _10: _10 = new _10

    type _11 = Succ[_10]
    val _11: _11 = new _11

    type _12 = Succ[_11]
    val _12: _12 = new _12

    type _13 = Succ[_12]
    val _13: _13 = new _13

    type _14 = Succ[_13]
    val _14: _14 = new _14

    type _15 = Succ[_14]
    val _15: _15 = new _15

    type _16 = Succ[_15]
    val _16: _16 = new _16

    type _17 = Succ[_16]
    val _17: _17 = new _17

    type _18 = Succ[_17]
    val _18: _18 = new _18

    type _19 = Succ[_18]
    val _19: _19 = new _19

    type _20 = Succ[_19]
    val _20: _20 = new _20

    type _21 = Succ[_20]
    val _21: _21 = new _21

    type _22 = Succ[_21]
    val _22: _22 = new _22
  }

  @annotation.inductive
  trait LT[A <: Nat, B <: Nat] extends Serializable

  object LT extends LT0 {
    def apply[A <: Nat, B <: Nat](implicit lt: A < B): LT[A, B] = lt

    implicit def lt1[B <: Nat] = new <[_0, Succ[B]] {}
    implicit def lt2[A <: Nat, B <: Nat](implicit lt : A < B) = new <[Succ[A], Succ[B]] {}
  }

  trait LT0 {
    type <[A <: Nat, B <: Nat] = LT[A, B]

    implicit def lt3[A <: Nat] = new <[A, Succ[A]] {}
  }

  @annotation.inductive
  trait Sum[A <: Nat, B <: Nat] extends Serializable { type Out <: Nat }

  object Sum {
    def apply[A <: Nat, B <: Nat](implicit sum: Sum[A, B]): Aux[A, B, sum.Out] = sum

    type Aux[A <: Nat, B <: Nat, C <: Nat] = Sum[A, B] { type Out = C }

    implicit def sum1[B <: Nat]: Aux[_0, B, B] = new Sum[_0, B] { type Out = B }
    implicit def sum2[A <: Nat, B <: Nat]
      (implicit sum : Sum[A, Succ[B]]): Aux[Succ[A], B, sum.Out] = new Sum[Succ[A], B] { type Out = sum.Out }
  }

  @annotation.inductive
  trait Diff[A <: Nat, B <: Nat] extends Serializable { type Out <: Nat }

  object Diff {
    def apply[A <: Nat, B <: Nat](implicit diff: Diff[A, B]): Aux[A, B, diff.Out] = diff

    type Aux[A <: Nat, B <: Nat, C <: Nat] = Diff[A, B] { type Out = C }

    implicit def diff1[A <: Nat]: Aux[A, _0, A] = new Diff[A, _0] { type Out = A }
    implicit def diff2[A <: Nat, B <: Nat]
      (implicit diff : Diff[A, B]): Aux[Succ[A], Succ[B], diff.Out] = new Diff[Succ[A], Succ[B]] { type Out = diff.Out }
  }

  @annotation.inductive
  trait Prod[A <: Nat, B <: Nat] extends Serializable { type Out <: Nat }

  object Prod {
    def apply[A <: Nat, B <: Nat](implicit prod: Prod[A, B]): Aux[A, B, prod.Out] = prod

    type Aux[A <: Nat, B <: Nat, C <: Nat] = Prod[A, B] { type Out = C }

    implicit def prod1[B <: Nat]: Aux[_0, B, _0] = new Prod[_0, B] { type Out = _0 }
    implicit def prod2[A <: Nat, B <: Nat, C <: Nat]
      (implicit prod: Prod.Aux[A, B, C], sum: Sum[B, C]): Aux[Succ[A], B, sum.Out] = new Prod[Succ[A], B] { type Out = sum.Out }
  }

  //@annotation.inductive
  trait Div[A <: Nat, B <: Nat] extends Serializable { type Out <: Nat }

  object Div {
    def apply[A <: Nat, B <: Nat](implicit div: Div[A, B]): Aux[A, B, div.Out] = div

    import LT._

    type Aux[A <: Nat, B <: Nat, C <: Nat] = Div[A, B] { type Out = C }

    implicit def div1[A <: Nat]: Aux[_0, A, _0] = new Div[_0, A] { type Out = _0 }

    implicit def div2[A <: Nat, B <: Nat](implicit lt: A < B): Aux[A, B, _0] =
      new Div[A, B] { type Out = _0 }

    implicit def div3[A <: Nat, B <: Nat, C <: Nat, D <: Nat]
      (implicit diff: Diff.Aux[Succ[A], B, C], div: Div.Aux[C, B, D]): Aux[Succ[A], B, Succ[D]] =
        new Div[Succ[A], B] { type Out = Succ[D] }
  }

  @annotation.inductive
  trait Pow[N <: Nat, X <: Nat] extends Serializable { type Out <: Nat }

  object Pow {
    def apply[A <: Nat, B <: Nat](implicit pow: Pow[A, B]): Aux[A, B, pow.Out] = pow

    import nats._1

    type Aux[N <: Nat, X <: Nat, Z <: Nat] = Pow[N, X] { type Out = Z }

    implicit def pow1[A <: Nat]: Aux[Succ[A], _0, _0] = new Pow[Succ[A], _0] { type Out = _0 }
    implicit def pow2[A <: Nat]: Aux[_0, Succ[A], _1] = new Pow[_0, Succ[A]] { type Out = _1 }
    implicit def pow3[N <: Nat, X <: Nat, Z <: Nat, Y <: Nat]
      (implicit ev : Pow.Aux[N, X, Z], ev2 : Prod.Aux[Z, X, Y]): Aux[Succ[N], X, Y] = new Pow[Succ[N], X] { type Out = Y }
  }

  trait Foo[T, P]
  object Foo {
    implicit def foo[T]: Foo[T, T] = ???
  }

  @annotation.inductive
  trait Bar[T]
  object Bar {
    implicit val barBase: Bar[Unit] = ???
    implicit def barStep[T, U, P](implicit ft: Foo[T, P], bu: Bar[U]): Bar[(T, U)] = ???
  }

  @annotation.inductive
  trait Baz[T] {
    type U
  }
  object Baz extends Baz0 {
    def apply[T](implicit bt: Baz[T]): Aux[T, bt.U] = bt

    implicit def bazStep[T, U](implicit btu: Aux[T, U], qu: Quux[U]): Aux[Option[T], T] = ???
  }
  trait Baz0 {
    type Aux[T, U0] = Baz[T] { type U = U0 }
    implicit def bazBase[T]: Aux[T, T] = ???
  }

  trait Quux[T]
  object Quux {
    implicit def quux[T]: Quux[T] = ???
  }

  trait Wobble[T] {
    type U
  }
  object Wobble {
    type Aux[T, U0] = Wobble[T] { type U = U0 }
    implicit def wobble[T]: Aux[T, T] = ???
  }

  @annotation.inductive
  trait Wibble[T, U]
  object Wibble extends Wibble0 {
    def apply[T, U](implicit wtu: Wibble[T, U]): Wibble[T, U] = wtu

    implicit def wibbleStep[T, U, V](implicit wtu: Wobble.Aux[T, U], wu: Wibble[T, U]): Wibble[Option[T], V] = ???
  }
  trait Wibble0 {
    implicit def wibbleBase[T]: Wibble[T, T] = ???
  }

  @annotation.inductive
  trait Wiggle[T] {
    type I
  }
  object Wiggle extends Wiggle0 {
    implicit def wiggleStep[T, U](implicit btu: Aux[T, U]): Aux[Option[T], U] = ???

  }
  trait Wiggle0 {
    type Aux[T, I0] = Wiggle[T] { type I = I0 }
    implicit def wiggleBase[T]: Aux[T, T] = ???
  }

  trait Waggle[T] {
    type A
  }
  object Waggle {
    type Aux[T, A0] = Waggle[T] { type A = A0 }
    def apply[T](implicit wa: Waggle[T]): Aux[T, wa.A] = wa
    implicit def waggle[T, U](implicit wi: Wiggle.Aux[T, U]): Waggle.Aux[T, U] = ???
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

  trait Lub[-A, -B, Out] extends Serializable {
    def left(a : A): Out
    def right(b : B): Out
  }

  object Lub {
    implicit def lub[T] = new Lub[T, T, T] {
      def left(a : T): T = a
      def right(b : T): T = b
    }
  }

  @annotation.inductive
  trait ToTraversable[L <: HList, M[_]] {
    type Lub
    def builder(): collection.mutable.Builder[Lub, M[Lub]]
    def append[LLub](l: L, b: collection.mutable.Builder[LLub, M[LLub]], f: Lub => LLub): Unit

    type Out = M[Lub]
    def apply(l: L): Out = {
      val b = builder()
      append(l, b, identity)
      b.result()
    }
  }

  object ToTraversable {
    def apply[L <: HList, M[_]]
      (implicit toTraversable: ToTraversable[L, M]): Aux[L, M, toTraversable.Lub] = toTraversable

    type Aux[L <: HList, M[_], Lub0] = ToTraversable[L, M] { type Lub = Lub0 }

    implicit def hnilToTraversable[L <: HNil, M[_], T]
      (implicit cbf : collection.generic.CanBuildFrom[M[T], T, M[T]]) : Aux[L, M, T] =
        new ToTraversable[L, M] {
          type Lub = T
          def builder() = cbf()
          def append[LLub](l : L, b : collection.mutable.Builder[LLub, M[LLub]], f : Lub => LLub) = {}
        }

    implicit def hnilToTraversableNothing[L <: HNil, M[_]]
      (implicit cbf : collection.generic.CanBuildFrom[M[Nothing], Nothing, M[Nothing]]) : Aux[L, M, Nothing] =
        hnilToTraversable[L, M, Nothing]

    implicit def hsingleToTraversable[T, M[_], Lub0]
      (implicit ev : T <:< Lub0, cbf : collection.generic.CanBuildFrom[Nothing, Lub0, M[Lub0]]) : Aux[T :: HNil, M, Lub0] =
        new ToTraversable[T :: HNil, M] {
          type Lub = Lub0
          def builder() = cbf()
          def append[LLub](l : T :: HNil, b : collection.mutable.Builder[LLub, M[LLub]], f : Lub0 => LLub) = {
            b += f(l.head)
          }
        }

    implicit def hlistToTraversable[H1, H2, T <: HList, LubT, Lub0, M[_]]
      (implicit
       tttvs  : Aux[H2 :: T, M, LubT],
       u      : Lub[H1, LubT, Lub0],
       cbf    : collection.generic.CanBuildFrom[M[Lub0], Lub0, M[Lub0]]) : Aux[H1 :: H2 :: T, M, Lub0] =
        new ToTraversable[H1 :: H2 :: T, M] {
          type Lub = Lub0
          def builder() = cbf()
          def append[LLub](l : H1 :: H2 :: T, b : collection.mutable.Builder[LLub, M[LLub]], f : Lub0 => LLub): Unit = {
            b += f(u.left(l.head)); tttvs.append[LLub](l.tail, b, f compose u.right)
          }
        }
  }
}

import shapeless._, nats._

object Test extends App {
  type L = Int :: Boolean :: HNil
  val sel = Selector[L, Boolean]
  val rev = Reverse[L]
  rev: Reverse[L] { type Out = Boolean :: Int :: HNil }
  object Fn {
    implicit val caseInt: Case1.Aux[Fn.type, Int, Int] = new Case1[Fn.type, Int] { type Result = Int ; def apply(t: Int): Int = t }
    implicit val caseBoolean: Case1.Aux[Fn.type, Boolean, Boolean] = new Case1[Fn.type, Boolean] { type Result = Boolean ; def apply(t: Boolean): Boolean = t }
  }

  val map = Mapper[Fn.type, L]
  map: Mapper.Aux[Fn.type, L, L]

  implicitly[Partition[L, Boolean]]

  implicitly[Sum.Aux[_2, _3, _5]]
  val sum23 = Sum[_2, _3]
  sum23: Sum.Aux[_2, _3, _5]

  implicitly[Prod.Aux[_0, _1, _0]]
  implicitly[Prod.Aux[_1, _0, _0]]
  implicitly[Prod.Aux[_1, _1, _1]]
  implicitly[Prod.Aux[_2, _1, _2]]
  implicitly[Prod.Aux[_2, _3, _6]]
  implicitly[Prod.Aux[_4, _5, _20]]

  val prod212 = Prod[_2, _1]
  prod212: Prod.Aux[_2, _1, _2]

  val prod236 = Prod[_2, _3]
  prod236: Prod.Aux[_2, _3, _6]

  implicitly[Bar[(Unit, Unit)]]

  val res2 = Wibble[Option[Int], String]
  res2: Wibble[Option[Int], String]
  val res3 = Wibble[Option[Option[Int]], String]
  res3: Wibble[Option[Option[Int]], String]

  val res0 = Baz[Option[Int]]
  res0: Baz.Aux[Option[Int], Int]
  val res1 = Baz[Option[Option[Int]]]
  res1: Baz.Aux[Option[Option[Int]], Option[Int]]

  trait Check[N <: Nat]
  def check(expected: Nat)(actually : => Check[expected.N]) {}

  def prod(a: Nat, b: Nat)(implicit prod : Prod[a.N, b.N]) = new Check[prod.Out] {}
  val p1 = prod(_2, _3)
  check(_6)(p1)
  val p2 = prod(_4, _5)
  check(_20)(p2)

  implicitly[Diff.Aux[_5, _1, _4]]

  def diff(a: Nat, b: Nat)(implicit diff : Diff[a.N, b.N]) = new Check[diff.Out] {}
  val diff1 = diff(_5, _1)
  check(_4)(diff1)

  implicitly[LT[_3, _5]]

  implicitly[Div.Aux[_7, _2, _3]]
  implicitly[Div.Aux[_7, _2, _3]]
  implicitly[Div.Aux[_22, _11, _2]]
  implicitly[Div.Aux[_15, _3, _5]]

  def div(a: Nat, b: Nat)(implicit div : Div[a.N, b.N]) = new Check[div.Out] {}
  val d1 = div(_7, _2)
  check(_3)(d1)
  val d2 = div(_22, _11)
  check(_2)(d2)
  val d3 = div(_15, _3)
  check(_5)(d3)

  implicitly[Pow.Aux[_0, _8, _1]]
  implicitly[Pow.Aux[_9, _0, _0]]
  implicitly[Pow.Aux[_3, _2, _8]]

  def pow(a: Nat, b: Nat)(implicit pow : Pow[a.N, b.N]) = new Check[pow.Out] {}
  val e1 = pow(_3, _1)
  check(_1)(e1)
  val e2 = pow(_2, _3)
  check(_9)(e2)
  val e3 = pow(_2, _4)
  check(_16)(e3)

  val res4 = Waggle[Option[Option[Int]]]
  res4: Waggle.Aux[Option[Option[Int]], Int]

  val res5 = Div[_1, _1]
  res5: Div.Aux[_1, _1, _1]

  trait Poly { outer =>
    trait Case[P, A] {
      type R
      val f: A => R
    }
    object Case {
      type Aux[A, R0] = Case[outer.type, A] { type R = R0 }
    }

    def at[A] = new {
      def apply[R0](f0: A => R0): Case.Aux[A, R0] = new Case[outer.type, A] { type R = R0 ; val f = f0 }
    }
  }

  object Poly {
    implicit def inst1[A](p: Poly)(implicit cse: p.Case[p.type, A]): (A) => cse.R = (a: A) => cse.f(a)
  }

  object Rec extends Poly {
    implicit def default[T] = at[T](_ => 13)
    implicit def opt[T](implicit rt: Case.Aux[T, Int]) = at[Option[T]](ot => ot.map(Rec).getOrElse(0))
  }

  trait Fruit
  class Apple extends Fruit
  class Pear extends Fruit

  val ttnil = ToTraversable[HNil, List]
  ttnil: ToTraversable.Aux[HNil, List, Nothing]

  implicitly[ToTraversable.Aux[HNil, List, Int]]

  val tti = ToTraversable[Int :: HNil, List]
  tti: ToTraversable.Aux[Int:: HNil, List, Int]

  val ttap = ToTraversable[Apple :: Pear :: HNil, List]
  //ttap: ToTraversable.Aux[Apple :: Pear :: HNil, List, Fruit]

  trait C[P, T]
  object P1 {
    implicit val p1: C[P1.type, Int] = ???
  }

  object P2 {
    implicit val p2a: C[P2.type, Unit] = ???
    implicit def p2b[T](implicit p1: C[P1.type, T]): C[P2.type, Option[T]] = ???
  }

  implicitly[C[P2.type, Option[Int]]]
}
