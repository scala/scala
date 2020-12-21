/*
 * Demo of using by name implicits to resolve (hidden) divergence issues when
 * traversing recursive generic structures.
 *
 * See https://stackoverflow.com/questions/25923974
 */
sealed trait HList
object HList {
  implicit class Syntax[L <: HList](l: L) {
    def ::[U](u: U): U :: L = new ::(u, l)
  }
}

sealed trait HNil extends HList
object HNil extends HNil
case class ::[+H, +T <: HList](head : H, tail : T) extends HList

trait Generic[T] {
  type Repr
  def to(t: T): Repr
  def from(r: Repr): T
}

object Generic {
  type Aux[T, Repr0] = Generic[T] { type Repr = Repr0 }
}

trait DeepHLister[R] {
  type Out
  def apply(r: R): Out
}

object DeepHLister extends DeepHLister0 {
  def apply[R](implicit dh: DeepHLister[R]): Aux[R, dh.Out] = dh

  implicit def consDeepHLister[H, OutH <: HList, T <: HList, OutT <: HList](implicit
    dhh: DeepHLister.Aux[H, OutH],
    dht: DeepHLister.Aux[T, OutT]
  ): Aux[H :: T, OutH :: OutT] = new DeepHLister[H :: T] {
    type Out = OutH :: OutT
    def apply(r: H :: T) = dhh(r.head) :: dht(r.tail)
  }

  implicit object hnilDeepHLister extends DeepHLister[HNil] {
    type Out = HNil
    def apply(r: HNil) = HNil
  }
}

trait DeepHLister0 extends DeepHLister1 {
  implicit def genDeepHLister[T, R <: HList, OutR <: HList](implicit
    gen: Generic.Aux[T, R],
    dhr: => DeepHLister.Aux[R, OutR]
  ): Aux[T, OutR] = new DeepHLister[T] {
    type Out = OutR
    def apply(r: T) = dhr(gen.to(r))
  }
}

trait DeepHLister1 {
  type Aux[R, Out0] = DeepHLister[R] { type Out = Out0 }

  implicit def default[T]: Aux[T, T] = new DeepHLister[T] {
    type Out = T
    def apply(r: T): T = r
  }
}

object Test extends App {
}

object DeepHListerDemo extends App {
  case class A(x: Int, y: String)
  object A {
    type ARepr = Int :: String :: HNil
    implicit val aGen: Generic.Aux[A, ARepr] = new Generic[A] {
      type Repr = ARepr
      def to(t: A): Repr = t.x :: t.y :: HNil
      def from(r: Repr): A = A(r.head, r.tail.head)
    }
  }

  case class B(x: A, y: A)
  object B {
    type BRepr = A :: A :: HNil
    implicit val bGen: Generic.Aux[B, BRepr] = new Generic[B] {
      type Repr = BRepr
      def to(t: B): Repr = t.x :: t.y :: HNil
      def from(r: Repr): B = B(r.head, r.tail.head)
    }
  }

  case class C(b: B, a: A)
  object C {
    type CRepr = B :: A :: HNil
    implicit val cGen: Generic.Aux[C, CRepr] = new Generic[C] {
      type Repr = CRepr
      def to(t: C): Repr = t.b :: t.a :: HNil
      def from(r: Repr): C = C(r.head, r.tail.head)
    }
  }

  case class D(a: A, b: B)
  object D {
    type DRepr = A :: B :: HNil
    implicit val dGen: Generic.Aux[D, DRepr] = new Generic[D] {
      type Repr = DRepr
      def to(t: D): Repr = t.a :: t.b :: HNil
      def from(r: Repr): D = D(r.head, r.tail.head)
    }
  }

  def typed[T](t : => T): Unit = {}

  type ARepr = Int :: String :: HNil
  type BRepr = ARepr :: ARepr :: HNil
  type CRepr = BRepr :: ARepr :: HNil
  type DRepr = ARepr :: BRepr :: HNil

  val adhl = DeepHLister[A :: HNil]
  typed[DeepHLister.Aux[A :: HNil, ARepr :: HNil]](adhl)

  val bdhl = DeepHLister[B :: HNil]
  typed[DeepHLister.Aux[B :: HNil, BRepr :: HNil]](bdhl)

  val cdhl = DeepHLister[C :: HNil]
  typed[DeepHLister.Aux[C :: HNil, CRepr :: HNil]](cdhl)

  val ddhl = DeepHLister[D :: HNil]
  typed[DeepHLister.Aux[D :: HNil, DRepr :: HNil]](ddhl)
}
