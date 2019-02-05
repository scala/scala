// deriving/src/main/scala/by-name-implicit-test.scala.scala
sealed trait AABB
case class AA(a: String) extends AABB
case class BB(a: String) extends AABB
case class DAABB(d: Double, aabb: AABB)
case class IDAABBS(i: Int, daabb: DAABB, s: String)

case class Dog(age: Long)
case class Cat(name: String, friend: Either[Cat, Dog])

// Definitions from Shapeless ---------------------------------------------------------------------

sealed trait HList extends Product with Serializable
final case class ::[+H, +T <: HList](head: H, tail: T) extends HList
sealed trait HNil extends HList
final case object HNil extends HNil

sealed trait Coproduct extends Product with Serializable
sealed trait :+:[+H, +T <: Coproduct] extends Coproduct
final case class Inl[+H, +T <: Coproduct](head: H) extends :+:[H, T]
final case class Inr[+H, +T <: Coproduct](tail: T) extends :+:[H, T]
sealed trait CNil extends Coproduct

trait Generic[T] {
  type Repr
  def to(t: T): Repr
  def from(r: Repr): T
}

// Manual Generic macro expansions ----------------------------------------------------------------

object GenericInstances {
  implicit val genAABB: Generic[AABB] { type Repr = AA :+: BB :+: CNil } =
    new Generic[AABB] {
      type Repr = AA :+: BB :+: CNil
      def to(t: AABB): Repr = t match {
        case x: AA => Inl(x)
        case x: BB => Inr(Inl(x))
      }
      def from(r: Repr): AABB = r match {
        case Inl(x) => x
        case Inr(Inl(x)) => x
        case _ => ???
      }
    }

  implicit val genAA: Generic[AA] { type Repr = String :: HNil } =
    new Generic[AA] {
      type Repr = String :: HNil
      def to(t: AA): Repr = t match { case AA(x) => ::(x, HNil) }
      def from(r: Repr): AA = r match { case ::(x, HNil) => AA(x) }
    }

  implicit val genBB: Generic[BB] { type Repr = String :: HNil } =
    new Generic[BB] {
      type Repr = String :: HNil
      def to(t: BB): Repr = t match { case BB(x) => ::(x, HNil) }
      def from(r: Repr): BB = r match { case ::(x, HNil) => BB(x) }
    }

  implicit val genDAABB: Generic[DAABB] { type Repr = Double :: AABB :: HNil } =
    new Generic[DAABB] {
      type Repr = Double :: AABB :: HNil
      def to(t: DAABB): Repr = t match { case DAABB(x, y) => ::(x, ::(y, HNil)) }
      def from(r: Repr): DAABB = r match { case ::(x, ::(y, HNil)) => DAABB(x, y) }
    }

  implicit val genIDAABBS: Generic[IDAABBS] { type Repr = Int :: DAABB :: String :: HNil } =
    new Generic[IDAABBS] {
      type Repr = Int :: DAABB :: String :: HNil
      def to(t: IDAABBS): Repr = t match { case IDAABBS(x, y, z) => ::(x, ::(y, ::(z, HNil))) }
      def from(r: Repr): IDAABBS = r match { case ::(x, ::(y, ::(z, HNil))) => IDAABBS(x, y, z) }
    }

  implicit val genDog: Generic[Dog] { type Repr = Long :: HNil } =
    new Generic[Dog] {
      type Repr = Long :: HNil
      def to(t: Dog): Repr = t match { case Dog(x) => ::(x, HNil) }
      def from(r: Repr): Dog = r match { case ::(x, HNil) => Dog(x) }
    }

  implicit val genCat: Generic[Cat] { type Repr = String :: Either[Cat, Dog] :: HNil } =
    new Generic[Cat] {
      type Repr = String :: Either[Cat, Dog] :: HNil
      def to(t: Cat): Repr = t match { case Cat(x, y) => ::(x, ::(y, HNil)) }
      def from(r: Repr): Cat = r match { case ::(x, ::(y, HNil)) => Cat(x, y) }
    }

  implicit def genEither[A, B]: Generic[Either[A, B]] { type Repr = Left[A, B] :+: Right[A, B] :+: CNil } =
    new Generic[Either[A, B]] {
      type Repr = Left[A, B] :+: Right[A, B] :+: CNil
      def to(t: Either[A, B]): Repr = t match {
        case (x: Left[A, B]  @unchecked) => Inl(x)
        case (x: Right[A, B] @unchecked) => Inr(Inl(x))
      }
      def from(r: Repr): Either[A, B] = r match {
        case Inl(x) => x
        case Inr(Inl(x)) => x
        case _ => ???
      }
    }

  implicit def genLeft[A, B]: Generic[Left[A, B]] { type Repr = A :: HNil } =
    new Generic[Left[A, B]] {
      type Repr = A :: HNil
      def to(t: Left[A, B]): Repr = t match { case Left(x) => ::(x, HNil) }
      def from(r: Repr): Left[A, B] = r match { case ::(x, HNil) => Left(x) }
    }

  implicit def genRight[A, B]: Generic[Right[A, B]] { type Repr = B :: HNil } =
    new Generic[Right[A, B]] {
      type Repr = B :: HNil
      def to(t: Right[A, B]): Repr = t match { case Right(x) => ::(x, HNil) }
      def from(r: Repr): Right[A, B] = r match { case ::(x, HNil) => Right(x) }
    }
}

// First example from https://github.com/milessabin/shapeless-type-class-derivation-2015-demo
object equal {
  trait Eq[T] {
    def eqv(x: T, y: T): Boolean
  }

  object Eq {
    implicit val eqInt: Eq[Int] =
      new Eq[Int] {
        def eqv(x: Int, y: Int): Boolean = x == y
      }

    implicit val eqString: Eq[String] =
      new Eq[String] {
        def eqv(x: String, y: String): Boolean = x == y
      }

    implicit def eqGeneric[T, R]
      (implicit
        gen: Generic[T] { type Repr = R },
        eqRepr: => Eq[R]
      ): Eq[T] =
        new Eq[T] {
          def eqv(x: T, y: T): Boolean =
            eqRepr.eqv(gen.to(x), gen.to(y))
        }

    implicit val eqHNil: Eq[HNil] = new Eq[HNil] {
      def eqv(x: HNil, y: HNil): Boolean = true
    }

    implicit def eqHCons[H, T <: HList]
      (implicit
        eqH: Eq[H],
        eqT: Eq[T]
      ): Eq[H :: T] =
        new Eq[H :: T] {
          def eqv(x: H :: T, y: H :: T): Boolean =
            eqH.eqv(x.head, y.head) && eqT.eqv(x.tail, y.tail)
        }

    implicit val eqCNil: Eq[CNil] = new Eq[CNil] {
      def eqv(x: CNil, y: CNil): Boolean = true
    }

    implicit def eqCNCons[H, T <: Coproduct]
      (implicit
        eqH: Eq[H],
        eqT: Eq[T]
      ): Eq[H :+: T] =
        new Eq[H :+: T] {
          def eqv(x: H :+: T, y: H :+: T): Boolean =
            (x, y) match {
              case (Inl(xh), Inl(yh)) => eqH.eqv(xh, yh)
              case (Inr(xt), Inr(yt)) => eqT.eqv(xt, yt)
              case _ => false
            }
        }
  }

  implicit class EqOps[T](x: T)(implicit eqT: Eq[T]) {
    def ===(y: T): Boolean = eqT.eqv(x, y)
  }

  import GenericInstances._

  implicit val EqLongInstance:   Eq[Long]   = new Eq[Long]   { def eqv(x: Long, y: Long):     Boolean = x == y }
  implicit val EqDoubleInstance: Eq[Double] = new Eq[Double] { def eqv(x: Double, y: Double): Boolean = x == y }
  implicit val EqIntInstance:    Eq[Int]    = new Eq[Int]    { def eqv(x: Int, y: Int):       Boolean = x == y }
  implicit val EqStringInstance: Eq[String] = new Eq[String] { def eqv(x: String, y: String): Boolean = x == y }

  implicitly[Eq[Dog]]
  implicitly[Eq[Cat]]
  implicitly[Eq[IDAABBS]]
}
