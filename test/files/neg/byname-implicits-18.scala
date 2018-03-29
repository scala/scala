/*
 * Demo of using by name implicits to resolve (hidden) divergence issues when
 * traversing recursive generic structures.
 *
 * See http://stackoverflow.com/questions/25923974
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

object Test extends App {
  case class Bootstrap[+A](head: A, tail: Option[Bootstrap[(A, A)]])
  object Bootstrap {
    type BootstrapRepr[+A] = A :: Option[Bootstrap[(A, A)]] :: HNil
    implicit def bootstrapGen[A]: Generic.Aux[Bootstrap[A], BootstrapRepr[A]] =
      new Generic[Bootstrap[A]] {
        type Repr = BootstrapRepr[A]
        def to(t: Bootstrap[A]): Repr = t.head :: t.tail :: HNil
        def from(r: Repr): Bootstrap[A] = Bootstrap(r.head, r.tail.head)
      }
  }

  class Tc[A]
  object Tc {
    implicit val tcInt: Tc[Int] = new Tc
    implicit def tcOpt[A: Tc]: Tc[Option[A]] = new Tc
    implicit def tcTuple[A: Tc, B: Tc]: Tc[(A, B)] = new Tc
    implicit val tcHNil: Tc[HNil] = new Tc
    implicit def tcHCons[H: Tc, T <: HList: Tc]: Tc[H :: T] = new Tc
    implicit def tcGen[A, R <: HList](
      implicit gen: Generic.Aux[A, R], tcR: => Tc[R]
    ): Tc[A] = new Tc
  }

  implicitly[Tc[Bootstrap[Int]]]
}
