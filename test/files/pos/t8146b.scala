// non-deterministic type errors, non-termination.
// seems to be due to inconsistent hashing/equality in SubTypePair

import scala.language.{existentials, implicitConversions}
import scala.annotation.unchecked.uncheckedVariance

trait Column[T]

// Turning this into a trait reduces (eliminates?) the likelihood of type errors (but not of non-termination)
abstract class Shape[Level <: ShapeLevel, -Mixed_, Unpacked_, Packed_]

trait ShapeLevel
trait NestedShapeLevel extends ShapeLevel
trait FlatShapeLevel extends NestedShapeLevel
trait ColumnsShapeLevel extends FlatShapeLevel

trait ProvenShape[U]

object ProvenShape {
  implicit def proveShapeOf[T, U](v: T)(implicit sh: Shape[_ <: FlatShapeLevel, T, U, _]): ProvenShape[U] = ???
}

sealed abstract class HList {
  type Self <: HList
  type :: [E] = HCons[E, Self]
  final def :: [E](elem: E): :: [E] = ???
}

final class HCons[+H, +T <: HList](val head: H, val tail: T) extends HList {
  type Self = HCons[H @uncheckedVariance, T @uncheckedVariance]
}

final object HNil extends HList {
  type Self = HNil.type
}

// Success is more likely when not using these aliases
object syntax {
  type :: [+H, +T <: HList] = HCons[H, T]
  type HNil = HNil.type
}

class HListBench {

  import syntax._

  implicit def columnShape[T, Level <: ShapeLevel]: Shape[Level, Column[T], T, Column[T]] = ???
  implicit def provenShape[T, P](implicit shape: Shape[_ <: FlatShapeLevel, T, _, P]): Shape[FlatShapeLevel, ProvenShape[T], T, P] = ???
  final class HListShape[Level <: ShapeLevel, M <: HList, U <: HList, P <: HList](val shapes: Seq[Shape[_ <: ShapeLevel, _, _, _]]) extends Shape[Level, M, U, P]
  implicit def hnilShape[Level <: ShapeLevel] = new HListShape[Level, HNil.type, HNil.type, HNil.type](Nil)
  implicit def hconsShape[Level <: ShapeLevel, M1, M2 <: HList, U1, U2 <: HList, P1, P2 <: HList]
    (implicit s1: Shape[_ <: Level, M1, U1, P1], s2: HListShape[_ <: Level, M2, U2, P2]) =
    new HListShape[Level, M1 :: M2, U1 :: U2, P1 :: P2](s1 +: s2.shapes)

  trait A[T] {
    def * : ProvenShape[T]
  }

  trait B extends A[
    Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: Int :: Int :: Int ::
    Int :: Int :: HNil ] {

    def c: Column[Int]

    def * = c :: c :: c :: c :: c ::
            c :: c :: c :: c :: c ::
            c :: c :: c :: c :: c ::
            c :: c :: c :: c :: c ::
            c :: c :: c :: c :: c ::
            c :: c :: HNil

  }
}
