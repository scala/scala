package strawman

package collection.test

import org.junit.Test
import strawman.collection.Iterable
import strawman.collection.mutable.Builder
import strawman.collection._

import scala.{Any, Either, Int, Left, None, Option, Right, Some, Unit}
import scala.Predef.ArrowAssoc
import scala.math.Ordering
import java.lang.String

class TraverseTest {

  // You can either overload methods for PolyBuildable and ConstrainedPolyBuildable (if you want to support constrained collection types)
  def optionSequence1[C[X] <: Iterable[X] with PolyBuildable[X, C], A](xs: C[Option[A]]): Option[C[A]] =
    xs.foldLeft[Option[Builder[A, C[A]]]](Some(xs.newBuilder)) {
      case (Some(builder), Some(a)) => Some(builder += a)
      case _ => None
    }.map(_.result)

  def optionSequence1[Ev[_], CC[_], A](xs: ConstrainedPolyBuildable[Option[A], CC, Ev] with Iterable[Option[A]])(implicit ev: Ev[A]): Option[CC[A]] =
    xs.foldLeft[Option[Builder[A, CC[A]]]](Some(xs.newConstrainedBuilder)) {
      case (Some(builder), Some(a)) => Some(builder += a)
      case _ => None
    }.map(_.result)

  // ...or use BuildFrom to abstract over both and also allow building arbitrary collection types
  def optionSequence[CC[X] <: Iterable[X], A](xs: CC[Option[A]])(implicit bf: BuildFrom[CC[Option[A]], A]): Option[bf.To] =
    xs.foldLeft[Option[Builder[A, bf.To]]](Some(bf.newBuilder(xs))) {
      case (Some(builder), Some(a)) => Some(builder += a)
      case _ => None
    }.map(_.result)

  def eitherSequence[C[X] <: Iterable[X], A, B](xs: C[Either[A, B]])(implicit bf: BuildFrom[xs.type, B]): Either[A, bf.To] =
    xs.foldLeft[Either[A, Builder[B, bf.To]]](Right(bf.newBuilder(xs))) {
      case (Right(builder), Right(b)) => Right(builder += b)
      case (Left(a)       ,        _) => Left(a)
      case (_             ,  Left(a)) => Left(a)
    }.right.map(_.result)

  @Test
  def optionSequence1Test: Unit = {
    val xs1 = immutable.List(Some(1), None, Some(2))
    val o1 = optionSequence1(xs1)
    val o1t: Option[immutable.List[Int]] = o1

    val xs2 = immutable.TreeSet(Some("foo"), Some("bar"), None)
    val o2 = optionSequence1(xs2)
    val o2t: Option[immutable.TreeSet[String]] = o2
  }

  def optionSequenceTest: Unit = {
    val xs1 = immutable.List(Some(1), None, Some(2))
    val o1 = optionSequence(xs1)
    val o1t: Option[immutable.List[Int]] = o1

    val xs2 = immutable.TreeSet(Some("foo"), Some("bar"), None)
    val o2 = optionSequence(xs2)(
     BuildFrom.buildFromConstrainedPolyBuildable[Ordering, immutable.TreeSet, Option[String], String])
    val o2t: Option[immutable.TreeSet[String]] = o2

    // Breakout-like use case from https://github.com/scala/scala/pull/5233:
    val xs4 = immutable.List[Option[(Int, String)]](Some((1 -> "a")), Some((2 -> "b")))
    val o4 = optionSequence(xs4)(immutable.TreeMap) // same syntax as in `.to`
    val o4t: Option[immutable.TreeMap[Int, String]] = o4
  }

  @Test
  def eitherSequenceTest: Unit = {
    val xs3: mutable.ListBuffer[Either[Int, String]] = mutable.ListBuffer(Right("foo"), Left(0), Right("bar"))
    val e1 = eitherSequence(xs3)
    val e1t: Either[Int, mutable.ListBuffer[String]] = e1
  }
}
