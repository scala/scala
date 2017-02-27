package strawman

package collection.test

import org.junit.Test
import strawman.collection.Iterable
import strawman.collection.mutable.Builder
import strawman.collection._

import scala.{Either, Int, Left, None, Option, Right, Some, Unit}
import java.lang.String

class TraverseTest {

  def optionSequence[C[X] <: Iterable[X], A](xs: C[Option[A]])(implicit canBuild: () => Builder[A, C[A]]): Option[C[A]] =
    xs.foldLeft[Option[Builder[A, C[A]]]](Some(canBuild.apply())) {
      case (Some(builder), Some(a)) => Some(builder += a)
      case _ => None
    }.map(_.result)

  def eitherSequence[C[X] <: Iterable[X], A, B](xs: C[Either[A, B]])(implicit canBuild: () => Builder[B, C[B]]): Either[A, C[B]] =
    xs.foldLeft[Either[A, Builder[B, C[B]]]](Right(canBuild.apply())) {
      case (Right(builder), Right(b)) => Right(builder += b)
      case (Left(a)       ,        _) => Left(a)
      case (_             ,  Left(a)) => Left(a)
    }.map(_.result)

//  @Test
  def traverseTest: Unit = {

    val xs1 = immutable.List(Some(1), None, Some(2))
    optionSequence(xs1)(immutable.List.canBuild[Int]) // TODO Remove explicit CanBuild parameter after https://issues.scala-lang.org/browse/SI-10081 is fixed?

    val xs2 = immutable.TreeSet(Some("foo"), Some("bar"), None)
    optionSequence(xs2)(immutable.TreeSet.canBuild[String])

    val xs3 = mutable.ListBuffer(Right("foo"), Left(0), Right("bar"))
    eitherSequence(xs3)(mutable.ListBuffer.canBuild[String])

  }

}
