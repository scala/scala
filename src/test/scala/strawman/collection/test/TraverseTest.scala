package strawman

package collection.test

import org.junit.Test
import strawman.collection.Iterable
import strawman.collection.mutable.Builder
import strawman.collection._

import scala.{Either, Int, Left, None, Option, Right, Some, Unit}
import scala.Predef.ArrowAssoc
import java.lang.String

class TraverseTest {

  def optionSequence[C[X] <: Iterable[X], A](xs: C[Option[A]])(implicit fi: CanBuild[A, C[A]]): Option[C[A]] =
    xs.foldLeft[Option[Builder[A, C[A]]]](Some(fi.newBuilder)) {
      case (Some(builder), Some(a)) => Some(builder += a)
      case _ => None
    }.map(_.result)

  def eitherSequence[C[X] <: Iterable[X], A, B](xs: C[Either[A, B]])(implicit fi: CanBuild[B, C[B]]): Either[A, C[B]] =
    xs.foldLeft[Either[A, Builder[B, C[B]]]](Right(fi.newBuilder)) {
      case (Right(builder), Right(b)) => Right(builder += b)
      case (Left(a)       ,        _) => Left(a)
      case (_             ,  Left(a)) => Left(a)
    }.map(_.result)

//  @Test
  def traverseTest: Unit = {

    val xs1 = immutable.List(Some(1), None, Some(2))
    val o1 = optionSequence(xs1)
    val o1t: Option[immutable.List[Int]] = o1

    val xs2 = immutable.TreeSet(Some("foo"), Some("bar"), None)
    val o2 = optionSequence(xs2)
    val o2t: Option[immutable.TreeSet[String]] = o2

    val xs3 = mutable.ListBuffer(Right("foo"), Left(0), Right("bar"))
    val e1 = eitherSequence(xs3)
    val e1t: Either[Int, mutable.ListBuffer[String]] = e1

    // This use case from https://github.com/scala/scala/pull/5233 still eludes us
    //val xs4 = immutable.List((1 -> "a"), (2 -> "b"))
    //val o4 = optionSequence(xs1)(immutable.TreeMap.iterableFactory)
    //val o4t: Option[immutable.TreeMap[Int, String]] = o4
  }

}
