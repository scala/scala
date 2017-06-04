package strawman

package collection.test

import org.junit.Test
import strawman.collection.Iterable
import strawman.collection.mutable.{ArrayBuffer, Builder, Growable}
import strawman.collection._

import scala.{Any, Either, Int, Left, None, Option, Right, Some, Unit}
import scala.Predef.ArrowAssoc
import scala.math.Ordering
import java.lang.String

class TraverseTest {

  def optionSequence[CC[X] <: Iterable[X] with IterableOps[X, CC, _], A](xs: CC[Option[A]]): Option[CC[A]] = {
    def folder[F[X] <: Growable[X]]: (Option[F[A]], Option[A]) => Option[F[A]] = { (bo, xo) =>
      (bo, xo) match {
        case (Some(builder), Some(a)) => Some(builder += a)
        case _ => None
      }
    }
    val factory = xs.iterableFactory
    factory match {
      case iterableBuilder: IterableFactoryWithBuilder[CC] =>
        xs.foldLeft[Option[Builder[A, CC[A]]]](
          Some(iterableBuilder.newBuilder[A]())
        )(
          folder[({ type l[X] = Builder[X, CC[X]] })#l]
        ).map(_.result)
      case _ =>
        xs.foldLeft[Option[ArrayBuffer[A]]](Some(new ArrayBuffer[A]))(folder).map(_.to(xs.iterableFactory))
    }
  }

  @Test
  def optionSequence1Test: Unit = {
    val xs1 = immutable.List(Some(1), None, Some(2))
    val o1 = optionSequence(xs1)
    val o1t: Option[immutable.List[Int]] = o1

    val xs2: immutable.Set[Option[String]] = immutable.TreeSet(Some("foo"), Some("bar"), None)
    val o2 = optionSequence(xs2)
    val o2t: Option[immutable.Set[String]] = o2

    val xs4 = immutable.List[Option[(Int, String)]](Some((1 -> "a")), Some((2 -> "b")))
    val o4 = optionSequence(xs4)
    val o4t: Option[immutable.List[(Int, String)]] = o4
    val o5: Option[immutable.TreeMap[Int, String]] = o4.map(_.to(immutable.TreeMap))
  }

}
