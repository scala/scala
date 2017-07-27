package strawman
package collection
package test

import mutable.Builder

import scala.util.Try
import scala.{Any, Int, None, Nothing, Option, Ordering, Some, Unit}
import scala.Predef.{augmentString, implicitly, wrapRefArray, assert}
import java.lang.String

import org.junit.Test

trait Parse[A] {
  def parse(s: String): Option[A]
}

object Parse {

  implicit def parseInt: Parse[Int] = (s: String) => Try(s.toInt).toOption

  implicit def parseTuple[A, B](implicit
                                parseA: Parse[A],
                                parseB: Parse[B]
                               ): Parse[(A, B)] = { (s: String) =>
    val parts = s.split("-")
    (parseA.parse(parts(0)), parseB.parse(parts(1))) match {
      case (Some(a), Some(b)) => Some((a, b))
      case _ => None
    }
  }

  def parseCollection[A, C](implicit parseA: Parse[A], cb: CanBuild[A, C]): Parse[C] = { (s: String) =>
    val parts = s.split("\\|")
    parts.foldLeft[Option[Builder[A, C]]](Some(cb.newBuilder())) { (maybeBuilder, s) =>
      (maybeBuilder, parseA.parse(s)) match {
        case (Some(builder), Some(a)) =>
          Some(builder += a)
        case _ => None
      }
    }.map(_.result())
  }

}

class GenericTest {

  @Test
  def genericTest: Unit = {
    assert(Parse.parseCollection[Int, immutable.List[Int]].parse("1|2|3").contains(1 :: 2 :: 3 :: immutable.Nil))
    assert(Parse.parseCollection[(Int, Int), immutable.HashMap[Int, Int]].parse("1-2|3-4").contains(immutable.HashMap((1, 2), (3, 4))))
  }

}
