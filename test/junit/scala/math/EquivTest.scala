package scala.math

import org.junit.Test
import scala.annotation.unused

class EquivTest {
  import EquivTest._

  @Test
  def testOrderingToEquivResolution(): Unit = {
    implicit val o: Ordering[Box[Int]] = intBoxOrdering

    assert(Equiv[Box[Int]] == intBoxOrdering)
  }

  @Test
  def testPartialOrderingToEquivResolution(): Unit = {
    implicit val po: PartialOrdering[Box[Int]] = intBoxOrdering

    assert(Equiv[Box[Int]] == intBoxOrdering)
  }

  @Test
  def testOrderingAndPartialOrderingToEquivResolution(): Unit = {
    @unused implicit val po: PartialOrdering[Box[Int]] = intBoxOrdering
    implicit val o: Ordering[Box[Int]] = intBoxOrdering

    assert(Equiv[Box[Int]] == intBoxOrdering)
  }

  @deprecated("Uses deprecated Equiv.universalEquiv", since="2.13")
  @Test
  def testUniversalEquivResolution(): Unit = {
    // Use explicit Equiv.universal instead.
    assert(Equiv[Box[Int]].equiv(Box(3), Box(1 + 2)))
  }
}

object EquivTest {
  final case class Box[A](value: A)

  val intBoxOrdering: Ordering[Box[Int]] = Ordering.by(_.value)
}
