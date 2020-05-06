package scala.math

import org.junit.Test
import scala.annotation.unused

class PartialOrderingTest {
  import EquivTest._

  @Test
  def testOrderingToPartialOrderingResolution(): Unit = {
    implicit val o: Ordering[Box[Int]] = intBoxOrdering

    assert(PartialOrdering[Box[Int]] == intBoxOrdering)
  }

  @Test
  def testOrderingAndPartialOrderingToPartialOrderingResolution(): Unit = {
    @unused implicit val po: PartialOrdering[Box[Int]] = intBoxOrdering
    implicit val o: Ordering[Box[Int]] = intBoxOrdering

    assert(PartialOrdering[Box[Int]] == intBoxOrdering)
  }
}
