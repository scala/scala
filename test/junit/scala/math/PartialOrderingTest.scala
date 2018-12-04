package scala.math

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class PartialOrderingTest {
  import EquivTest._

  @Test
  def testOrderingToPartialOrderingResolution: Unit = {
    implicit val o: Ordering[Box[Int]] = intBoxOrdering

    assert(PartialOrdering[Box[Int]] == intBoxOrdering)
  }

  @Test
  def testOrderingAndPartialOrderingToPartialOrderingResolution: Unit = {
    implicit val po: PartialOrdering[Box[Int]] = intBoxOrdering
    implicit val o: Ordering[Box[Int]] = intBoxOrdering

    assert(PartialOrdering[Box[Int]] == intBoxOrdering)
  }
}
