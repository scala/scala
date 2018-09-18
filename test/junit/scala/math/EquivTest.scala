package scala.math

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class EquivTest {
  import EquivTest._
  
  @Test
  def testOrderingToEquivResolution: Unit = {
    implicit val o: Ordering[Box[Int]] = intBoxOrdering

    assert(Equiv[Box[Int]].equiv(Box(3), Box(1 + 2)))
  }

  @Test
  def testPartialOrderingToEquivResolution: Unit = {
    implicit val po: PartialOrdering[Box[Int]] = intBoxOrdering

    assert(Equiv[Box[Int]].equiv(Box(3), Box(1 + 2)))
  }

  @Test
  def testOrderingAndPartialOrderingToEquivResolution: Unit = {
    implicit val po: PartialOrdering[Box[Int]] = intBoxOrdering
    implicit val o: Ordering[Box[Int]] = intBoxOrdering

    assert(Equiv[Box[Int]].equiv(Box(3), Box(1 + 2)))
  }
}

object EquivTest {
  final case class Box[A](value: A)

  val intBoxOrdering: Ordering[Box[Int]] = Ordering.by(_.value)
}
