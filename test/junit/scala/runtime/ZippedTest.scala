
package scala.runtime

import scala.language.postfixOps

import org.junit.Assert._
import org.junit.Test

import scala.collection.Seq
import scala.collection.immutable.{List, Vector}

/** Tests Tuple?Zipped */
@deprecated("Tests deprecated API", since="2.13")
class ZippedTest {
  @Test
  def crossZipped(): Unit = {

    val xs1 = List.range(1, 100)
    val xs2 = xs1.view
    val xs3 = xs1 take 10
    val ss1 = Stream from 1
    val ss2 = ss1.view
    val ss3 = ss1 take 10
    val as1 = 1 to 100 toArray
    val as2 = as1.view
    val as3 = as1 take 10

    def xss1 = List[Iterable[Int]](xs1, xs2, xs3, ss1, ss2, ss3, as1, as2, as3)
    def xss2 = List[Iterable[Int]](xs1, xs2, xs3, ss3, as1, as2, as3)  // no infinities
    def xss3 = List[Iterable[Int]](xs2, xs3, ss3, as1) // representative sampling

    for (cc1 <- xss1 ; cc2 <- xss2) {
      val sum1 = (cc1, cc2).zipped map { case (x, y) => x + y } sum
      val sum2 = (cc1, cc2).zipped map (_ + _) sum

      assert(sum1 == sum2)
    }

    for (cc1 <- xss1 ; cc2 <- xss2 ; cc3 <- xss3) {
      val sum1 = (cc1, cc2, cc3).zipped map { case (x, y, z) => x + y + z } sum
      val sum2 = (cc1, cc2, cc3).zipped map (_ + _ + _) sum

      assert(sum1 == sum2)
    }

    assert((ss1, ss1).zipped exists ((x, y) => true))
    assert((ss1, ss1, ss1).zipped exists ((x, y, z) => true))

    assert(!(ss1, ss2, 1 to 3).zipped.exists(_ + _ + _ > 100000))
    assert((1 to 3, ss1, ss2).zipped.forall(_ + _ + _ > 0))
    assert((ss1, 1 to 3, ss2).zipped.map(_ + _ + _).size == 3)
  }

  @Test
  def test_si9379(): Unit = {
    class Boom {
      private var i = -1
      def inc = {
        i += 1
        if (i > 1000) throw new NoSuchElementException("Boom! Too many elements!")
        i
      }
    }
    val b = new Boom
    val s = Stream.continually(b.inc)
    // zipped.toString must allow s to short-circuit evaluation
    assertTrue((s, s).zipped.toString contains s.toString)
  }

  @Test
  def testTuple2Invert(): Unit = {
    // prolix actuals test inferred type
    assertEquals(Seq((1, "a"), (2, "b")), { val r = (List(1, 2),   List("a", "b")).invert; r: List[(Int, String)] })
    assertEquals(Seq((1, "a"), (2, "b")), { val r = (List(1, 2),    Seq("a", "b")).invert; r: List[(Int, String)] })
    assertEquals(Seq((1, "a")          ), { val r = ( Seq(1, 2), Vector("a"     )).invert; r:  Seq[(Int, String)] })
    assertEquals(Seq((1, "a")          ), { val r = ( Seq(1   ), Vector("a", "b")).invert; r:  Seq[(Int, String)] })
  }

  @Test
  def testTuple3Invert(): Unit = {
    // prolix actuals test inferred type
    assertEquals(Seq((1, "a", 4d), (2, "b", 5d)), { val r = (  List(1, 2),     List("a", "b"),  Seq(4d, 5d)).invert; r:   List[(Int, String, Double)] })
    assertEquals(Seq((1, "a", 4d)              ), { val r = (   Seq(1   ),   Vector("a", "b"), List(4d, 5d)).invert; r:    Seq[(Int, String, Double)] })
    assertEquals(Seq((1, "b", 4d)              ), { val r = (Vector(1, 2),      Seq(     "b"), List(4d, 5d)).invert; r: Vector[(Int, String, Double)] })
    assertEquals(Seq((1, "a", 5d)              ), { val r = (Vector(1, 2),      Seq("a"     ), List(    5d)).invert; r: Vector[(Int, String, Double)] })
  }
}
