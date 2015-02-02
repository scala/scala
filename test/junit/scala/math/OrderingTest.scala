package scala.math

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class OrderingTest {

  /* Test for SI-9077 */
  @Test
  def testReverseOrdering {
    def check[T: Ordering](t1: T, t2: T): Unit = {
      val O = Ordering[T]
      val R = O.reverse
      assertEquals(O.min(t1, t2), R.max(t1, t2))
      assertEquals(O.max(t1, t2), R.min(t1, t2))

      assertEquals(O.lteq(t1, t2), R.lteq(t2, t1))
      assertEquals(O.lt(t1, t2), R.lt(t2, t1))
      assertEquals(O.gteq(t1, t2), R.gteq(t2, t1))
      assertEquals(O.gt(t1, t2), R.gt(t2, t1))
      assertEquals(O.compare(t1, t2), R.compare(t2, t1))

      assertEquals(O.equiv(t1, t2), R.equiv(t1, t2))

      assertEquals(O.on((x: T) => x).min(t1, t2), R.on((x: T) => x).max(t1, t2))

      assertEquals(O.tryCompare(t1, t2), R.tryCompare(t2, t1))

      assertEquals(O.mkOrderingOps(t1).<(t2), R.mkOrderingOps(t2).<(t1))
      assertEquals(O.mkOrderingOps(t1).<=(t2), R.mkOrderingOps(t2).<=(t1))
      assertEquals(O.mkOrderingOps(t1).>(t2), R.mkOrderingOps(t2).>(t1))
      assertEquals(O.mkOrderingOps(t1).>=(t2), R.mkOrderingOps(t2).>=(t1))

      assertEquals(O.mkOrderingOps(t1).min(t2), R.mkOrderingOps(t1).max(t2))
      assertEquals(O.mkOrderingOps(t1).max(t2), R.mkOrderingOps(t1).min(t2))
    }
    def checkAll[T: Ordering](ts: T*): Unit = {
      for (t1 <- ts; t2 <- ts) check(t1, t2)
    }
    checkAll[Unit](())
    checkAll[Boolean](true, false)
    checkAll[Byte](Byte.MinValue, -1.toByte, 0.toByte, 1.toByte, Byte.MaxValue)
    checkAll[Char](Char.MinValue, -1.toChar, 0.toChar, 1.toChar, Char.MaxValue)
    checkAll[Short](Short.MinValue, -1, 0, 1, Short.MaxValue)
    checkAll[Int](Int.MinValue, -1, 0, 1, Int.MaxValue)
    checkAll[Double](Double.MinValue, -1, -0, 0, 1, Double.MaxValue)
    checkAll[Float](Float.MinValue, -1, -0, 0, 1, Float.MaxValue)

    checkAll[BigInt](Int.MinValue, -1, 0, 1, Int.MaxValue)
    checkAll[BigDecimal](Int.MinValue, -1, -0, 1, Int.MaxValue)
    checkAll[String]("", "a", "b", "bb")
    checkAll[String]("", "a", "b", "bb")
    checkAll[Option[Int]](None, Some(1), Some(2))
    checkAll[Iterable[Int]](Nil, List(1), List(1, 2))
    checkAll[(Int, Int)]((1, 2), (1, 3), (4, 5))
  }
}

