package scala.math

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import java.{lang => jl}

import scala.collection.SortedSet
import scala.math.Ordering.Float.TotalOrdering
import scala.math.Ordering.Double.TotalOrdering

@RunWith(classOf[JUnit4])
class OrderingTest {
  val floats = Seq(
    Float.NegativeInfinity,
    Float.MinValue,
    -1f,
    -0f,
    0f,
    Float.MinPositiveValue,
    1f,
    Float.MaxValue,
    Float.PositiveInfinity,
    Float.NaN
  )

  val doubles = Seq(
    Double.NegativeInfinity,
    Double.MinValue,
    -1d,
    -0d,
    0d,
    Double.MinPositiveValue,
    1d,
    Double.MaxValue,
    Double.PositiveInfinity,
    Double.NaN
  )

  /* Test for scala/bug#9077 */
  @Test
  def reverseOrdering(): Unit = {
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
    checkAll[Double](doubles: _*)
    checkAll[Float](floats: _*)

    checkAll[BigInt](Int.MinValue, -1, 0, 1, Int.MaxValue)
    checkAll[BigDecimal](Int.MinValue, -1, -0, 1, Int.MaxValue)
    checkAll[String]("", "a", "b", "bb")
    checkAll[String]("", "a", "b", "bb")
    checkAll[Option[Int]](None, Some(1), Some(2))
    checkAll[Iterable[Int]](Nil, List(1), List(1, 2))
    checkAll[(Int, Int)]((1, 2), (1, 3), (4, 5))
  }

  @Test
  def reverseOf(): Unit = {
    def check[T](ord: Ordering[T]): Unit = {
      assert(ord isReverseOf ord.reverse)
      assert(ord.reverse isReverseOf ord)
      assert(!(ord isReverseOf ord))
      assert(!(ord.reverse isReverseOf ord.reverse))
      assert(!ord.isReverseOf({ (_, _) => 0 }: Ordering[T]))
      assert(!ord.reverse.isReverseOf({ (_, _) => 0 }: Ordering[T]))
    }

    check(Ordering[Int])
    check(Ordering[(Int, Long)])
    check(Ordering[(Int, Long, Float)])
    check(Ordering[(Int, Long, Float, Double)])
    check(Ordering[(Int, Long, Float, Double, Byte)])
    check(Ordering[(Int, Long, Float, Double, Byte, Char)])
    check(Ordering[(Int, Long, Float, Double, Byte, Char, Short)])
    check(Ordering[(Int, Long, Float, Double, Byte, Char, Short, BigInt)])
    check(Ordering[(Int, Long, Float, Double, Byte, Char, Short, BigInt, BigDecimal)])
    check(Ordering[Option[Int]])

    import Ordering.Implicits._
    check(Ordering[Seq[Int]])
    check(Ordering[SortedSet[Int]])
  }

  @Test
  def cachedReverse(): Unit = {
    def check[T](ord: Ordering[T]): Unit = {
      assert(ord.reverse eq ord.reverse)
    }

    check(Ordering[Int])
  }

  @Test
  def composedOrdering(): Unit = {
    case class Pair(a: Int, b: Int)

    def check(ord1: Ordering[Pair], ord2: Ordering[Pair]): Unit = {
      val pairs = List(Pair(0, 0), Pair(0, 1), Pair(1, 1))
      for (p1 <- pairs; p2 <- pairs) {
        assertEquals(signum(ord1.compare(p1, p2)), signum(ord2.compare(p1, p2)))
      }
    }

    val o1 = Ordering.by[Pair, (Int, Int)]((p: Pair) => (p.a, p.b))
    val o2 = Ordering.by[Pair, Int](_.a).orElseBy[Int](_.b)
    val o3 = Ordering.by[Pair, Int](_.a).orElse(Ordering.by[Pair, Int](_.b))

    check(o1, o2)
    check(o1, o3)
  }

  /* Test for scala/bug#10511 */
  @Test
  def floatDoubleTotalOrdering(): Unit = {
    val fNegZeroBits = jl.Float.floatToRawIntBits(-0.0f)
    val fPosZeroBits = jl.Float.floatToRawIntBits(0.0f)

    val dNegZeroBits = jl.Double.doubleToRawLongBits(-0.0d)
    val dPosZeroBits = jl.Double.doubleToRawLongBits(0.0d)

    def checkFloats(floats: Float*): Unit = {
      def same(f1: Float, f2: Float): Boolean = {
        val thisBits = jl.Float.floatToRawIntBits(f1)
        if (thisBits == fNegZeroBits) jl.Float.floatToRawIntBits(f2) == fNegZeroBits
        else if (thisBits == fPosZeroBits) jl.Float.floatToRawIntBits(f2) == fPosZeroBits
        else f1 == f2 || (jl.Float.isNaN(f1) && jl.Float.isNaN(f2))
      }

      val O = Ordering[Float]
      for (i <- floats; j <- floats) {
        val msg = s"for i=$i, j=$j"

        // consistency with `compare`
        assertEquals(msg, O.compare(i, j) < 0, O.lt(i, j))
        assertEquals(msg, O.compare(i, j) <= 0, O.lteq(i, j))
        assertEquals(msg, O.compare(i, j) == 0, O.equiv(i, j))
        assertEquals(msg, O.compare(i, j) >= 0, O.gteq(i, j))
        assertEquals(msg, O.compare(i, j) > 0, O.gt(i, j))

        // consistency with other ops
        assertTrue(msg, O.lteq(i, j) || O.gteq(i, j))
        assertTrue(msg, O.lteq(i, j) || O.gt(i, j))
        assertTrue(msg, O.lteq(i, j) != O.gt(i, j))
        assertTrue(msg, O.lt(i, j) || O.gteq(i, j))
        assertTrue(msg, O.lt(i, j) != O.gteq(i, j))
        // exactly one of `lt`, `equiv`, `gt` is true
        assertTrue(msg,
          (O.lt(i, j) ^ O.equiv(i, j) ^ O.gt(i, j))
            && !(O.lt(i, j) && O.equiv(i, j) && O.gt(i, j)))

        // consistency with `max` and `min`
        assertEquals(msg, O.compare(i, j) >= 0, same(O.max(i, j), i))
        assertEquals(msg, O.compare(i, j) <= 0, same(O.min(i, j), i))
        if (!same(i, j)) {
          assertEquals(msg, O.compare(i, j) < 0, same(O.max(i, j), j))
          assertEquals(msg, O.compare(i, j) > 0, same(O.min(i, j), j))
        }
      }
    }

    def checkDoubles(doubles: Double*): Unit = {
      def same(d1: Double, d2: Double): Boolean = {
        val thisBits = jl.Double.doubleToRawLongBits(d1)
        if (thisBits == dNegZeroBits) jl.Double.doubleToRawLongBits(d2) == dNegZeroBits
        else if (thisBits == dPosZeroBits) jl.Double.doubleToRawLongBits(d2) == dPosZeroBits
        else d1 == d2 || (jl.Double.isNaN(d1) && jl.Double.isNaN(d2))
      }

      val O = Ordering[Double]
      for (i <- doubles; j <- doubles) {
        val msg = s"for i=$i, j=$j"

        // consistency with `compare`
        assertEquals(msg, O.compare(i, j) < 0, O.lt(i, j))
        assertEquals(msg, O.compare(i, j) <= 0, O.lteq(i, j))
        assertEquals(msg, O.compare(i, j) == 0, O.equiv(i, j))
        assertEquals(msg, O.compare(i, j) >= 0, O.gteq(i, j))
        assertEquals(msg, O.compare(i, j) > 0, O.gt(i, j))

        // consistency with other ops
        assertTrue(msg, O.lteq(i, j) || O.gteq(i, j))
        assertTrue(msg, O.lteq(i, j) || O.gt(i, j))
        assertTrue(msg, O.lteq(i, j) != O.gt(i, j))
        assertTrue(msg, O.lt(i, j) || O.gteq(i, j))
        assertTrue(msg, O.lt(i, j) != O.gteq(i, j))
        // exactly one of `lt`, `equiv`, `gt` is true
        assertTrue(msg,
          (O.lt(i, j) ^ O.equiv(i, j) ^ O.gt(i, j))
            && !(O.lt(i, j) && O.equiv(i, j) && O.gt(i, j)))

        // consistency with `max` and `min`
        assertEquals(msg, O.compare(i, j) >= 0, same(O.max(i, j), i))
        assertEquals(msg, O.compare(i, j) <= 0, same(O.min(i, j), i))
        if (!same(i, j)) {
          assertEquals(msg, O.compare(i, j) < 0, same(O.max(i, j), j))
          assertEquals(msg, O.compare(i, j) > 0, same(O.min(i, j), j))
        }
      }
    }

    checkFloats(floats: _*)
    checkDoubles(doubles: _*)
  }

  @Test
  def floatDoubleIeeeOrdering(): Unit = {
    def checkFloats(floats: Float*): Unit = {
      val O = Ordering.Float.IeeeOrdering
      for (i <- floats; j <- floats) {
        val msg = s"for i=$i, j=$j"

        // consistency with `compare`
        assertEquals(msg, O.compare(i, j) < 0, O.lt(i, j))
        assertEquals(msg, O.compare(i, j) <= 0, O.lteq(i, j))
        assertEquals(msg, O.compare(i, j) == 0, O.equiv(i, j))
        assertEquals(msg, O.compare(i, j) >= 0, O.gteq(i, j))
        assertEquals(msg, O.compare(i, j) > 0, O.gt(i, j))

        // consistency with other ops
        assertTrue(msg, O.lteq(i, j) || O.gteq(i, j))
        assertTrue(msg, O.lteq(i, j) || O.gt(i, j))
        assertTrue(msg, O.lteq(i, j) != O.gt(i, j))
        assertTrue(msg, O.lt(i, j) || O.gteq(i, j))
        assertTrue(msg, O.lt(i, j) != O.gteq(i, j))
        // exactly one of `lt`, `equiv`, `gt` is true
        assertTrue(msg,
          (O.lt(i, j) ^ O.equiv(i, j) ^ O.gt(i, j))
            && !(O.lt(i, j) && O.equiv(i, j) && O.gt(i, j)))
      }
    }

    def checkDoubles(doubles: Double*): Unit = {
      val O = Ordering.Double.IeeeOrdering
      for (i <- doubles; j <- doubles) {
        val msg = s"for i=$i, j=$j"

        // consistency with `compare`
        assertEquals(msg, O.compare(i, j) < 0, O.lt(i, j))
        assertEquals(msg, O.compare(i, j) <= 0, O.lteq(i, j))
        assertEquals(msg, O.compare(i, j) == 0, O.equiv(i, j))
        assertEquals(msg, O.compare(i, j) >= 0, O.gteq(i, j))
        assertEquals(msg, O.compare(i, j) > 0, O.gt(i, j))

        // consistency with other ops
        assertTrue(msg, O.lteq(i, j) || O.gteq(i, j))
        assertTrue(msg, O.lteq(i, j) || O.gt(i, j))
        assertTrue(msg, O.lteq(i, j) != O.gt(i, j))
        assertTrue(msg, O.lt(i, j) || O.gteq(i, j))
        assertTrue(msg, O.lt(i, j) != O.gteq(i, j))
        // exactly one of `lt`, `equiv`, `gt` is true
        assertTrue(msg,
          (O.lt(i, j) ^ O.equiv(i, j) ^ O.gt(i, j))
            && !(O.lt(i, j) && O.equiv(i, j) && O.gt(i, j)))
      }
    }

    checkFloats(floats.filterNot(jl.Float.isNaN): _*)
    checkDoubles(doubles.filterNot(jl.Double.isNaN): _*)
  }

  /* Test for scala/bug#8664 */
  @Test
  def symbolOrdering(): Unit = {
    assertEquals(Seq('b, 'c, 'a).sorted, Seq('a, 'b, 'c))
  }

  @Test
  def orderingEquality(): Unit = {
    def check[T](ord: => Ordering[T]): Unit = {
      assertEquals(ord, ord)
      assertEquals(ord.hashCode(), ord.hashCode())
      assertEquals(ord.reverse, ord.reverse)
      assertEquals(ord.reverse.hashCode(), ord.reverse.hashCode())
    }

    check(Ordering[Int])
    check(Ordering[(Int, Long)])
    check(Ordering[(Int, Long, Float)])
    check(Ordering[(Int, Long, Float, Double)])
    check(Ordering[(Int, Long, Float, Double, Byte)])
    check(Ordering[(Int, Long, Float, Double, Byte, Char)])
    check(Ordering[(Int, Long, Float, Double, Byte, Char, Short)])
    check(Ordering[(Int, Long, Float, Double, Byte, Char, Short, BigInt)])
    check(Ordering[(Int, Long, Float, Double, Byte, Char, Short, BigInt, BigDecimal)])
    check(Ordering[Option[Int]])

    import Ordering.Implicits._
    check(Ordering[Seq[Int]])
    check(Ordering[SortedSet[Int]])
  }

  /* Test for scala/bug#11284 */
  @Test
  def supertypeOrdering(): Unit = {
    val before = java.time.LocalDate.of(2004, 1, 20)
    val now = java.time.LocalDate.now()
    val later = java.time.LocalDate.now().plusWeeks(1)

    assertEquals(Seq(before, now, later), Seq(now, later, before).sorted)
  }
}
