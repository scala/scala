package scala.collection.immutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.math._
import scala.util._

/* Tests various ranges by making sure they all agree on the same answers. */
@RunWith(classOf[JUnit4])
class RangeConsistencyTest {
  def r2nr[T: Integral](
    r: Range, puff: T, stride: T, check: (T,T) => Boolean, bi: T => BigInt
  ): List[(BigInt,Try[Int])] = {
    val num = implicitly[Integral[T]]
    import num._
    val one = num.one
    
    if (!check(puff, fromInt(r.start))) return Nil
    val start = puff * fromInt(r.start)
    val sp1 = start + one
    val sn1 = start - one
      
    if (!check(puff, fromInt(r.end))) return Nil
    val end = puff * fromInt(r.end)
    val ep1 = end + one
    val en1 = end - one
    
    if (!check(stride, fromInt(r.step))) return Nil
    val step = stride * fromInt(r.step)
    
    def NR(s: T, e: T, i: T) = {
      val delta = (bi(e) - bi(s)).abs - (if (r.isInclusive) 0 else 1)
      val n = if (r.length == 0) BigInt(0) else delta / bi(i).abs + 1
      if (r.isInclusive) {
        (n, Try(NumericRange.inclusive(s,e,i).length))
      }
      else {
        (n, Try(NumericRange(s,e,i).length))
      }
    } 
    
    List(NR(start, end, step)) :::
    (if (sn1 < start) List(NR(sn1, end, step)) else Nil) :::
    (if (start < sp1) List(NR(sp1, end, step)) else Nil) :::
    (if (en1 < end) List(NR(start, en1, step)) else Nil) :::
    (if (end < ep1) List(NR(start, ep1, step)) else Nil)
  }
  
  // Motivated by SI-4370: Wrong result for Long.MinValue to Long.MaxValue by Int.MaxValue
  @Test
  def rangeChurnTest() {
    val rn = new Random(4370)
    for (i <- 0 to 10000) { control.Breaks.breakable {
      val start = rn.nextInt
      val end = rn.nextInt
      val step = rn.nextInt(4) match {
        case 0 => 1
        case 1 => -1
        case 2 => (rn.nextInt(11)+2)*(2*rn.nextInt(2)+1)
        case 3 => var x = rn.nextInt; while (x==0) x = rn.nextInt; x
      }
      val r = if (rn.nextBoolean) Range.inclusive(start, end, step) else Range(start, end, step)
      
      try { r.length }
      catch { case iae: IllegalArgumentException => control.Breaks.break }
      
      val lpuff = rn.nextInt(4) match {
        case 0 => 1L
        case 1 => rn.nextInt(11)+2L
        case 2 => 1L << rn.nextInt(60)
        case 3 => math.max(1L, math.abs(rn.nextLong))
      }
      val lstride = rn.nextInt(4) match {
        case 0 => lpuff
        case 1 => 1L
        case 2 => 1L << rn.nextInt(60)
        case 3 => math.max(1L, math.abs(rn.nextLong))
      }
      val lr = r2nr[Long](
        r, lpuff, lstride, 
        (a,b) => { val x = BigInt(a)*BigInt(b); x.isValidLong },
        x => BigInt(x)
      )
      
      lr.foreach{ case (n,t) => assert(
        t match {
          case Failure(_) => n > Int.MaxValue
          case Success(m) => n == m
        },
        (r.start, r.end, r.step, r.isInclusive, lpuff, lstride, n, t)
      )}
      
      val bipuff = rn.nextInt(3) match {
        case 0 => BigInt(1)
        case 1 => BigInt(rn.nextLong) + Long.MaxValue + 2
        case 2 => BigInt("1" + "0"*(rn.nextInt(100)+1))
      }
      val bistride = rn.nextInt(3) match {
        case 0 => bipuff
        case 1 => BigInt(1)
        case 2 => BigInt("1" + "0"*(rn.nextInt(100)+1))
      }
      val bir = r2nr[BigInt](r, bipuff, bistride, (a,b) => true, identity)
      
      bir.foreach{ case (n,t) => assert(
        t match {
          case Failure(_) => n > Int.MaxValue
          case Success(m) => n == m
        },
        (r.start, r.end, r.step, r.isInclusive, bipuff, bistride, n, t)
      )}              
    }}
  }
  
  @Test
  def testSI4370() { assert{
    Try((Long.MinValue to Long.MaxValue by Int.MaxValue).length) match {
      case Failure(iae: IllegalArgumentException) => true
      case _ => false
    }
  }}
  
  @Test
  def testSI6736() {
    // These operations on overfull ranges should all succeed.
    assert( (0 to Int.MaxValue).contains(4) )
    assert( !((Int.MinValue to 0).contains(4)) )
    assert( (Int.MinValue to 0).last == 0 )
    assert( (Int.MinValue until 5).last == 4 )
    assert( (-7 to -99 by -4).last == -99 && (-7 until -99 by -4).last == -95 )
    assert( (Int.MinValue to 5) == (Int.MinValue until 6) )
    assert( (-3 to Int.MaxValue).drop(4).length == Int.MaxValue )
    assert( (-3 to Int.MaxValue).take(1234) == (-3 to 1230) )
    assert( (-3 to Int.MaxValue).dropRight(4).length == Int.MaxValue )
    assert( (-3 to Int.MaxValue).takeRight(1234).length == 1234 )
    assert( (-3 to Int.MaxValue).dropWhile(_ <= 0).length == Int.MaxValue )
    assert( (-3 to Int.MaxValue).span(_ <= 0) match { case (a,b) => a.length == 4 && b.length == Int.MaxValue } )
  }
  
  @Test
  def testSI9348() {
    // Test exclusive range with (end-start) != 0 (mod step)
    assert( (0.0f until 0.4f by 0.25f) sameElements List(0.0f, 0.25f) )
    assert( (1.0 until 2.2 by 0.5) sameElements List(1.0, 1.5, 2.0) )
    
    def bd(d: Double) = BigDecimal(d)
    val bdRange = bd(-10.0) until bd(0.0) by bd(4.5)
    assert( bdRange sameElements List(bd(-10.0), bd(-5.5), bd(-1.0)) )
  }

  @Test
  def test_SI9388()  {
    val possiblyNotDefaultNumeric = new scala.math.Numeric[Int] {
      def fromInt(x: Int) = x
      def minus(x: Int, y: Int): Int = x - y
      def negate(x: Int): Int = -x
      def plus(x: Int, y: Int): Int = x + y
      def times(x: Int, y: Int): Int = x*y
      def toDouble(x: Int): Double = x.toDouble
      def toFloat(x: Int): Float = x.toFloat
      def toInt(x: Int): Int = x
      def toLong(x: Int): Long = x.toLong
      def compare(x: Int, y: Int) = x compare y
    }
    val r = (Int.MinValue to Int.MaxValue by (1<<23))
    val nr = NumericRange(Int.MinValue, Int.MaxValue, 1 << 23)
    assert({ var i = 0; r.foreach(_ => i += 1); i } == 512)
    assert({ var i = 0; nr.foreach(_ => i += 1); i } == 512)
    assert(r.sum == Int.MinValue)
    assert(nr.sum == Int.MinValue)
    assert(r.sum(possiblyNotDefaultNumeric) == Int.MinValue)
    assert(nr.sum(possiblyNotDefaultNumeric) == Int.MinValue)
  }

  @Test
  def test_SI10086()  {
    implicit val customIntegral =
      new Numeric.IntIsIntegral with Ordering.IntOrdering {}

    val nr = NumericRange(1, 10, 1)
    assert(nr.min == 1)
    assert(nr.max == 9)

    // Also test with custom ordering.
    assert(nr.min(customIntegral.reverse) == 9)
    assert(nr.max(customIntegral.reverse) == 1)

    case class A(v: Int)

    implicit object aIsIntegral extends scala.math.Integral[A] {
      def compare(x: A, y: A): Int = x.v.compare(y.v)
      def fromInt(x: Int): A = A(x)
      def minus(x: A, y: A): A = A(x.v - y.v)
      def negate(x: A): A = A(-x.v)
      def plus(x: A, y: A): A = A(x.v + y.v)
      def times(x: A, y: A): A = A(x.v * y.v)
      def quot(x: A, y: A): A = A(x.v / y.v)
      def rem(x: A, y: A): A = A(x.v % y.v)
      def toDouble(x: A): Double = x.v.toDouble
      def toFloat(x: A): Float = x.v.toFloat
      def toInt(x: A): Int = x.v
      def toLong(x: A): Long = x.v.toLong
    }

    val r = NumericRange(A(1), A(10), A(1))
    assert(r.min == A(1))
    assert(r.max == A(9))

    // Also test with custom ordering.
    assert(r.min(aIsIntegral.reverse) == A(9))
    assert(r.max(aIsIntegral.reverse) == A(1))
  }
}
