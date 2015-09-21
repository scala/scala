import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

class Counter(r: Range) {
  var cnt = 0L
  var last: Option[Int] = None
  val str = "Range["+r.start+", "+r.end+", "+r.step+(if (r.isInclusive) "]" else ")")
  def apply(x: Int) = {
    cnt += 1L
    if (cnt % 500000000L == 0L) {
      println("Working: %s %d %d" format (str, cnt, x))
    }
    if (cnt > (Int.MaxValue.toLong + 1) * 2) {
      val msg = "Count exceeds maximum possible for an Int Range: %s" format str
      println(msg) // exception is likely to be eaten by an out of memory error
      sys error msg
    }
    if ((r.step > 0 && last.exists(_ > x)) || (r.step < 0 && last.exists(_ < x))) {
      val msg = "Range %s wrapped: %d %s" format (str, x, last.toString)
      println(msg) // exception is likely to be eaten by an out of memory error
      sys error msg
    }
    last = Some(x)
  }
}

abstract class RangeTest(kind: String) extends Properties("Range "+kind) {
  def myGen: Gen[Range]

  def genReasonableSizeRange = oneOf(genArbitraryRange, genBoundaryRange)

  def genArbitraryRange = for {
    start <- choose(Int.MinValue, Int.MaxValue)
    end <- choose(Int.MinValue, Int.MaxValue)
    step <- choose(-Int.MaxValue, Int.MaxValue)
  } yield Range(start, end, if (step == 0) 100 else step)

  def genBoundaryRange = for {
    boundary <- oneOf(Int.MinValue, -1, 0, 1, Int.MaxValue)
    isStart <- arbitrary[Boolean]
    size <- choose(1, 100)
    step <- choose(1, 101)
  } yield {
    val signum = if (boundary == 0) 1 else boundary.signum
    if (isStart) Range(boundary, boundary - size * boundary.signum, - step * signum)
    else         Range(boundary - size * boundary.signum, boundary, step * signum)
  }


  def genSmallRange = for {
    start <- choose(-100, 100)
    end <- choose(-100, 100)
    step <- choose(1, 1)
  } yield if (start < end) Range(start, end, step) else Range(start, end, -step)

  def genRangeByOne = oneOf(genRangeOpenByOne, genRangeClosedByOne)

  def genRangeOpenByOne = for {
    r <- oneOf(genSmallRange, genBoundaryRange)
    if (r.end.toLong - r.start.toLong).abs <= 10000000L
  } yield if (r.start < r.end) Range(r.start, r.end) else Range(r.end, r.start)

  def genRangeClosedByOne = for (r <- genRangeOpenByOne) yield r.start to r.end

  def str(r: Range) = "Range["+r.start+", "+r.end+", "+r.step+(if (r.isInclusive) "]" else ")")

  def expectedSize(r: Range): Long = if (r.isInclusive) {
    (r.end.toLong - r.start.toLong < 0, r.step < 0) match {
      case (true, true) | (false, false) => (r.end.toLong - r.start.toLong).abs / r.step.abs.toLong + 1L
      case _ => if (r.start == r.end) 1L else 0L
    }
  } else {
    (r.end.toLong - r.start.toLong < 0, r.step < 0) match {
      case (true, true) | (false, false) => (
        (r.end.toLong - r.start.toLong).abs / r.step.abs.toLong
        + (if ((r.end.toLong - r.start.toLong).abs % r.step.abs.toLong > 0L) 1L else 0L)
      )
      case _ => 0L
    }
  }

  def within(r: Range, x: Int) = if (r.step > 0)
    r.start <= x && (if (r.isInclusive) x <= r.end else x < r.end)
  else
    r.start >= x && (if (r.isInclusive) x >= r.end else x > r.end)

  def multiple(r: Range, x: Int) = (x.toLong - r.start) % r.step == 0

  property("foreach.step") = forAllNoShrink(myGen) { r =>
//    println("foreach.step "+str(r))
    var allValid = true
    val cnt = new Counter(r)
//    println("--------------------")
//    println(r)
    r foreach { x => cnt(x)
//      println(x + ", " + (x - r.start) + ", " + (x.toLong - r.start) + ", " + ((x.toLong - r.start) % r.step))
      allValid &&= multiple(r, x)
    }
    allValid :| str(r)
  }

  property("foreach.inside.range") = forAll(myGen) { r =>
//    println("foreach.inside.range "+str(r))
    var allValid = true
    var last: Option[Int] = None
    val cnt = new Counter(r)
    r foreach { x => cnt(x)
      allValid &&= within(r, x)
    }
    allValid :| str(r)
  }

  property("foreach.visited.size") = forAll(myGen) { r =>
//    println("foreach.visited.size "+str(r))
    var visited = 0L
    val cnt = new Counter(r)
    r foreach { x => cnt(x)
      visited += 1L
    }
//    println("----------")
//    println(str(r))
//    println("size: " + r.size)
//    println("expected: " + expectedSize(r))
//    println("visited: " + visited)
    (visited == expectedSize(r)) :| str(r)
  }

  property("sum") = forAll(myGen) { r =>
//    println("----------")
//    println("sum "+str(r))
    val rSum = r.sum
    val expected = r.length match {
      case 0 => 0
      case 1 => r.head
      case x if x < 1000 => 
        // Explicit sum, to guard against having the same mistake in both the
        // range implementation and test implementation of sum formula.
        // (Yes, this happened before.)
        var i = r.head
        var s = 0L
        var n = x
        while (n > 0) {
          s += i
          i += r.step
          n -= 1
        }
        s.toInt
      case _ =>
        // Make sure head + last doesn't overflow!
        ((r.head.toLong + r.last) * r.length  / 2).toInt
    }
//   println("size: " + r.length)
//   println("expected: " + expected)
//   println("obtained: " + rSum)

   (rSum == expected) :| str(r)
  }

/* checks that sum respects custom Numeric */
  property("sumCustomNumeric") = forAll(myGen) { r =>
    val mod = 65536
    object mynum extends Numeric[Int] {
        def plus(x: Int, y: Int): Int = (x + y) % mod
        override def zero = 0

        def fromInt(x: Int): Int = ???
        def minus(x: Int, y: Int): Int = ???
        def negate(x: Int): Int = ???
        def times(x: Int, y: Int): Int = ???
        def toDouble(x: Int): Double = ???
        def toFloat(x: Int): Float = ???
        def toInt(x: Int): Int = ((x % mod) + mod * 2) % mod
        def toLong(x: Int): Long = ???
        def compare(x: Int, y: Int): Int = ???
      }

    val rSum = r.sum(mynum)
    val expected = mynum.toInt(r.sum)

    (rSum == expected) :| str(r)
  }


  property("length") = forAll(myGen suchThat (r => expectedSize(r).toInt == expectedSize(r))) { r =>
//    println("length "+str(r))
    (r.length == expectedSize(r)) :| str(r)
  }

  property("isEmpty") = forAll(myGen suchThat (r => expectedSize(r).toInt == expectedSize(r))) { r =>
//    println("isEmpty "+str(r))
    (r.isEmpty == (expectedSize(r) == 0L)) :| str(r)
  }

  property("contains") = forAll(myGen, arbInt.arbitrary) { (r, x) =>
//    println("contains "+str(r))
//    println("----------------")
//    println(str(r))
//    println(x)
//    println("within: " + within(r, x))
//    println("multiple: " + multiple(r, x))
//    println("contains: " + r.contains(x))
    ((within(r, x) && multiple(r, x)) == r.contains(x)) :| str(r)+": "+x
  }

  property("take") = forAll(myGen suchThat (r => expectedSize(r).toInt == expectedSize(r)), arbInt.arbitrary) { (r, x) =>
//    println("take "+str(r))
    val t = r take x
    (t.size == (0 max x min r.size) && t.start == r.start && t.step == r.step) :| str(r)+" / "+str(t)+": "+x
  }

  property("init") = forAll(myGen suchThat (r => expectedSize(r).toInt == expectedSize(r))) { r =>
//    println("init "+str(r))
    (r.size == 0) || {
      val t = r.init
      (t.size + 1 == r.size) && (t.isEmpty || t.head == r.head)
    }
  }

  property("takeWhile") = forAll(myGen suchThat (r => expectedSize(r).toInt == expectedSize(r)), arbInt.arbitrary) { (r, x) =>
//    println("takeWhile "+str(r))
    val t = (if (r.step > 0) r takeWhile (_ <= x) else r takeWhile(_ >= x))
    if (r.size == 0) {
      (t.size == 0) :| str(r)+" / "+str(t)+": "+x
    } else {
      val t2 = (if (r.step > 0) Range(r.start, x min r.last, r.step).inclusive else Range(r.start, x max r.last, r.step).inclusive)
      (t.start == r.start && t.size == t2.size && t.step == r.step) :| str(r)+" / "+str(t)+" / "+str(t2)+": "+x
    }
  }

  property("reverse.toSet.equal") = forAll(myGen) { r =>
//    println("reverse.toSet.equal "+str(r))
    val reversed = r.reverse
    val aresame = r.toSet == reversed.toSet
    if (!aresame) {
      println(str(r))
      println(r)
      println(reversed)
      println(r.toSet)
      println(reversed.toSet)
    }
    aresame :| str(r)
  }
}

object NormalRangeTest extends RangeTest("normal") {
  override def myGen = genReasonableSizeRange
  def genOne = for {
    start <- arbitrary[Int]
    end <- arbitrary[Int]
    if (start.toLong - end.toLong).abs < Int.MaxValue.toLong
  } yield Range(start, end, if (start < end) 1 else - 1)
  property("by 1.size + 1 == inclusive.size") = forAll(genOne) { r =>
    (r.size + 1 == r.inclusive.size) :| str(r)
  }
}

object InclusiveRangeTest extends RangeTest("inclusive") {
  override def myGen = for (r <- genReasonableSizeRange) yield r.inclusive
}

object ByOneRangeTest extends RangeTest("byOne") {
  override def myGen = genRangeByOne
}

object InclusiveByOneRangeTest extends RangeTest("inclusiveByOne") {
  override def myGen = for (r <- genRangeByOne) yield r.inclusive
}

object SmallValuesRange extends RangeTest("smallValues") {
  override def myGen = genSmallRange
}

object TooLargeRange extends Properties("Too Large Range") {
  val genTooLargeStart = for {
    start <- choose(-Int.MinValue, 0)
  } yield start

  property("Too large range throws exception") = forAll(genTooLargeStart) { start =>
    try   {
      val r = Range.inclusive(start, Int.MaxValue, 1)
      val l = r.length
      println("how here? length = " + l + ", r = " + r.toString)
      false
    }
    catch { case _: IllegalArgumentException => true }
  }
}

object Test extends Properties("Range") {
  import org.scalacheck.{ Test => STest }

  include(NormalRangeTest)
  include(InclusiveRangeTest)
  include(ByOneRangeTest)
  include(InclusiveByOneRangeTest)
  include(TooLargeRange)
}

/* Mini-benchmark
def testRange(i: Int, j: Int, k: Int) = {
  var count = 0
  for {
    vi <- 0 to i
    vj <- 0 to j
    vk <- 0 to k
  } { count += 1 }
}

testRange(10, 1000, 10000)
testRange(10000, 1000, 10)
*/

