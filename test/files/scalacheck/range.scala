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
    if (cnt > (Int.MaxValue.toLong + 1) * 2)
      error("Count exceeds maximum possible for an Int Range")
    if ((r.step > 0 && last.exists(_ > x)) || (r.step < 0 && last.exists(_ < x)))
      error("Range wrapped: %d %s" format (x, last.toString))
    last = Some(x)
  }
}

abstract class RangeTest(kind: String) extends Properties("Range "+kind) {
  def myGen: Gen[Range]

  val genRange = for {
    start <- arbitrary[Int]
    end <- arbitrary[Int]
    step <- Gen.choose(1, (start - end).abs + 1)
  } yield if (start < end) Range(start, end, step) else Range(start, end, -step)

  val genReasonableSizeRange = for {
    start <- choose(-Int.MinValue, Int.MaxValue)
    end <- choose(-Int.MinValue, Int.MaxValue)
    step <- choose(-Int.MaxValue, Int.MaxValue)
  } yield Range(start, end, if (step == 0) 100 else step)

  val genSmallRange = for {
    start <- choose(-100, 100)
    end <- choose(-100, 100)
    step <- choose(1, 1)
  } yield if (start < end) Range(start, end, step) else Range(start, end, -step)

  val genRangeByOne = for {
    start <- arbitrary[Int]
    end <- arbitrary[Int]
    if (end.toLong - start.toLong).abs <= 10000000L
  } yield if (start < end) Range(start, end) else Range(end, start)

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

  property("foreach.step") = forAll(myGen) { r =>
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
    var allValid = true
    var last: Option[Int] = None
    val cnt = new Counter(r)
    r foreach { x => cnt(x)
      allValid &&= within(r, x)
    }
    allValid :| str(r)
  }

  property("foreach.visited.size") = forAll(myGen) { r =>
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

  property("length") = forAll(myGen suchThat (r => expectedSize(r).toInt == expectedSize(r))) { r =>
    (r.length == expectedSize(r)) :| str(r)
  }

  property("isEmpty") = forAll(myGen suchThat (r => expectedSize(r).toInt == expectedSize(r))) { r =>
    (r.isEmpty == (expectedSize(r) == 0L)) :| str(r)
  }

  property("contains") = forAll(myGen, arbInt.arbitrary) { (r, x) =>
//    println("----------------")
//    println(str(r))
//    println(x)
//    println("within: " + within(r, x))
//    println("multiple: " + multiple(r, x))
//    println("contains: " + r.contains(x))
    ((within(r, x) && multiple(r, x)) == r.contains(x)) :| str(r)+": "+x
  }

  property("take") = forAll(myGen suchThat (r => expectedSize(r).toInt == expectedSize(r)), arbInt.arbitrary) { (r, x) =>
    val t = r take x
    (t.size == (0 max x min r.size) && t.start == r.start && t.step == r.step) :| str(r)+" / "+str(t)+": "+x
  }

  property("init") = forAll(myGen suchThat (r => expectedSize(r).toInt == expectedSize(r))) { r =>
    (r.size == 0) || {
      val t = r.init
      (t.size + 1 == r.size) && (t.isEmpty || t.head == r.head)
    }
  }

  property("takeWhile") = forAll(myGen suchThat (r => expectedSize(r).toInt == expectedSize(r)), arbInt.arbitrary) { (r, x) =>
    val t = (if (r.step > 0) r takeWhile (_ <= x) else r takeWhile(_ >= x))
    if (r.size == 0) {
      (t.size == 0) :| str(r)+" / "+str(t)+": "+x
    } else {
      val t2 = (if (r.step > 0) Range(r.start, x min r.last, r.step).inclusive else Range(r.start, x max r.last, r.step).inclusive)
      (t.start == r.start && t.size == t2.size && t.step == r.step) :| str(r)+" / "+str(t)+" / "+str(t2)+": "+x
    }
  }

  property("reverse.toSet.equal") = forAll(myGen) { r =>
    val reversed = r.reverse
    val aresame = r.toSet == reversed.toSet
    if (!aresame) {
      println(str(r))
      println(r)
      println(reversed)
      println(r.toSet)
      println(reversed.toSet)
    }
    aresame
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
  override def myGen = genSmallRange
}

object InclusiveByOneRangeTest extends RangeTest("inclusiveByOne") {
  override def myGen = for (r <- genSmallRange) yield r.inclusive
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
      println("how here? r = " + r.toString)
      false
    }
    catch { case _: IllegalArgumentException => true }
  }
}

object Test extends Properties("Range") {
  import org.scalacheck.{ Test => STest }

  List(NormalRangeTest, InclusiveRangeTest, ByOneRangeTest, InclusiveByOneRangeTest, TooLargeRange) foreach { ps =>
    STest.checkProperties(STest.Params(testCallback = ConsoleReporter(0)), ps)
  }
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

