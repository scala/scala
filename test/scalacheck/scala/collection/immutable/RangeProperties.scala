package scala
package collection.immutable

import org.scalacheck._

class RangeProperties extends Properties("immutable.Range") {
  import Prop._
  import RangeProperties._

  property("indexOf") = forAll { (r: Range, i: Int) =>
    r.indexOf(i) == vectorOf(r).indexOf(i)
  }

  property("indexOf with start") = forAll { (r: Range, start: Int, i: Int) =>
    r.indexOf(i, start) == r.toVector.indexOf(i, start)
  }

  property("lastIndexOf") = forAll { (r: Range, i: Int) =>
    r.lastIndexOf(i) == r.toVector.lastIndexOf(i)
  }

  property("lastIndexOf with end") = forAll { (r: Range, end: Int, i: Int) =>
    r.lastIndexOf(i, end) == r.toVector.lastIndexOf(i, end)
  }

  property("sorted") = forAll { (r: Range) =>
    r.sorted.toVector == r.toVector.sorted
  }

  property("sorted backwards") = forAll { (r: Range) =>
    r.sorted(Ordering.Int.reverse).toVector == r.toVector.sorted(Ordering.Int.reverse)
  }

  property("sameElements") = forAll { (r: Range, s: Range) =>
    r.sameElements(s) == r.toVector.sameElements(s.toVector)
  }

  property("sameElements reflexive") = forAll { (r: Range) =>
    r sameElements r
  }

  property("sameElements neg init") = forAll { (r: Range) =>
    !r.isEmpty ==> !r.sameElements(r.init)
  }

  property("sameElements neg tail") = forAll { (r: Range) =>
    !r.isEmpty ==> !r.sameElements(r.tail)
  }

  property("sameElements neg empty/non-empty") = forAll { (r1: ArbitrarilyEmptyRange, r2: ArbitrarilyEmptyRange) =>
    (r1.r.isEmpty != r2.r.isEmpty) ==> !r1.r.sameElements(r2.r)
  }

  property("sameElements with different clusivity") = forAll { (r: Range) =>
    !r.isEmpty ==> {
      val oneFurther = if (r.step > 0) 1 else -1
      if (r.isInclusive) r.sameElements(r.start until (r.end + oneFurther) by r.step)
      else r.sameElements(r.start to (r.end - oneFurther) by r.step)
    }
  }
}

object RangeProperties {
  final val MinInt = -128
  final val MaxInt = 127

  case class ArbitrarilyEmptyRange(r: Range)

  implicit val arbitrarilyEmptyRange: Arbitrary[ArbitrarilyEmptyRange] =
    Arbitrary(for {
      start <- Gen.choose(MinInt, MaxInt)
      step  <- Gen.choose(MinInt, MaxInt) filter (_ != 0)
      end   <- Gen.choose(MinInt, MaxInt)
      incl  <- Gen.oneOf(true, false)
      r      = (if (incl) start to end else start until end) by step
    } yield ArbitrarilyEmptyRange(r))

  implicit val arbitraryRange: Arbitrary[Range] =
    Arbitrary(for {
      start <- Gen.choose(MinInt, MaxInt)
      step  <- Gen.choose(MinInt, MaxInt).filter(_ != 0)
      end   <- Gen.choose(MinInt, MaxInt)
      incl  <- Gen.oneOf(true, false)
      r0     = if (incl) start to end else start until end
      s0     = if ((start < end) == (step > 0)) step else -step
      r      = r0 by s0
    } yield r)

  def vectorOf(r: Range): Vector[Int] = Vector.range(r.start, r.end, r.step)
}
