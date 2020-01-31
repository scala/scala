package scala.collection.immutable

import org.scalacheck._
import org.scalacheck.Prop._
import Gen._

object BitSetProperties extends Properties("immutable.BitSet") {

  // the top of the range shouldn't be too high, else we may not get enough overlap
  implicit val arbitraryBitSet: Arbitrary[BitSet] = Arbitrary(
    oneOf(
      const(BitSet()),
      oneOf(0 to 100).map(i => BitSet(i)),
      listOfN(200, oneOf(0 to 10000)).map(_.to(BitSet))
    )
  )

  property("min") = forAll { (bs: BitSet) =>
    bs.nonEmpty ==> (bs.min ?= bs.toList.min)
  }
  property("min reverse") = forAll { (bs: BitSet) =>
    bs.nonEmpty ==> (bs.min(Ordering.Int.reverse) ?= bs.toList.min(Ordering.Int.reverse))
  }

  property("max") = forAll { (bs: BitSet) =>
    bs.nonEmpty ==> (bs.max ?= bs.toList.max)
  }

  property("max reverse") = forAll { (bs: BitSet) =>
    bs.nonEmpty ==> (bs.max(Ordering.Int.reverse) ?= bs.toList.max(Ordering.Int.reverse))
  }

  property("diff bitSet") = forAll { (left: BitSet, right: BitSet) =>
    (left.diff(right): Set[Int]) ?= left.to(HashSet).diff(right.to(HashSet))
  }
  property("diff hashSet") = forAll { (left: BitSet, right: BitSet) =>
    (left.diff(right.to(HashSet)) : Set[Int]) ?= left.to(HashSet).diff(right.to(HashSet))
  }

  property("filter") = forAll { (bs: BitSet) =>
    bs.filter(_ % 2 == 0) ?= bs.toList.filter(_ % 2 == 0).to(BitSet)
  }
  property("filterNot") = forAll { (bs: BitSet) =>
    bs.filterNot(_ % 2 == 0) ?= bs.toList.filterNot(_ % 2 == 0).to(BitSet)
  }

  property("partition") = forAll { (bs: BitSet) =>
    val p = (i: Int) => i % 2 == 0
    val (left, right) = bs.partition(p)
    (left ?= bs.filter(p)) && (right ?= bs.filterNot(p))
  }

  property("iteratorFrom") = forAll(
    listOfN(200, oneOf(0 to 10000)).map(_.to(Set)),
    Gen.chooseNum[Int](0, 10000)) { (xs: Set[Int], start: Int) =>
    val bs = xs.to(BitSet)
    bs.iteratorFrom(start).to(Set) ?= xs.filter(_ >= start)
  }
}
