package scala.collection.mutable

import org.scalacheck._
import org.scalacheck.Prop._
import Gen._
object BitSetProperties extends Properties("mutable.BitSet") {

  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(1000)
      .withInitialSeed(42L)

  // the top of the range shouldn't be too high, else we may not get enough overlap
  implicit val arbitraryBitSet: Arbitrary[BitSet] = Arbitrary(
    oneOf(
      const(BitSet()),
      oneOf(0 to 100).map(i => BitSet(i)),
      listOfN(200, oneOf(0 to 10000)).map(_.to(BitSet))
    )
  )

  property("diff") = forAll { (left: BitSet, right: BitSet) =>
    (left.diff(right): Set[Int]) ?= left.to(HashSet).diff(right.to(HashSet))
  }
  property("diff hashSet") = forAll { (left: BitSet, right: BitSet) =>
    (left.diff(right.to(HashSet)): Set[Int]) ?= left.to(HashSet).diff(right.to(HashSet))
  }
  property("filter") = forAll { (bs: BitSet) =>
    bs.filter(_ % 2 == 0) ?= bs.toList.filter(_ % 2 == 0).to(BitSet)
  }
  property("filterNot") = forAll { (bs: BitSet) =>
    bs.filterNot(_ % 2 == 0) ?= bs.toList.filterNot(_ % 2 == 0).to(BitSet)
  }
  property("filterInPlace") = forAll { (bs: BitSet) =>
    val list = bs.toList
    bs.filterInPlace(_ % 2 == 0) ?= list.filter(_ % 2 == 0).to(BitSet)
  }
  property("partition") = forAll { (bs: BitSet) =>
    val p = (i: Int) => i % 2 == 0
    val (left, right) = bs.partition(p)
    (left ?= bs.filter(p)) && (right ?= bs.filterNot(p))
  }
}
