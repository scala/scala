package scala.collection.mutable
import org.scalacheck._
import org.scalacheck.Prop._
import Gen._

object BitSetProperties extends Properties("mutable.BitSet") {
  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(500)
      .withInitialSeed(42L)

  // the top of the range shouldn't be too high, else we may not get enough overlap
  implicit val arbitraryBitSet: Arbitrary[BitSet] = Arbitrary(
    oneOf(
      const(BitSet()),
      oneOf(0 to 100).map(i => BitSet(i)),
      listOfN(200, oneOf(0 to 10000)).map(_.to(BitSet))
    )
  )

  /** the max number to include in generated BitSets */
  val highestNum = 15000

  implicit val nonNegativeRange: Arbitrary[Range] = Arbitrary(
    for {
      start <- chooseNum(0, highestNum)
      end <- chooseNum(start, highestNum)
      by <- oneOf(-1, 1, 5)
    } yield start to end by by
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


  property("addAll(Range)") = forAll{ (bs: BitSet, range: Range) =>
    val bsClone1 = bs.clone()
    val bsClone2 = bs.clone()
    range.foreach(bsClone2.add)
    bsClone1.addAll(range) ?= bsClone2
  }

  property("subsetOf(BitSet) equivalent to slow implementation") = forAll{ (left: BitSet, right: BitSet) =>
    (Prop(left.subsetOf(right)) ==> left.forall(right)) &&
      (Prop(left.forall(right)) ==> Prop(left.subsetOf(right)))
  }

  property("left subsetOf (left union right) && right subsetOf (left union right)") = forAll{ (left: BitSet, right: BitSet) =>
    val leftUnionRight = left concat right
    left.subsetOf(leftUnionRight) && right.subsetOf(leftUnionRight)
  }

  property("range") = forAll{ (bs: BitSet, range: Range) =>
    bs.range(range.start, range.end) ?= bs.filter(b => b >= range.start && b < range.end)
  }

}
