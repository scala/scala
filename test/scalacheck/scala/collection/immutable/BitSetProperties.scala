package scala.collection.immutable
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Prop.BooleanOperators
import Gen._
object BitSetProperties extends Properties("immutable.BitSet") {
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
}