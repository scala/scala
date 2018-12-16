package scala.collection.mutable
import org.scalacheck._
import org.scalacheck.Prop._
import scala.collection.immutable
object HashSetProperties extends Properties("mutable.HashSet") {
  override def overrideParameters(p: Test.Parameters): Test.Parameters =
      p.withInitialSeed(42L)

  property("filterInPlace(p)") = forAll { hs: immutable.HashSet[Int] =>
    val p = (i: Int) => i % 2 == 0
    val expected: collection.Set[Int] = hs.filter(p)
    val actual: collection.Set[Int] = hs.to(HashSet).filterInPlace(p)
    actual ?= expected
  }
}