package scala.collection.mutable
import org.scalacheck._
import org.scalacheck.Prop._
import scala.collection.immutable
object LinkedHashSetProperties extends Properties("mutable.LinkedHashSet") {
  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withInitialSeed(42L)

  property("filterInPlace(p)") = forAll { v: Vector[Int] =>
    val p = (i: Int) => i % 2 == 0
    val expected: collection.Set[Int] = v.to(immutable.HashSet).filter(p)
    val actual: collection.Set[Int] = v.to(LinkedHashSet).filterInPlace(p)
    (actual ?= expected) && expected.subsetOf(actual)
  }
}