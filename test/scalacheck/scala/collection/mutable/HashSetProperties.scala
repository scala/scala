package scala.collection.mutable
import org.scalacheck._
import org.scalacheck.Prop._
import scala.collection.immutable

object HashSetProperties extends Properties("mutable.HashSet") {

  property("filterInPlace(p)") = forAll { (hs: immutable.HashSet[Int], p: Int => Boolean) =>
    val expected: collection.Set[Int] = hs.filter(p)
    val actual: collection.Set[Int] = hs.to(HashSet).filterInPlace(p)
    actual ?= expected
  }
  property("filterInPlace(_ => true)") = forAll { hs: immutable.HashSet[Int] =>
    val actual: collection.Set[Int] = hs.to(HashSet).filterInPlace(_ => true)
    actual ?= hs
  }
  property("filterInPlace(_ => false)") = forAll { hs: immutable.HashSet[Int] =>
    val actual: collection.Set[Int] = hs.to(HashSet).filterInPlace(_ => false)
    actual ?= Set.empty
  }
  property("addAll(immutable.HashSet)") = forAll { (left: immutable.HashSet[Int], right: immutable.HashSet[Int]) =>
    val expected: collection.Set[Int] = left union right
    val actual: collection.Set[Int] = left.to(HashSet).addAll(right)
    actual ?= expected
  }
  property("addAll(mutable.HashSet)") = forAll { (left: immutable.HashSet[Int], right: immutable.HashSet[Int]) =>
    val expected: collection.Set[Int] = left union right
    val actual: collection.Set[Int] = left.to(HashSet).addAll(right.to(HashSet))
    actual ?= expected
  }
  property("addAll(Vector)") = forAll { (left: immutable.HashSet[Int], right: immutable.HashSet[Int]) =>
    val expected: collection.Set[Int] = left union right
    val actual: collection.Set[Int] = left.to(HashSet).addAll(right.to(Vector))
    actual ?= expected
  }
  property("subtractAll(immutable.HashSet)") = forAll { (left: immutable.HashSet[Int], right: immutable.HashSet[Int]) =>
    val expected: collection.Set[Int] = left diff right
    val actual: collection.Set[Int] = left.to(HashSet).subtractAll(right)
    actual ?= expected
  }
  property("subtractAll(immutable.HashSet)") = forAll { (left: immutable.HashSet[Int], right: immutable.HashSet[Int]) =>
    val expected: collection.Set[Int] = left diff right
    val actual: collection.Set[Int] = left.to(HashSet).subtractAll(right.to(HashSet))
    actual ?= expected
  }
  property("subtractAll(immutable.Vector)") = forAll { (left: immutable.HashSet[Int], right: immutable.HashSet[Int]) =>
    val expected: collection.Set[Int] = left diff right
    val actual: collection.Set[Int] = left.to(HashSet).subtractAll(right.to(Vector))
    actual ?= expected
  }
}