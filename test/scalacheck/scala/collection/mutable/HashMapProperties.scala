package scala.collection.mutable

import org.scalacheck._
import org.scalacheck.Prop._

import scala.collection.immutable

object HashMapProperties extends Properties("mutable.HashMap") {

  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(500)
      .withInitialSeed(42L)

  property("addAll(immutable.HashMap)") = forAll { (left: immutable.HashMap[Int, Int], right: immutable.HashMap[Int, Int]) =>
    val expected: collection.Map[Int, Int] = left concat right
    val actual: collection.Map[Int, Int] = left.to(HashMap).addAll(right)

    actual ?= expected
  }
  property("addAll(mutable.HashMap)") = forAll { (left: immutable.HashMap[Int, Int], right: immutable.HashMap[Int, Int]) =>
    val expected: collection.Map[Int, Int] = left concat right
    val actual: collection.Map[Int, Int] = left.to(HashMap).addAll(right.to(HashMap.mapFactory))
    actual ?= expected
  }
  property("addAll(Vector)") = forAll { (left: immutable.HashMap[Int, Int], right: Vector[(Int, Int)]) =>
    val expected: collection.Map[Int, Int] = left concat right
    val actual: collection.Map[Int, Int] = left.to(HashMap).addAll(right)
    actual ?= expected
  }
  property("subtractAll(immutable.HashSet)") = forAll { (left: immutable.HashMap[Int, Int], right: immutable.HashSet[Int]) =>
    val expected: collection.Map[Int, Int] = left -- right
    val actual: collection.Map[Int, Int] = left.to(HashMap).subtractAll(right.to(HashSet))
    actual ?= expected
  }
  property("subtractAll(mutable.HashSet)") = forAll { (left: immutable.HashMap[Int, Int], right: immutable.HashSet[Int]) =>
    val expected: collection.Map[Int, Int] = left -- right
    val actual: collection.Map[Int, Int] = left.to(HashMap).subtractAll(right.to(HashSet))
    actual ?= expected
  }
  property("subtractAll(Vector)") = forAll { (left: immutable.HashMap[Int, Int], right: Vector[Int]) =>
    val expected: collection.Map[Int, Int] = left -- right
    val actual: collection.Map[Int, Int] = left.to(HashMap).subtractAll(right.to(Vector))
    actual ?= expected
  }
}