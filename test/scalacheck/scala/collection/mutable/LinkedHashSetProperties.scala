package scala.collection.mutable

import org.scalacheck.Prop._
import org.scalacheck.Properties

import scala.annotation.nowarn

object LinkedHashSetProperties extends Properties("mutable.LinkedHashSet") {
  property("-- retains order") = forAll { (lhs: LinkedHashSet[Int], els: collection.immutable.Set[Int]) =>
    @nowarn("cat=deprecation")
    val actual = (lhs -- els).toList
    val expected = lhs.toList.filterNot(els)
    actual ?= expected
  }
}
