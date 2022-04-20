package scala.collection.mutable

import org.scalacheck.Prop._
import org.scalacheck.Properties

import scala.annotation.nowarn

object LinkedHashMapProperties extends Properties("mutable.LinkedHashMap") {
  property("-- retains order") = forAll { (lhm: LinkedHashMap[Int, Int], els: collection.immutable.Set[Int]) =>
    @nowarn("cat=deprecation")
    val actual = (lhm -- els).toList.map(_._1)
    val expected = lhm.toList.map(_._1).filterNot(els)
    actual ?= expected
  }
}
