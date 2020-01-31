
package scala.collection.convert

import org.scalacheck.{Properties, Test}
import org.scalacheck.Prop._

import scala.collection.immutable


object WrapperProperties extends Properties("Wrappers") {

  property("JSetWrapper#filterInPlace(p)") = forAll { (hs: immutable.HashSet[Int], p: Int => Boolean) =>
    val expected: collection.Set[Int] = hs.filter(p)
    val actual: collection.Set[Int] = {
      val jset = new java.util.HashSet[Int]()
      hs.foreach(jset.add)
      JavaCollectionWrappers.JSetWrapper(jset)
    }.filterInPlace(p)
    actual ?= expected
  }

}
