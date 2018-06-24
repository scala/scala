import scala.collection.immutable.LinkedMap

import org.scalacheck._
import Arbitrary.arbitrary
import Prop._
import Gen._

import scala.collection.mutable.ListBuffer

object LinkedMapTest extends Properties("LinkedMap") {

  property("ordering") = forAll { (m: Map[Int, Int]) =>
    val list = m.toList
    val lm = LinkedMap.from(list)
    val iter = lm.iterator
    iter.toList == list
  }

  property("ordering append") = forAll(arbitrary[Map[Int,Int]] suchThat (_.size > 1))
    { (m: Map[Int, Int]) =>
      val list = m.toList
      val first = list.init
      val elem = list.last
      val lm = LinkedMap.from(first)
      (lm + elem).iterator.toList == first :+ elem
    }
}
