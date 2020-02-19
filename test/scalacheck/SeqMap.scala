import scala.collection.immutable.SeqMap

import org.scalacheck._
import Arbitrary.arbitrary
import Prop._
import Gen._

import scala.collection.mutable.ListBuffer

object SeqMapTest extends Properties("SeqMap") {

  property("overrides stringPrefix") = {
    new SeqMap[Int, String] {
      def removed(key: Int): SeqMap[Int, String] = ???
      def updated[V1 >: String](key: Int, value: V1): SeqMap[Int, V1] = ???
      override def get(key: Int): Option[String] = ???
      override def iterator: Iterator[(Int, String)] = Iterator(1 -> "hi")
    }.toString == "SeqMap(<iterable>)"
  }

  property("ordering") = forAll { (m: Map[Int, Int]) =>
    val list = m.toList
    val sm = SeqMap.from(list)
    val iter = sm.iterator
    iter.toList == list
  }

  property("ordering append") = forAll(arbitrary[Map[Int,Int]] suchThat (_.size > 1))
    { (m: Map[Int, Int]) =>
      val (m1, m2) = m.splitAt(m.size / 2)
      val first = m1.toList
      val second = m2.toList
      (SeqMap.from(first) ++ second).iterator.toList == first ++ second
  }

  property("ordering on withDefault") = forAll(arbitrary[Map[Int,Int]] suchThat (_.size > 1))
    { (m: Map[Int, Int]) =>
      val (m1, m2) = m.splitAt(m.size / 2)
      val first = m1.toList
      val second = m2.toList
      (SeqMap.from(first).withDefaultValue(1) ++ second).iterator.toList == first ++ second
  }
}
