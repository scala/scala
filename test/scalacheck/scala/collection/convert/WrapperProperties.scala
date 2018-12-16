package scala.collection.convert
import java.util
import java.util.function.Predicate

import org.scalacheck._
import org.scalacheck.Prop._

import scala.collection.immutable

object WrapperProperties extends Properties("Wrappers") {
  override def overrideParameters(p: Test.Parameters): Test.Parameters = p.withInitialSeed(42L)

  property("JSetWrapper#filterInPlace(p)") = forAll { hs: immutable.HashSet[Int] =>
    val p = (i: Int) => i % 2 == 0
    val expected: collection.Set[Int] = hs.filter(p)
    val actual: collection.Set[Int] = {
      val jset = new util.HashSet[Int]()
      hs.foreach(jset.add)
      Wrappers.JSetWrapper(jset)
    }.filterInPlace(p)
    actual ?= expected
  }
  property("MutableSetWrapper#filterInPlace(p)") = forAll { hs: immutable.HashSet[Int] =>
    val p = (i: Int) => i % 2 == 0

    val expected: (util.Set[Int], Boolean) = {
      val jset = new util.HashSet[Int]
      hs.foreach(jset.add)
      (jset, jset.removeIf(new Predicate[Int] {
        override def test(t: Int): Boolean = !p(t)
      }))
    }

    val actual: (util.Set[Int], Boolean) = {
      val jset = Wrappers.MutableSetWrapper(hs.to(collection.mutable.Set))
      (jset, jset.removeIf(new Predicate[Int] {
        override def test(t: Int): Boolean = !p(t)
      }))
    }

    actual ?= expected
  }

}