import scala.collection.immutable.OrderedMap

import org.scalacheck._
import Arbitrary.arbitrary
import Prop._
import Gen._

object OrderedMapTest extends Properties("OrderedMap") {
  property("transitive test") = {
    val x = OrderedMap(1 -> 2, 3 -> 4)
    val y = Map(1 -> 2, 3 -> 4)
    val z = OrderedMap(3 -> 4, 1 -> 2)
    x == y && y == z && x == z
  }

  property("shuffle") = forAll { (m: Map[Int, Int]) =>
    val asSeq = Seq.from(m)
    val vm1 = OrderedMap.from(asSeq)
    val vm2 = OrderedMap.from(scala.util.Random.shuffle(asSeq))
    vm1 == vm2
  }

  property("notEqual") = forAll { (m1: Map[Int, Int], m2: Map[Int, Int]) =>
    m1 != m2 ==> {
      val vm1 = OrderedMap.from(m1)
      val vm2 = OrderedMap.from(m2)
      m1 != m2
    }
  }
}
