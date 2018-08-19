import scala.collection.immutable.VectorMap

import org.scalacheck._
import Arbitrary.arbitrary
import Prop._
import Gen._

object VectorMapTest extends Properties("VectorMap") {
  property("transitive test") = {
    val x = VectorMap(1 -> 2, 3 -> 4)
    val y = Map(1 -> 2, 3 -> 4)
    val z = VectorMap(3 -> 4, 1 -> 2)
    x == y && y == z && x == z
  }

  property("shuffle") = forAll { (m: Map[Int, Int]) =>
    val asSeq = Seq.from(m)
    val vm1 = VectorMap.from(asSeq)
    val vm2 = VectorMap.from(scala.util.Random.shuffle(asSeq))
    vm1 == vm2
  }

  property("notEqual") = forAll { (m1: Map[Int, Int], m2: Map[Int, Int]) =>
    m1 != m2 ==> {
      val vm1 = VectorMap.from(m1)
      val vm2 = VectorMap.from(m2)
      m1 != m2
    }
  }
}
