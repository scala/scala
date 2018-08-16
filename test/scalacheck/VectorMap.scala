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
}
