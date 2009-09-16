import org.scalacheck._
import Prop._

object Test extends Properties("List") {
  property("concat") = forAll { (l1: List[Int], l2: List[Int]) => (l1.size + l2.size) == (l1 ::: l2).size }
}

