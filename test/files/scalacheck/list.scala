import org.scalacheck._
import Prop._

object Test {
  val prop_ConcatLists =
    property((l1: List[Int], l2: List[Int]) => (l1.size + l2.size) == (l1 ::: l2).size)

  val tests = List(("ConcatLists", prop_ConcatLists))
}

