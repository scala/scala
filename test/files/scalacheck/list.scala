import org.scalacheck._
import Prop._
import Gen._

object Test extends Properties("List") {
  def sorted(xs: List[Int]) = xs sortWith (_ < _)

  property("concat size") = forAll { (l1: List[Int], l2: List[Int]) => (l1.size + l2.size) == (l1 ::: l2).size }
  property("reverse") = forAll { (l1: List[Int]) => l1.reverse.reverse == l1 }
  property("toSet") = forAll { (l1: List[Int]) => sorted(l1.toSet.toList) sameElements sorted(l1).distinct }
  // property("flatten") = forAll { (xxs: List[List[Int]]) => xxs.flatten.length == (xxs map (_.length) sum) }
  property("startsWith/take") = forAll { (xs: List[Int], count: Int) => xs startsWith (xs take count) }
  property("endsWith/takeRight") = forAll { (xs: List[Int], count: Int) => xs endsWith (xs takeRight count) }
  property("fill") = forAll(choose(1, 100)) { count =>
    forAll { (x: Int) =>
      val xs = List.fill(count)(x)
      (xs.length == count) && (xs.distinct == List(x))
    }
  }
}

