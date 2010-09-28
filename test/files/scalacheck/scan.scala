import org.scalacheck._
import Prop._
import Gen._


object Test extends Properties("TraversableLike.scanLeft") {
  property("scanLeft") = forAll { (xs: List[Int], z: Int) => {
    val sums = xs.scanLeft(z)(_ + _)
    (xs.size == 0) || sums.zip(sums.tail).map(x => x._2 - x._1) == xs
  }}
}






