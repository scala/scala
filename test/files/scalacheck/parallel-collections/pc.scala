



import org.scalacheck._
import scala.collection.parallel._


class ParCollProperties extends Properties("Parallel collections") {
  // parallel arrays
  include(mutable.IntParallelArrayCheck)
}


object Test {
  def main(args: Array[String]) {
    val results = org.scalacheck.Test.checkProperties(new ParCollProperties)
    if (!results.forall(_._2.passed)) println(results)
  }
}
