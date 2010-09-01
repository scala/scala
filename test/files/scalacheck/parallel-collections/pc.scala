



import org.scalacheck._
import scala.collection.parallel._


object ParCollProperties extends Properties("Parallel collections") {
  include(mutable.IntParallelArrayCheck)
  include(immutable.ParallelRangeCheck)
}


object Test {
  def main(args: Array[String]) {
    val results = org.scalacheck.Test.checkProperties(ParCollProperties)
    if (!results.forall(_._2.passed)) println(results)
  }
}
