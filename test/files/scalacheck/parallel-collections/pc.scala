



import org.scalacheck._

import scala.collection.parallel._


class ParCollProperties extends Properties("Parallel collections") {
  /*   Collections   */

  // parallel arrays
  //include(mutable.IntParallelArrayCheck)

  // parallel ranges
  //include(immutable.ParallelRangeCheck)

  // parallel immutable hash maps (tries)
  include(immutable.IntIntParallelHashMapCheck)

  // parallel immutable hash sets (tries)

  // parallel mutable hash maps (tables)


  /*   Views   */

  // parallel array views

  // parallel immutable hash map views

  // parallel mutable hash map views
}


object Test {
  def main(args: Array[String]) {
    val pc = new ParCollProperties
    org.scalacheck.Test.checkProperties(
      org.scalacheck.Test.Params(
        rng = new java.util.Random(5134L),
        testCallback = new ConsoleReporter(0),
        workers = 1
      ),
      pc
    )
  }
}
