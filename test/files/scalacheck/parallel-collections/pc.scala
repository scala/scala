



import org.scalacheck._

import scala.collection.parallel._


class ParCollProperties extends Properties("Parallel collections") {
  /*   Collections   */
  
  // parallel arrays
  include(mutable.IntParallelArrayCheck)
  
  // parallel ranges
  include(immutable.ParallelRangeCheck)
  
  // parallel immutable hash maps (tries)
  include(immutable.IntIntParallelHashMapCheck)
  
  // parallel immutable hash sets (tries)
  include(immutable.IntParallelHashSetCheck)
  
  // parallel mutable hash maps (tables)
  include(mutable.IntIntParallelHashMapCheck)
  
  // parallel mutable hash sets (tables)
  include(mutable.IntParallelHashSetCheck)
  
  // parallel vectors
  include(immutable.IntParallelVectorCheck)
}


object Test {
  def main(args: Array[String]) {
    val pc = new ParCollProperties
    org.scalacheck.Test.checkProperties(
      org.scalacheck.Test.Params(
        rng = new java.util.Random(5134L),
        testCallback = new ConsoleReporter(0),
        workers = 1,
        minSize = 0,
        maxSize = 4000,
        minSuccessfulTests = 5
      ),
      pc
    )
  }
}
