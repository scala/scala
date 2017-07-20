// package here to be able access the package-private implementation and shutdown the pool
package scala

import org.scalacheck._
import scala.collection.parallel._

class ParCollProperties extends Properties("Parallel collections") {

  def includeAllTestsWith(support: TaskSupport, descriptor: String) {
    // parallel arrays with default task support
    include(new mutable.IntParallelArrayCheck(support, descriptor) { })

    // parallel ranges
    include(new immutable.ParallelRangeCheck(support, descriptor) { })

    // parallel immutable hash maps (tries)
    include(new immutable.IntIntParallelHashMapCheck(support, descriptor) { })

    // parallel immutable hash sets (tries)
    include(new immutable.IntParallelHashSetCheck(support, descriptor) { })

    // parallel mutable hash maps (tables)
    include(new mutable.IntIntParallelHashMapCheck(support, descriptor) { })

    // parallel ctrie
    include(new mutable.IntIntParallelConcurrentTrieMapCheck(support, descriptor) { })

    // parallel mutable hash sets (tables)
    include(new mutable.IntParallelHashSetCheck(support, descriptor) { })

    // parallel vectors
    include(new immutable.IntParallelVectorCheck(support, descriptor) { })
  }

  includeAllTestsWith(defaultTaskSupport, "defaultTaskSupport")

  val ec = scala.concurrent.ExecutionContext.fromExecutorService(java.util.concurrent.Executors.newFixedThreadPool(5))
  val ectasks = new collection.parallel.ExecutionContextTaskSupport(ec)
  includeAllTestsWith(ectasks, "ectasks")

  // no post test hooks in scalacheck, so the best we can do is:
  TestCleanup.register(ec.shutdown())
}


object TestCleanup extends Runnable {
  private val cleanups = scala.collection.mutable.Buffer[() => Unit]()
  def register(action: => Any) = synchronized {
    cleanups += {() => action}
  }
  // called by the SBT build. Scalacheck doesn't have any native support for cleanup
  override def run(): Unit = {
    cleanups.foreach(_.apply())
    cleanups.clear()
  }
}

/*
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
*/
