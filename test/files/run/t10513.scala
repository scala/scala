//package scala.concurrent
import scala.concurrent._
import duration._

import scala.util.{Random, Try}
import ExecutionContext.Implicits.global

/** This test uses recursive calls to Future.flatMap to create arrays whose
  * combined size is slightly greater than the JVM heap size. A previous
  * implementation of Future.flatMap would retain references to each array,
  * resulting in a speedy OutOfMemoryError. Now, each array should be freed soon
  * after it is created and the test should complete without problems.
  */
object Test {

  def main(args: Array[String]) {
    val arrSz = 50 * 10000
    val numFutures = 4000

    val rng = new Random()
    val longStandingPromise = Promise[Nothing]

    val futures = List.tabulate(numFutures) { i =>
      val arr = Array.tabulate(arrSz)(identity)
      val idx = rng.nextInt(arrSz)
      val f1 = Future {
        arr
      }
      val f2 = Future.firstCompletedOf(List(longStandingPromise.future, f1))
      f2.map(arr => arr(idx))
    }
    val fSeq = Future.sequence(futures)
    val finalF = fSeq.map(_.sum)
    val res = Await.result(finalF, 2.minutes)
  }
}
