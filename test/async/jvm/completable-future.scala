//> using options -Xasync

import java.util.concurrent._
import scala.tools.partest.async.CompletableFutureAwait._

object Test {
  val pool = java.util.concurrent.Executors.newWorkStealingPool()
  def f1 = CompletableFuture.supplyAsync(() => 1, pool)

  def main(args: Array[String]): Unit = {
    val f = async(pool) {
      var i = 0
      while (i < 100) {
        i += await(f1)
      }
      i
    }
    val result = f.get()
    assert(result == 100, result)
  }
}