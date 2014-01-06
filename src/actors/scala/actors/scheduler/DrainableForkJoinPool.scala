package scala.actors
package scheduler

import java.util.Collection
import scala.concurrent.forkjoin.{ForkJoinPool, ForkJoinTask}

private class DrainableForkJoinPool(parallelism: Int, maxPoolSize: Int) extends ForkJoinPool(parallelism, ForkJoinPool.defaultForkJoinWorkerThreadFactory, null, true) {

  override def drainTasksTo(c: Collection[ _ >: ForkJoinTask[_]]): Int =
    super.drainTasksTo(c)
}
