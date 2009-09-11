package scala.actors
package scheduler

import java.util.Collection
import scala.concurrent.forkjoin.{ForkJoinPool, ForkJoinTask}

private class DrainableForkJoinPool extends ForkJoinPool {

  override def drainTasksTo(c: Collection[ForkJoinTask[_]]): Int =
    super.drainTasksTo(c)

}
