package scala.actors

import java.util.Collection
import forkjoin.{ForkJoinPool, ForkJoinTask}

private class DrainableForkJoinPool extends ForkJoinPool {

  override def drainTasksTo(c: Collection[ForkJoinTask[_]]): Int =
    super.drainTasksTo(c)

}
