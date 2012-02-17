/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

import java.util.concurrent.{ThreadPoolExecutor, LinkedBlockingQueue, TimeUnit}

/** The `TaskRunners` object...
 *
 *  @author Philipp Haller
 */
@deprecated("Use `ExecutionContext`s instead.", "2.10.0")
object TaskRunners {

  implicit val threadRunner: FutureTaskRunner =
    new ThreadRunner

  implicit val threadPoolRunner: FutureTaskRunner = {
    val numCores = Runtime.getRuntime().availableProcessors()
    val keepAliveTime = 60000L
    val workQueue = new LinkedBlockingQueue[Runnable]
    val exec = new ThreadPoolExecutor(numCores,
                                      numCores,
                                      keepAliveTime,
                                      TimeUnit.MILLISECONDS,
                                      workQueue,
                                      new ThreadPoolExecutor.CallerRunsPolicy)
    JavaConversions.asTaskRunner(exec)
  }

}
