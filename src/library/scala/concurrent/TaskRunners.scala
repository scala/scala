package scala.concurrent

import java.util.concurrent.{ThreadPoolExecutor, LinkedBlockingQueue, TimeUnit}

/** The <code>TaskRunners</code> object...
 *
 *  @author Philipp Haller
 */
object TaskRunners {

  implicit val threadRunner: TaskRunner[Unit] =
    new ThreadRunner[Unit]

  implicit val threadPoolRunner: TaskRunner[Unit] = {
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
