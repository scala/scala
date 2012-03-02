/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection.parallel



import java.util.concurrent.ThreadPoolExecutor
import scala.concurrent.forkjoin.ForkJoinPool
import scala.concurrent.ExecutionContext



trait TaskSupport extends Tasks


private[collection] class ForkJoinTaskSupport(val environment: ForkJoinPool = ForkJoinTasks.defaultForkJoinPool)
extends TaskSupport with AdaptiveWorkStealingForkJoinTasks


private[collection] class ThreadPoolTaskSupport(val environment: ThreadPoolExecutor = ThreadPoolTasks.defaultThreadPool)
extends TaskSupport with AdaptiveWorkStealingThreadPoolTasks


private[collection] class ExecutionContextTaskSupport(val environment: ExecutionContext = scala.concurrent.executionContext)
extends TaskSupport with ExecutionContextTasks



















