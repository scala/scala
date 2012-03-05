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



/** A trait implementing the scheduling of
 *  a parallel collection operation.
 *  
 *  Task support objects handle how a task is split and
 *  distributed across processors. A task support object can be
 *  changed in a parallel collection after it has been created,
 *  but only during a quiescent period, i.e. while there are no
 *  concurrent invocations to parallel collection methods.
 */
trait TaskSupport extends Tasks


/** A task support that uses a fork join pool to schedule tasks */
class ForkJoinTaskSupport(val environment: ForkJoinPool = ForkJoinTasks.defaultForkJoinPool)
extends TaskSupport with AdaptiveWorkStealingForkJoinTasks


/** A task support that uses a thread pool executor to schedule tasks */
class ThreadPoolTaskSupport(val environment: ThreadPoolExecutor = ThreadPoolTasks.defaultThreadPool)
extends TaskSupport with AdaptiveWorkStealingThreadPoolTasks


/** A task support that uses an execution context to schedule tasks.
 *  
 *  It can be used with the default execution context implementation in the `scala.concurrent` package.
 *  It internally forwards the call to either a forkjoin based task support or a thread pool executor one,
 *  depending on what the execution context uses.
 *  
 *  By default, parallel collections are parametrized with this task support object, so parallel collections
 *  share the same execution context backend as the rest of the `scala.concurrent` package.
 */
class ExecutionContextTaskSupport(val environment: ExecutionContext = scala.concurrent.executionContext)
extends TaskSupport with ExecutionContextTasks



















