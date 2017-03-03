/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection.parallel

import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.ForkJoinPool
import scala.concurrent.ExecutionContext

/** A trait implementing the scheduling of a parallel collection operation.
 *
 *  Parallel collections are modular in the way operations are scheduled. Each
 *  parallel collection is parameterized with a task support object which is
 *  responsible for scheduling and load-balancing tasks to processors.
 *
 *  A task support object can be changed in a parallel collection after it has
 *  been created, but only during a quiescent period, i.e. while there are no
 *  concurrent invocations to parallel collection methods.
 *
 *  There are currently a few task support implementations available for
 *  parallel collections. The [[scala.collection.parallel.ForkJoinTaskSupport]]
 *  uses a fork-join pool internally.
 *
 *  The [[scala.collection.parallel.ExecutionContextTaskSupport]] uses the
 *  default execution context implementation found in scala.concurrent, and it
 *  reuses the thread pool used in scala.concurrent.
 *
 *  The execution context task support is set to each parallel collection by
 *  default, so parallel collections reuse the same fork-join pool as the
 *  future API.
 *
 *  Here is a way to change the task support of a parallel collection:
 *
 *  {{{
 *  import scala.collection.parallel._
 *  val pc = mutable.ParArray(1, 2, 3)
 *  pc.tasksupport = new ForkJoinTaskSupport(
 *    new java.util.concurrent.ForkJoinPool(2))
 *  }}}
 *
 *  @see [[http://docs.scala-lang.org/overviews/parallel-collections/configuration.html Configuring Parallel Collections]] section
 *    on the parallel collection's guide for more information.
 */
trait TaskSupport extends Tasks

/** A task support that uses a fork join pool to schedule tasks.
 *
 *  @see [[scala.collection.parallel.TaskSupport]] for more information.
 */
class ForkJoinTaskSupport(val environment: ForkJoinPool = ForkJoinTasks.defaultForkJoinPool)
extends TaskSupport with AdaptiveWorkStealingForkJoinTasks

/** A task support that uses a thread pool executor to schedule tasks.
 *
 *  @see [[scala.collection.parallel.TaskSupport]] for more information.
 */
@deprecated("use `ForkJoinTaskSupport` instead", "2.11.0")
class ThreadPoolTaskSupport(val environment: ThreadPoolExecutor = ThreadPoolTasks.defaultThreadPool)
extends TaskSupport with AdaptiveWorkStealingThreadPoolTasks

/** A task support that uses an execution context to schedule tasks.
 *
 *  It can be used with the default execution context implementation in the
 *  `scala.concurrent` package. It internally forwards the call to either a
 *  forkjoin based task support or a thread pool executor one, depending on
 *  what the execution context uses.
 *
 *  By default, parallel collections are parameterized with this task support
 *  object, so parallel collections share the same execution context backend
 *  as the rest of the `scala.concurrent` package.
 *
 *  @see [[scala.collection.parallel.TaskSupport]] for more information.
 */
class ExecutionContextTaskSupport(val environment: ExecutionContext = scala.concurrent.ExecutionContext.global)
extends TaskSupport with ExecutionContextTasks
