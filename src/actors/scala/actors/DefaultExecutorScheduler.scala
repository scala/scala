/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import java.util.concurrent.{ThreadPoolExecutor, TimeUnit, LinkedBlockingQueue}

/**
 * The <code>DefaultExecutorScheduler</code> class uses a default
 * <code>ThreadPoolExecutor</code> for executing <code>Actor</code>s.
 *
 * It can be configured using the two JVM properties
 * <code>actors.corePoolSize</code> and
 * <code>actors.maxPoolSize</code> that control the initial and
 * maximum size of the thread pool, respectively.
 *
 * @author Philipp Haller
 */
class DefaultExecutorScheduler extends ExecutorScheduler {

  private val workQueue = new LinkedBlockingQueue[Runnable]

  private val threadPool = new ThreadPoolExecutor(ThreadPoolConfig.corePoolSize,
                                                  ThreadPoolConfig.maxPoolSize,
                                                  50L,
                                                  TimeUnit.MILLISECONDS,
                                                  workQueue)

  executor = threadPool

  override val CHECK_FREQ = 50
}
