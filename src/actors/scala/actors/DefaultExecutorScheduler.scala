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

  private val rt = Runtime.getRuntime()
  private val minNumThreads = 4

  /** The value of the actors.corePoolSize JVM property. This property
   *  determines the initial thread pool size.
   */
  private val coreProp = try {
    System.getProperty("actors.corePoolSize")
  } catch {
    case ace: java.security.AccessControlException =>
      null
  }

  private val maxProp =
    try {
      System.getProperty("actors.maxPoolSize")
    } catch {
      case ace: java.security.AccessControlException =>
        null
    }

  private val initCoreSize =
    if (null ne coreProp) Integer.parseInt(coreProp)
    else {
      val numCores = rt.availableProcessors()
      if (2 * numCores > minNumThreads)
        2 * numCores
      else
        minNumThreads
    }

  private val maxSize =
    if (null ne maxProp) Integer.parseInt(maxProp)
    else 256

  private val coreSize = initCoreSize

  private val workQueue = new LinkedBlockingQueue[Runnable]

  private val threadPool = new ThreadPoolExecutor(coreSize,
                                                  maxSize,
                                                  50L,
                                                  TimeUnit.MILLISECONDS,
                                                  workQueue)

  executor = threadPool

  override val CHECK_FREQ = 50
}
