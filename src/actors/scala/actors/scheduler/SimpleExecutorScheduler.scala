/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors
package scheduler

import java.util.concurrent.ExecutorService

/**
 * The <code>SimpleExecutorScheduler</code> class uses an
 * <code>ExecutorService</code> to execute <code>Actor</code>s. It
 * does not start an additional thread.
 *
 * A <code>SimpleExecutorScheduler</code> attempts to shut down
 * the underlying <code>ExecutorService</code> only if
 * <code>terminate</code> is set to true.
 *
 * Otherwise, the <code>ExecutorService</code> must be shut down either
 * directly or by shutting down the
 * <code>SimpleExecutorScheduler</code> instance.
 *
 * @author Philipp Haller
 */
class SimpleExecutorScheduler(protected var executor: ExecutorService,
                              protected var terminate: Boolean)
  extends TerminationService(terminate) with ExecutorScheduler {

  /* This constructor (and the var above) is currently only used to work
   * around a bug in scaladoc, which cannot deal with early initializers
   * (to be used in subclasses such as DefaultExecutorScheduler) properly.
   */
  def this() {
    this(null, true)
  }

}
