/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.actors
package scheduler

/**
 * Default scheduler for actors with daemon semantics, such as those backing futures.
 *
 * @author Erik Engbrecht
 */
object DaemonScheduler extends DelegatingScheduler {

  def makeNewScheduler(): IScheduler = new DefaultThreadPoolScheduler(true)

}
