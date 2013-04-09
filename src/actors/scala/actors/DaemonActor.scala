/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.actors

import scheduler.DaemonScheduler

/**
 * Base trait for actors with daemon semantics.
 *
 * Unlike a regular `Actor`, an active `DaemonActor` will not
 * prevent an application terminating, much like a daemon thread.
 *
 * @author Erik Engbrecht
 */
@deprecated("Use the akka.actor package instead. For migration from the scala.actors package refer to the Actors Migration Guide.", "2.11.0")
trait DaemonActor extends Actor {
  override def scheduler: IScheduler = DaemonScheduler
}
