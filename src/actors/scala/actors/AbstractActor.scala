/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.actors

import scala.language.higherKinds

/**
 * @author Philipp Haller
 *
 * @define actor actor
 */
@deprecated("Use the akka.actor package instead. For migration from the scala.actors package refer to the Actors Migration Guide.", "2.11.0")
trait AbstractActor extends OutputChannel[Any] with CanReply[Any, Any] {

  type Future[+R] <: scala.actors.Future[R]

  private[actors] def exiting: Boolean = false

  private[actors] def linkTo(to: AbstractActor): Unit

  private[actors] def unlinkFrom(from: AbstractActor): Unit

  private[actors] def exit(from: AbstractActor, reason: AnyRef): Unit
}
