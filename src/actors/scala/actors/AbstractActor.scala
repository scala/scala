/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.actors

/**
 * @author Philipp Haller
 *
 * @define actor actor
 */
trait AbstractActor extends OutputChannel[Any] with CanReply[Any, Any] {

  type Future[+R] <: scala.actors.Future[R]

  private[actors] def exiting: Boolean = false

  private[actors] def linkTo(to: AbstractActor): Unit

  private[actors] def unlinkFrom(from: AbstractActor): Unit

  private[actors] def exit(from: AbstractActor, reason: AnyRef): Unit

}
