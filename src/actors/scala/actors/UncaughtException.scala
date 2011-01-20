/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.actors

/**
 * The exit reason when an actor fails to catch an exception.
 *
 * @param actor   the actor that threw the exception
 * @param message the message the actor was processing, or None if no message (e.g. on initial startup)
 * @param sender  the sender of the most recent message
 * @param thread  the thread on which the actor was running
 * @param cause   the uncaught exception
 *
 * @author Philipp Haller
 * @author Erik Engbrecht
 */
case class UncaughtException(actor: Actor,
                             message: Option[Any],
                             sender: Option[OutputChannel[Any]],
                             thread: Thread,
                             cause: Throwable)
extends Exception(cause) {

  override def toString() =
    "UncaughtException("+actor+","+message+","+sender+","+cause+")"

}
