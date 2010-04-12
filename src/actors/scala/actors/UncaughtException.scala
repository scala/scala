/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
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
 * @param thread  the thread on which the actor was running
 * @param cause   the uncaught exception
 *
 * @author Philipp Haller
 * @author Erik Engbrecht
 */
class UncaughtException[Msg >: Null](val actor: Reactor[Msg],
                                     val message: Option[Msg],
                                     val sender: Option[OutputChannel[Any]],
                                     val thread: Thread,
                                     cause: Exception)
extends Exception(cause) {

  override def toString() =
    "UncaughtException("+actor+","+message+","+sender+","+cause+")"

}
