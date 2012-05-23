/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.actors

@deprecated("Scala Actors are being removed from the standard library. Please refer to the migration guide.", "2.10")
trait ReplyReactor extends InternalReplyReactor {
  protected[actors] def sender: OutputChannel[Any] = super.internalSender
}
