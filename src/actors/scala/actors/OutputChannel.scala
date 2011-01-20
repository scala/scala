/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.actors

import scala.annotation.unique.unique

/**
 * The <code>OutputChannel</code> trait provides a common interface
 * for all channels to which values can be sent.
 *
 * @author Philipp Haller
 *
 * @define actor `OutputChannel`
 */
trait OutputChannel[-Msg] {

  /**
   * Sends <code>msg</code> to this $actor (asynchronous).
   *
   * @param  msg      the message to send
   */
  def !(msg: Msg @unique): Unit

  /**
   * Sends <code>msg</code> to this $actor (asynchronous) supplying
   * explicit reply destination.
   *
   * @param  msg      the message to send
   * @param  replyTo  the reply destination
   */
  def send(msg: Msg @unique, replyTo: OutputChannel[Any]): Unit

  /**
   * Forwards <code>msg</code> to this $actor (asynchronous).
   *
   * @param  msg      the message to forward
   */
  def forward(msg: Msg @unique): Unit

  /**
   * Returns the <code>Actor</code> that is receiving from this $actor.
   */
  def receiver: Actor
}
