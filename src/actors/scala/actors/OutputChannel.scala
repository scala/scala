/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

/**
 * The <code>OutputChannel</code> trait provides a common interface
 * for all channels to which values can be sent.
 *
 * @version 0.9.17
 * @author Philipp Haller
 */
trait OutputChannel[-Msg] extends AbstractReactor[Msg] {

  /**
   * Sends <code>msg</code> to this
   * <code>OutputChannel</code> (asynchronous).
   */
  def !(msg: Msg): Unit

  /**
   * Sends <code>msg</code> to this
   * <code>OutputChannel</code> (asynchronous) supplying
   * explicit reply destination.
   *
   * @param  msg      the message to send
   * @param  replyTo  the reply destination
   */
  def send(msg: Msg, replyTo: OutputChannel[Any]): Unit

  /**
   * Forwards <code>msg</code> to this
   * <code>OutputChannel</code> (asynchronous).
   */
  def forward(msg: Msg): Unit

  /**
   * Returns the <code>Reactor</code> that is
   * receiving from this <code>OutputChannel</code>.
   */
  def receiver: Actor
}
