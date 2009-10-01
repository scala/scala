/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

/**
 * The <code>InputChannel</code> trait provides a common interface
 * for all channels from which values can be received.
 *
 * @version 0.9.8
 * @author Philipp Haller
 */
trait InputChannel[+Msg] {

  /**
   * Receives a message from this <code>InputChannel</code>.
   *
   * @param  f    a partial function with message patterns and actions
   * @return      result of processing the received value
   */
  def receive[R](f: PartialFunction[Msg, R]): R

  /**
   * Receives a message from this <code>InputChannel</code> within
   * a certain time span.
   *
   * @param  msec the time span before timeout
   * @param  f    a partial function with message patterns and actions
   * @return      result of processing the received value
   */
  def receiveWithin[R](msec: Long)(f: PartialFunction[Any, R]): R

  /**
   * Receives a message from this <code>InputChannel</code>.
   * <p>
   * This method never returns. Therefore, the rest of the computation
   * has to be contained in the actions of the partial function.
   *
   * @param  f    a partial function with message patterns and actions
   */
  def react(f: PartialFunction[Msg, Unit]): Nothing

  /**
   * Receives a message from this <code>InputChannel</code> within
   * a certain time span.
   * <p>
   * This method never returns. Therefore, the rest of the computation
   * has to be contained in the actions of the partial function.
   *
   * @param  msec the time span before timeout
   * @param  f    a partial function with message patterns and actions
   */
  def reactWithin(msec: Long)(f: PartialFunction[Any, Unit]): Nothing

  /**
   * Receives the next message from this <code>Channel</code>.
   */
  def ? : Msg
}
