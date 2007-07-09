/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2007, LAMP/EPFL             **
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
 * @version 0.9.8
 * @author Philipp Haller
 */
trait OutputChannel[Msg] {

  /**
   * Sends <code>msg</code> to this
   * <code>OutputChannel</code> (asynchronous).
   */
  def !(msg: Msg): Unit

  /**
   * Forwards <code>msg</code> to this
   * <code>OutputChannel</code> (asynchronous).
   */
  def forward(msg: Msg): Unit
}
