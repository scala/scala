/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

/**
 * The <code>InputChannel</code> trait provides a common interface
 * for all channels from which values can be received.
 *
 * @version 0.9.4
 * @author Philipp Haller
 */
trait InputChannel[+Msg] {
  def receive[R](f: PartialFunction[Msg, R]): R
  def receiveWithin[R](msec: long)(f: PartialFunction[Any, R]): R
  def react(f: PartialFunction[Msg, Unit]): Nothing
  def reactWithin(msec: long)(f: PartialFunction[Any, Unit]): Nothing
}
