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
 * @version 0.9.6
 * @author Philipp Haller
 */
trait OutputChannel[Msg] {
  def !(msg: Msg): Unit
  def forward(msg: Msg): Unit
}
