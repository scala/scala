/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

/**
 * The <code>AbstractReactor</code> trait.
 *
 * @version 0.9.18
 * @author Philipp Haller
 */
package scala.actors

trait AbstractReactor[-T] {

  /**
   * Sends <code>msg</code> to this
   * <code>AbstractReactor</code> (asynchronous).
   */
  def !(msg: T): Unit

}
