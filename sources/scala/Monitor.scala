/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


/** This trait can be used to mix in functionality for synchronizing
 *  threads on this object.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 11/07/2003
 */
trait Monitor {
  def await(def cond: boolean) = while (false == cond) { wait() }
}
