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

    def synchronized[a](def p: a): a =
        scala.runtime.NativeMonitor.synchronised(this, p);

    def await(def cond: Boolean) = while (!cond) { this.wait() }
}
