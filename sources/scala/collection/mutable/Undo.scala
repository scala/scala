/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable;


/** Classes that implement the <code>Undo</code> trait provide an operation
 *  <code>undo</code> which can be used to undo the last operation.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
trait Undo {

    /** Undo the last operation.
     */
    def undo: Unit;
}
