/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


/** Collection classes supporting this trait provide a method
 *  <code>elements</code> which returns an iterator over all the
 *  elements contained in the collection.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 */
trait StructuralEquality[+A <: StructuralEquality[A]] {

    /** Compares this object with the provided object structurally;
     *  i.e. by looking at the internal structure of aggregated objects.
     *  <code>===</code> does not have to be compatible with the
     *  <code>hashCode</code> method.
     *
     *  @param   that	the other object
     *  @returns true, iff <code>this</code> and <code>that</code> are
     *           structurally equivalent.
     */
    def ===[B >: A](that: B): Boolean = (this == that);

    /** Compares this object with the provided object structurally
     *  and returns true, iff the two objects differ.
     *
     *  @param   that	the other object
     *  @returns false, iff <code>this</code> and <code>that</code> are
     *           structurally equivalent.
     */
    def !==[B >: A](that: B): Boolean = !(this === that);
}
