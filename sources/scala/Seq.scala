/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


/** Class <code>Seq[A]</code> represents finite sequences of elements
 *  of type <code>A</code>.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 */
trait Seq[+A] with PartialFunction[Int, A] with Iterable[A] {

    /** Returns the length of the sequence.
     *
     *  @return the sequence length.
     */
    def length: Int;

    /** Is this partial function defined for the index <code>x</code>?
     *
     *  @return true, iff <code>x</code> is a legal sequence index.
     */
    def isDefinedAt(x: Int): Boolean = (x >= 0) && (x < length);

    /** Customizes the <code>toString</code> method.
     *
     *  @return a string representation of this sequence.
     */
    override def toString() = {
        val iter = elements;
        var res = "Seq(";
        if (iter.hasNext) {
        	res = res + iter.next;
        	while (iter.hasNext)
        		res = res + ", " + iter.next;
        }
        res + ")"
    }
}
