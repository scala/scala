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
class SeqProxy[+A](x: Seq[A]) extends Seq[A] with IterableProxy(x) {

    /** Returns the length of the sequence.
     *
     *  @return the sequence length.
     */
    def length: Int = x.length;

    /** Access element number <code>n</code>.
     *
     *  @return  the element at index <code>n</code>.
     */
    def apply(n: Int): A = x.apply(n);

    /** Is this partial function defined for the index <code>x</code>?
     *
     *  @return true, iff <code>x</code> is a legal sequence index.
     */
    override def isDefinedAt(y: Int): Boolean = x.isDefinedAt(y);

    /** Returns the index of the first occurence of the specified
     *  object in this sequence.
     *
     *  @param  elem  element to search for.
     *  @return the index in this sequence of the first occurence of the specified
     * 	        element, or -1 if the sequence does not contain this element.
     */
    override def indexOf[B >: A](elem: B): Int = x.indexOf(elem);

    /** Returns the index of the last occurence of the specified
     *  element in this sequence, or -1 if the sequence does not
     *  contain this element.
     *
     *  @param  elem   element to search for.
     *  @return the index in this sequence of the last occurence of the
     *          specified  element, or -1 if the sequence does not contain
     *          this element.
     */
	override def lastIndexOf[B >: A](elem: B): Int = x.lastIndexOf(elem);

    /** Returns the sub-sequence starting from index <code>n</code>.
     */
    override def take(n: Int): Seq[A] = x.take(n);

    /** Returns a new sub-sequence that drops the first <code>n</code>
     *  elements of this sequence.
     */
    override def drop(n: Int): Seq[A] = x.drop(n);

    /** Returns a subsequence starting from index <code>from</code>
     *  consisting of <code>len</code> elements.
     */
    override def subseq(from: Int, len: Int): Seq[A] = x.subseq(from, len);

    /** Fills the given array <code>xs</code> with the elements of
     *  this sequence starting at position <code>start</code>.
     *
     *  @param  xs the array to fill.
     *  @param  start starting index.
     *  @return the given array <code>xs</code> filled with the elements
     *          of this sequence.
     */
    override def copyToArray[B >: A](xs: Array[B], start: Int): Array[B] = x.copyToArray(xs, start);

    /** Transform this sequence into a list of all elements.
     *
     *  @return  a list which enumerates all elements of this sequence.
     */
    override def toList: List[A] = x.toList;
}
