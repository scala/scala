/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable;


/** Buffers are used to create sequences of elements incrementally by
 *  appending, prepending, or inserting new elements. It is also
 *  possible to access and modify elements in a random access fashion
 *  via the index of the element in the current sequence.
 *
 *  @author  Matthias Zenger
 *  @version 1.1, 02/03/2004
 */
trait Buffer[A] with Seq[A] {

    /** Append a single element to this buffer and return
     *  the identity of the buffer.
     *
     *  @param elem  the element to append.
     */
    def +(elem: A): Buffer[A];

    /** Append a single element to this buffer.
     *
     *  @param elem  the element to append.
     */
    def +=(elem: A): Unit = this + elem;

    /** Appends a number of elements provided by an iterable object
     *  via its <code>elements</code> method. The identity of the
     *  buffer is returned.
     *
     *  @param iter  the iterable object.
     */
    def ++(iter: Iterable[A]): Buffer[A] = {
        iter.elements.foreach(e => this += e);
        this
    }

    /** Appends a number of elements provided by an iterable object
     *  via its <code>elements</code> method.
     *
     *  @param iter  the iterable object.
     */
    def ++=(iter: Iterable[A]): Unit = this ++ iter;

    /** Appends a sequence of elements to this buffer.
     *
     *  @param elems  the elements to append.
     */
    def append(elems: A*): Unit = this ++ elems;

    /** Appends a number of elements provided by an iterable object
     *  via its <code>elements</code> method.
     *
     *  @param iter  the iterable object.
     */
    def appendAll(iter: Iterable[A]): Unit = this ++ iter;

    /** Prepend a single element to this buffer and return
     *  the identity of the buffer.
     *
     *  @param elem  the element to append.
     */
    def +:(elem: A): Buffer[A];

    /** Prepends a number of elements provided by an iterable object
     *  via its <code>elements</code> method. The identity of the
     *  buffer is returned.
     *
     *  @param iter  the iterable object.
     */
    def ++:(iter: Iterable[A]): Buffer[A] = {
        iter.toList.reverse.foreach(e => e +: this);
        this
    }

    /** Prepend an element to this list.
     *
     *  @param elem  the element to prepend.
     */
    def prepend(elems: A*): Unit = elems ++: this;

    /** Prepends a number of elements provided by an iterable object
     *  via its <code>elements</code> method. The identity of the
     *  buffer is returned.
     *
     *  @param iter  the iterable object.
     */
    def prependAll(elems: Iterable[A]): Unit = elems ++: this;

    /** Inserts new elements at the index <code>n</code>. Opposed to method
     *  <code>update</code>, this method will not replace an element with a
     *  one. Instead, it will insert the new elements at index <code>n</code>.
     *
     *  @param n      the index where a new element will be inserted.
     *  @param elems  the new elements to insert.
     */
    def insert(n: Int, elems: A*): Unit = insertAll(n, elems);

    /** Inserts new elements at the index <code>n</code>. Opposed to method
     *  <code>update</code>, this method will not replace an element with a
     *  one. Instead, it will insert a new element at index <code>n</code>.
     *
     *  @param n     the index where a new element will be inserted.
     *  @param iter  the iterable object providing all elements to insert.
     */
    def insertAll(n: Int, iter: Iterable[A]): Unit;

    /** Replace element at index <code>n</code> with the new element
     *  <code>newelem</code>.
     *
     *  @param n       the index of the element to replace.
     *  @param newelem the new element.
     */
    def update(n: Int, newelem: A): Unit;

    /** Removes the element on a given index position.
     *
     *  @param n  the index which refers to the element to delete.
     */
    def remove(n: Int): A;

    /** Clears the buffer contents.
     */
    def clear: Unit;

    /** The hashCode method always yields an error, since it is not
     *  safe to use buffers as keys in hash tables.
     *
     *  @return never.
     */
    override def hashCode(): Int = error("unsuitable as hash key");

    /** Defines the prefix of the string representation.
	 */
	override protected def stringPrefix: String = "Buffer";
}
