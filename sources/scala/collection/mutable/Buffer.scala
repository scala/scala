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
 *  appending or prepending new elements. It is also possible to
 *  access and modify elements in a random access fashion via the
 *  index of the element in the sequence.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
class Buffer[A] with MutableList[A] {

    /** Prepend an element to this list.
     *
     *  @param elem  the element to prepend.
     */
    def prepend(elem: A) = prependElem(elem);

    /** Appends a sequence of elements to this buffer.
     *
     *  @param elems  the elements to append.
     */
    def append(elems: A*) = (this += elems);

    /** Append a single element to this buffer.
     *
     *  @param elem  the element to append.
     */
    def +=(elem: A) = appendElem(elem);

    /** Appends a number of elements provided by an iterable object
     *  via its <code>elements</code> method.
     *
     *  @param iter  the iterable object.
     */
    def +=(iter: Iterable[A]) = iter.elements.foreach(e => appendElem(e));

    /** Replace element at index <code>n</code> with the new element
     *  <code>newelem</code>.
     *
     *  @param n       the index of the element to replace.
     *  @param newelem the new element.
     */
    def update(n: Int, newelem: A): Unit = {
        var elem = first;
        var i = n;
        while (i > 0) {
            elem = elem.next;
            if (elem == null)
                error("cannot update element " + n + " in Buffer");
            i = i - 1;
        }
        elem.elem = newelem;
    }

    /** Inserts a new element at the index <code>n</code>. Opposed to method
     *  <code>update</code>, this method will not replace an element with a
     *  one. Instead, it will insert a new element at index <code>n</code>.
     *
     *  @param n        the index where a new element will be inserted.
     *  @param newelem  the new element to insert.
     */
    def insert(n: Int, newelem: A): Unit = {
        if (n == 0)
            prepend(newelem);
        else if (n >= len)
            append(newelem);
        else {
            var elem = first;
            var i = n;
            while (i > 1) {
                elem = elem.next;
                if (elem == null)
                    error("cannot insert element " + n + " in Buffer");
                i = i - 1;
            }
            val old = elem.next;
            elem.next = new LinkedList[A](newelem, old);
        }
    }

    /** Removes the element on a given index position.
     *
     *  @param n  the index which refers to the element to delete.
     */
    def remove(n: Int): A = {
        val old = apply(n);
        if (n >= len)
            error("cannot remove element " + n + " in Buffer");
        if ((n == 0) && (len == 1)) {
            first = null;
            last = null;
        } else if (n == 0) {
            first = first.next;
        } else {
            var elem = first;
            var i = n;
            while (i > 1) {
                elem = elem.next;
                i = i - 1;
            }
            elem.next = elem.next.next;
            if (n == (len - 1)) {
                last = elem.next;
            }
        }
        len = len - 1;
        old;
    }

    /** Clears the buffer contents.
     */
    def clear: Unit = reset;

    /** Checks if two buffers are structurally identical.
     *
     *  @return true, iff both buffers contain the same sequence of elements.
     */
    override def equals(that: Any): Boolean =
        that.isInstanceOf[Buffer[A]] &&
        { val other = that.asInstanceOf[Buffer[A]];
          elements.zip(other.elements).forall {
            case Pair(thiselem, thatelem) => thiselem == thatelem;
        }};

    /** The hashCode method always yields an error, since it is not
     *  safe to use buffers as keys in hash tables.
     *
     *  @return never.
     */
    override def hashCode(): Int = error("unsuitable as hash key");
}
