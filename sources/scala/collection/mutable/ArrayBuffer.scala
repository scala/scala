/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable;


/** An implementation of the Buffer trait using an array to
 *  represent the assembled sequence internally.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 15/03/2004
 */
class ArrayBuffer[A] extends Buffer[A] {

	protected def initialCapacity: Int = 8;

	private var buf: Array[A] = new Array(initialCapacity);
	private var size: Int = 0;

	def length: Int = size;

	def apply(n: Int): A = {
	    if ((n < 0) || (n >= size))
	    	error("cannot access element " + n + " in ArrayBuffer");
	    else
	    	buf(n);
	}

	protected def ensureCapacity(n: Int): Unit = {
	    if ((size + n) >= buf.length) {
	        val newbuf: Array[A] = new Array(buf.length * 2);
	        System.arraycopy(buf, 0, newbuf, 0, size);
	        buf = newbuf;
	    }
	}

    /** Append a single element to this buffer and return
     *  the identity of the buffer.
     *
     *  @param elem  the element to append.
     */
    def +(elem: A): Buffer[A] = {
    	ensureCapacity(1);
    	buf(size) = elem;
    	size = size + 1;
    	this
    }

    /** Appends a number of elements provided by an iterable object
     *  via its <code>elements</code> method. The identity of the
     *  buffer is returned.
     *
     *  @param iter  the iterable object.
     */
    override def ++(iter: Iterable[A]): Buffer[A] = { insertAll(size, iter); this }

    /** Prepend a single element to this buffer and return
     *  the identity of the buffer.
     *
     *  @param elem  the element to append.
     */
    def +:(elem: A): Buffer[A] = {
        ensureCapacity(1);
        System.arraycopy(buf, 0, buf, 1, size);
        buf(0) = elem;
        size = size + 1;
        this
    }

    /** Prepends a number of elements provided by an iterable object
     *  via its <code>elements</code> method. The identity of the
     *  buffer is returned.
     *
     *  @param iter  the iterable object.
     */
    override def ++:(iter: Iterable[A]): Buffer[A] = { insertAll(0, iter); this }

    /** Inserts new elements at the index <code>n</code>. Opposed to method
     *  <code>update</code>, this method will not replace an element with a
     *  one. Instead, it will insert a new element at index <code>n</code>.
     *
     *  @param n     the index where a new element will be inserted.
     *  @param iter  the iterable object providing all elements to insert.
     */
    def insertAll(n: Int, iter: Iterable[A]): Unit = {
        if ((n < 0) || (n > size))
        	error("cannot insert element " + n + " in ListBuffer");
    	val xs = iter.elements.toList;
    	val len = xs.length;
    	ensureCapacity(len);
    	System.arraycopy(buf, n, buf, n + len, size - n);
    	xs.copyToArray(buf, n);
    	size = size + len;
    }

    /** Replace element at index <code>n</code> with the new element
     *  <code>newelem</code>.
     *
     *  @param n       the index of the element to replace.
     *  @param newelem the new element.
     */
    def update(n: Int, newelem: A): Unit = {
    	if ((n < 0) || (n >= size))
    		error("cannot update element " + n + " in ArrayBuffer");
    	else {
    		val res = buf(n);
    		buf(n) = newelem;
    		res
    	}
    }

    /** Removes the element on a given index position.
     *
     *  @param n  the index which refers to the element to delete.
     */
    def remove(n: Int): A = {
    	if ((n < 0) || (n >= size))
    		error("cannot remove element " + n + " in Buffer");
        val res = buf(n);
        System.arraycopy(buf, n + 1, buf, n, size - n - 1);
        size = size - 1;
        res
    }

    /** Clears the buffer contents.
     */
    def clear: Unit = {
        size = 0;
    }

    /** Returns a new iterator over all elements of this buffer.
     */
    def elements: Iterator[A] = new Iterator[A] {
        var i = 0;
        def hasNext: Boolean = i < size;
        def next: A = { i = i + 1; buf(i - 1) }
    }

    /** Checks if two buffers are structurally identical.
     *
     *  @return true, iff both buffers contain the same sequence of elements.
     */
    override def equals(obj: Any): Boolean = obj match {
    	case that: ArrayBuffer[A] =>
    		elements.zip(that.elements).forall {
    			case Pair(thiselem, thatelem) => thiselem == thatelem;
    		}
    	case _ => false
    }

    /** Defines the prefix of the string representation.
	 */
	override protected def stringPrefix: String = "ArrayBuffer";
}
