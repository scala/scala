/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable;


/** This class is used internally to implement data structures that
 *  are based on resizable arrays.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 03/05/2004
 */
abstract class ResizableArray[A] with Iterable[A] {
    import java.lang.System.arraycopy;

    protected val initialSize: Int = 16;
    protected var array: Array[A] = new Array[A](initialSize);
    protected var size: Int = 0;

    /** Extends the internal array if more elements are needed.
     */
    protected def ensureSize(n: Int): Unit = {
        if ((size + n) > array.length) {
            val newar: Array[A] = new Array(array.length * 2);
            arraycopy(array, 0, newar, 0, size);
            array = newar;
        }
    }

    /** Swap two elements of this array.
     */
    protected def swap(a: Int, b: Int): Unit = {
        val h = array(a);
        array(a) = array(b);
        array(b) = h;
    }

    /** Move parts of the array.
     */
    protected def copy(m: Int, n: Int, len: Int) = {
        arraycopy(array, m, array, n, len);
    }

    /** Returns the length of this resizable array.
     */
    def length: Int = size;

    /** Returns a new iterator over all elements of this resizable array.
     */
    def elements: Iterator[A] = new Iterator[A] {
        var i = 0;
        def hasNext: Boolean = i < size;
        def next: A = { i = i + 1; array(i - 1) }
    }
}
