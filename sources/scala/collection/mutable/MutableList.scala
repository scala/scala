/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable;


/** This class is used internally to represent mutable lists. It is the
 *  basis for the implementation of the classes <code>Buffer</code>,
 *  <code>Stack</code>, and <code>Queue</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
abstract class MutableList[A] extends Seq[A] with PartialFunction[Int, A] {

    protected var first: LinkedList[A] = null;
    protected var last: LinkedList[A] = null;
    protected var len: Int = 0;

    def length: Int = len;

    def apply(n: Int): A = get(n) match {
        case None => error("element not found")
        case Some(value) => value
    }

    def get(n: Int): Option[A] = first.get(n);

    protected def prependElem(elem: A): Unit = {
        first = new LinkedList[A](elem, first);
        if (len == 0)
            last = first;
        len = len + 1;
    }

    protected def appendElem(elem: A): Unit = {
        if (len == 0)
            prependElem(elem);
        else {
            last.next = new LinkedList[A](elem, null);
            last = last.next;
            len = len + 1;
        }
    }

    protected def reset: Unit = {
        first = null;
        last = null;
        len = 0;
    }

    def elements: Iterator[A] =
        if (first == null) Nil.elements else first.elements;

    override def toList: List[A] = if (first == null) Nil else first.toList;

	override protected def stringPrefix: String = "MutableList";
}
