/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection;


/** This trait defines the interface of collections that do not contain
 *  duplicate elements. Trait <code>Set</code> may only be used for
 *  accessing elements from set implementations. Two different extensions
 *  of trait <code>Set</code> in the package <code>scala.collections.mutable</code>
 *  and  <code>scala.collections.immutable</code> provide functionality for
 *  adding new elements to a set. The trait in the first package is implemented
 *  by sets the are modified destructively, whereas the trait in the second
 *  package is used by functional set implementations that rely on immutable
 *  data structures.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
trait Set[A] with Iterable[A] {

    def size: Int;

    def isEmpty: Boolean = (size == 0);

    def contains(elem: A): Boolean;

    def subsetOf(that: Set[A]): Boolean = {
        val iter = elements;
        var res = true;
        while (res && iter.hasNext) {
            res = that.contains(iter.next);
        }
        res
    }

    def foreach(f: A => Unit): Unit = {
        val iter = elements;
        while (iter.hasNext) {
            f(iter.next);
        }
    }

    def exists(p: A => Boolean): Boolean = {
        val iter = elements;
        var res = false;
        while (!res && iter.hasNext) {
            if (p(iter.next)) { res = true; }
        }
        res;
    }

    def toList: List[A] = {
        var res: List[A] = Nil;
        val iter = elements;
        while (iter.hasNext) {
            res = iter.next :: res;
        }
        res;
    }

    override def toString() =
        if (size == 0)
            "{}"
        else
            "{" + {
                val iter = elements;
                var res = iter.next.toString();
                while (iter.hasNext) {
                    res = res + ", " + iter.next;
                }
                res;
            } + "}";
}
