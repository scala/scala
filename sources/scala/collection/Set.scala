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
trait Set[A] with Iterable[A]
             with StructuralEquality[Set[A]] {

    def size: Int;

    def contains(elem: A): Boolean;

    def isEmpty: Boolean = (size == 0);

    def subsetOf(that: Set[A]): Boolean = forall(that.contains);

    def foreach(f: A => Unit): Unit = elements.foreach(f);

    def forall(p: A => Boolean): Boolean = elements.forall(p);

    def exists(p: A => Boolean): Boolean = elements.exists(p);

    def toList: List[A] = {
        var res: List[A] = Nil;
        elements.foreach { elem => res = elem :: res; }
        res;
    }

    override def ===[B >: Set[A]](that: B): Boolean =
        that.isInstanceOf[Set[A]] &&
        { val other = that.asInstanceOf[Set[A]];
          this.size == other.size &&
          this.elements.forall(other.contains) };

    override def toString(): String =
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
