/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable;


/** This trait should be used as a mixin. It synchronizes the <code>Set</code>
 *  functions of the class into which it is mixed in.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
trait SynchronizedSet[A] extends scala.collection.mutable.Set[A] {

    abstract override def size: Int = synchronized {
        super.size
    }

    override def isEmpty: Boolean = synchronized {
        super.isEmpty
    }

    abstract override def contains(elem: A) = synchronized {
        super.contains(elem);
    }

    abstract override def +=(elem: A): Unit = synchronized {
        super.+=(elem);
    }

    override def incl(elems: A*): Unit = synchronized {
        super.incl(elems);
    }

    override def incl(that: Iterable[A]) = synchronized {
        super.incl(that);
    }

    abstract override def -=(elem: A): Unit = synchronized {
        super.-=(elem);
    }

    override def excl(elems: A*): Unit = synchronized {
        super.excl(elems);
    }

    override def excl(that: Iterable[A]) = synchronized {
        super.excl(that);
    }

    override def intersect(that: Set[A]) = synchronized {
        super.intersect(that);
    }

    abstract override def clear: Unit = synchronized {
        super.clear;
    }

    override def subsetOf(that: scala.collection.Set[A]) = synchronized {
        super.subsetOf(that);
    }

    override def foreach(f: A => Unit) = synchronized {
        super.foreach(f);
    }

    override def filter(p: A => Boolean) = synchronized {
        super.filter(p);
    }

    override def toList: List[A] = synchronized {
        super.toList;
    }

    override def toString() = synchronized {
        super.toString();
    }
}
