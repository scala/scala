/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.immutable;


trait Map[A, B] with scala.collection.Map[A, B] {

    def update(key: A, value: B): Map[A, B];

    def -(key: A): Map[A, B];

    def +(key: A): MapTo = new MapTo(key);

    def incl(mappings: Pair[A, B]*): Map[A, B] = incl(mappings);

    def incl(map: Iterable[Pair[A, B]]): Map[A, B] = {
        val iter = map.elements;
        var res = this;
        while (iter.hasNext) {
            val Pair(key, value) = iter.next;
            res = res.update(key, value);
        }
        res;
    }

    def excl(keys: A*): Map[A, B] = excl(keys);

    def excl(keys: Iterable[A]): Map[A, B] = {
        val iter = keys.elements;
        var res = this;
        while (iter.hasNext) {
            res = res - iter.next;
        }
        res;
    }

    def map(f: (A, B) => B): Map[A, B] = {
        var res = this;
        elements foreach {
            case Pair(key, value) => res = res.update(key, f(key, value));
        }
        res;
    }

    def filter(p: (A, B) => Boolean): Map[A, B] = {
        var res = this;
        toList foreach {
            case Pair(key, value) => if (p(key, value)) { res = res.excl(key); }
        }
        res;
    }

    override def toString() =
        if (size == 0)
            "{}"
        else
            "{" + {
                val iter = elements;
                var res = mappingToString(iter.next);
                while (iter.hasNext) {
                    res = res + ", " + mappingToString(iter.next);
                }
                res;
            } + "}";

    def mappingToString(p: Pair[A, B]) = p._1.toString() + " -> " + p._2;

    class MapTo(key: A) {
        def ->(value: B): Map[A, B] = update(key, value);
    }
}
