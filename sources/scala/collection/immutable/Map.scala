/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.immutable;


trait Map[A, B, +This <: Map[A, B, This]]: This with scala.collection.Map[A, B] {

    def update(key: A, value: B): This;

    def -(key: A): This;

    def +(key: A): MapTo = new MapTo(key);

    def incl(mappings: Pair[A, B]*): This = incl(mappings);

    def incl(map: Iterable[Pair[A, B]]): This = {
        val iter = map.elements;
        var res = this;
        while (iter.hasNext) {
            val Pair(key, value) = iter.next;
            res = res.update(key, value);
        }
        res;
    }

    def excl(keys: A*): This = excl(keys);

    def excl(keys: Iterable[A]): This = {
        val iter = keys.elements;
        var res = this;
        while (iter.hasNext) {
            res = res - iter.next;
        }
        res;
    }

    def map(f: (A, B) => B): This = {
        var res = this;
        elements foreach {
            case Pair(key, value) => res = res.update(key, f(key, value));
        }
        res;
    }

    def filter(p: (A, B) => Boolean): This = {
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
        def ->(value: B): This = update(key, value);
    }
}
