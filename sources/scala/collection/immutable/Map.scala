/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.immutable;


trait Map[A, B, This <: scala.collection.immutable.Map[A, B, This]]: This with scala.collection.Map[A, B] {

    def update(key: A, value: B): This;

    def remove(key: A): This;

    def clear: This;

    def put(mappings: Pair[A, B]*): This = putMap(mappings);

    def putMap(map: Iterable[Pair[A, B]]): This = {
        val iter = map.elements;
        var res = this;
        while (iter.hasNext) {
            val Pair(key, value) = iter.next;
            res = res.update(key, value);
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
            case Pair(key, value) => if (p(key, value)) { res = res.remove(key); }
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
}
