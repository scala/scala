/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;

/** I promise, there will be some documentation soon! :-) Matthias
 */
trait MutableMap[A, B] with Map[A, B] {

    def update(key: A, value: B): Unit;

    def remove(key: A): Unit;

    def clear: Unit = {
    	val iter = keys;
    	while (iter.hasNext) {
            remove(iter.next);
        }
    }

    def put(key: A, value: B): B = {
        val old = apply(key);
        update(key, value);
        old;
    }

    def putAll(mappings: Pair[A, B]*) = {
        val ys = mappings as List[Pair[A, B]];
        ys foreach { case Pair(key, value) => update(key, value); };
    }

    def putMap(map: Iterable[Pair[A, B]]): Unit = map.elements foreach {
        case Pair(key, value) => update(key, value);
    }

    def map(f: (A, B) => B): Unit = elements foreach {
        case Pair(key, value) => update(key, f(key, value));
    }

    def filter(p: (A, B) => Boolean): Unit = toList foreach {
        case Pair(key, value) => if (p(key, value)) remove(key);
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
