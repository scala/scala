/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


trait SynchronizedMap[A, B] extends MutableMap[A, B] with Monitor {

    override def size: Int = synchronized {
        super.size;
    }

    override def get(key: A): Option[B] = synchronized {
        super.get(key);
    }

    override def isEmpty: Boolean = synchronized {
        super.isEmpty;
    }

    override def apply(key: A): B = synchronized {
        super.apply(key);
    }

    override def contains(key: A): Boolean = synchronized {
        super.contains(key);
    }

    override def isDefinedAt(key: A) = synchronized {
        super.isDefinedAt(key);
    }

    override def keys: Iterator[A] = synchronized {
        super.keys;
    }

    override def values: Iterator[B] = synchronized {
        super.values;
    }

    override def foreach(f: (A, B) => Unit) = synchronized {
        super.foreach(f);
    }

    override def toList: List[Pair[A, B]] = synchronized {
        super.toList;
    }

    override def update(key: A, value: B): Unit = synchronized {
        super.update(key, value);
    }

    override def remove(key: A): Unit = synchronized {
        super.remove(key);
    }

    override def clear: Unit = synchronized {
        super.clear;
    }

    override def put(mappings: Pair[A, B]*) = synchronized {
        super.putMap(mappings);
    }

    override def putMap(map: Iterable[Pair[A, B]]): Unit = synchronized {
        super.putMap(map);
    }

    override def map(f: (A, B) => B): Unit = synchronized {
        super.map(f);
    }

    override def filter(p: (A, B) => Boolean): Unit = synchronized {
        super.filter(p);
    }

    override def toString() = synchronized {
        super.toString();
    }
}
