/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable


/** This class should be used as a mixin. It synchronizes the <code>Map</code>
 *  functions of the class into which it is mixed in.
 *
 *  @author  Matthias Zenger, Martin Odersky
 *  @version 2.0, 31/12/2006
 */
trait SynchronizedMap[A, B] extends Map[A, B] {

  abstract override def size: Int = synchronized {
    super.size
  }

  abstract override def get(key: A): Option[B] = synchronized {
    super.get(key)
  }

  override def isEmpty: Boolean = synchronized {
    super.isEmpty
  }

  override def apply(key: A): B = synchronized {
    super.apply(key)
  }

  override def contains(key: A): Boolean = synchronized {
    super.contains(key)
  }

  override def isDefinedAt(key: A) = synchronized {
    super.isDefinedAt(key)
  }

  override def keys: Iterator[A] = synchronized {
    super.keys
  }

  override def keySet: collection.Set[A] = synchronized {
    super.keySet
  }

  override def values: Iterator[B] = synchronized {
    super.values
  }

  abstract override def elements: Iterator[(A, B)] = synchronized {
    super.elements
  }

  override def toList: List[(A, B)] = synchronized {
    super.toList
  }

  abstract override def update(key: A, value: B): Unit = synchronized {
    super.update(key, value)
  }

  override def += (kv: (A, B)): Unit = synchronized {
    super.+=(kv)
  }

  /** Add two or more key/value pairs to this map.
   *  @param    kv1 the key/first value pair.
   *  @param    kv2 the second key/first value pair.
   *  @param    kvs the remaining key/first value pairs.
   */
  override def += (kv1: (A, B), kv2: (A, B), kvs: (A, B)*): Unit = synchronized {
    super.+=(kv1, kv2, kvs: _*)
  }

  override def ++=(map: Iterable[(A, B)]): Unit = synchronized {
    super.++=(map)
  }

  override def ++=(it: Iterator[(A, B)]): Unit = synchronized {
    super.++=(it)
  }

  @deprecated
  override def incl(mappings: (A, B)*): Unit = synchronized {
    super.incl(mappings: _*)
  }

  abstract override def -=(key: A): Unit = synchronized {
    super.-=(key)
  }

  override def -= (key1: A, key2: A, keys: A*): Unit = synchronized {
    super.-=(key1, key2, keys: _*)
  }

  override def --=(keys: Iterable[A]): Unit = synchronized {
    super.--=(keys)
  }

  override def --=(it: Iterator[A]): Unit = synchronized {
    super.--=(it)
  }

  @deprecated
  override def excl(keys: A*): Unit = synchronized {
    super.excl(keys: _*)
  }

  override def clear(): Unit = synchronized {
    super.clear
  }

  override def transform(f: (A, B) => B): Unit = synchronized {
    super.transform(f)
  }

  override def retain(p: (A, B) => Boolean): Unit = synchronized {
    super.retain(p)
  }

  override def toString() = synchronized {
    super.toString()
  }

  override def equals(that: Any): Boolean = synchronized {
    super.equals(that)
  }

  override def hashCode(): int = synchronized {
    super.hashCode()
  }

  override def <<(cmd: Message[(A, B)]): Unit = synchronized {
    super.<<(cmd)
  }

  override def clone(): Map[A, B] = synchronized {
    super.clone()
  }
}
