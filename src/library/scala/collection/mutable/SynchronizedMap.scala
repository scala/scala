/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable


/** This class should be used as a mixin. It synchronizes the <code>Map</code>
 *  functions of the class into which it is mixed in.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
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

  override def values: Iterator[B] = synchronized {
    super.values
  }

  override def foreach(f: Pair[A, B] => Unit) = synchronized {
    super.foreach(f)
  }

  override def toList: List[Pair[A, B]] = synchronized {
    super.toList
  }

  abstract override def update(key: A, value: B): Unit = synchronized {
    super.update(key, value)
  }

  override def ++=(map: Iterable[Pair[A, B]]): Unit = synchronized {
    super.++=(map)
  }

  override def ++=(it: Iterator[Pair[A, B]]): Unit = synchronized {
    super.++=(it)
  }

  override def incl(mappings: Pair[A, B]*): Unit = synchronized {
    super.++=(mappings)
  }

  abstract override def -=(key: A): Unit = synchronized {
    super.-=(key)
  }

  override def --=(keys: Iterable[A]): Unit = synchronized {
    super.--=(keys)
  }

  override def --=(it: Iterator[A]): Unit = synchronized {
    super.--=(it)
  }

  override def excl(keys: A*): Unit = synchronized {
    super.--=(keys)
  }

  override def clear: Unit = synchronized {
    super.clear
  }

  override def map(f: Pair[A, B] => B): Unit = synchronized {
    super.map(f)
  }

  override def filter(p: Pair[A, B] => Boolean): Unit = synchronized {
    super.filter(p)
  }

  override def toString() = synchronized {
    super.toString()
  }

  override def <<(cmd: Message[Pair[A, B]]): Unit = synchronized {
    super.<<(cmd)
  }

  override def clone(): Map[A, B] = synchronized {
    super.clone()
  }
}
