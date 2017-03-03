/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala
package collection
package mutable

import script._

/** This class should be used as a mixin. It synchronizes the `Set`
 *  functions of the class into which it is mixed in.
 *
 *  @tparam A    type of the elements contained in this synchronized set.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 *  @since   1
 *  @define Coll `SynchronizedSet`
 *  @define coll synchronized set
 */
@deprecated("Synchronization via traits is deprecated as it is inherently unreliable. Consider java.util.concurrent.ConcurrentHashMap[A,Unit] as an alternative.", "2.11.0")
trait SynchronizedSet[A] extends Set[A] {
  abstract override def size: Int = synchronized {
    super.size
  }

  override def isEmpty: Boolean = synchronized {
    super.isEmpty
  }

  abstract override def contains(elem: A) = synchronized {
    super.contains(elem)
  }

  abstract override def +=(elem: A): this.type = synchronized[this.type] {
    super.+=(elem)
  }

  override def ++=(xs: TraversableOnce[A]): this.type = synchronized[this.type] {
    super.++=(xs)
  }

  abstract override def -=(elem: A): this.type = synchronized[this.type] {
    super.-=(elem)
  }

  override def --=(xs: TraversableOnce[A]): this.type = synchronized[this.type] {
    super.--=(xs)
  }

  override def update(elem: A, included: Boolean): Unit = synchronized {
    super.update(elem, included)
  }

  override def add(elem: A): Boolean = synchronized {
    super.add(elem)
  }

  override def remove(elem: A): Boolean = synchronized {
    super.remove(elem)
  }

  override def intersect(that: scala.collection.GenSet[A]) = synchronized {
    super.intersect(that)
  }

  abstract override def clear(): Unit = synchronized {
    super.clear()
  }

  override def subsetOf(that: scala.collection.GenSet[A]) = synchronized {
    super.subsetOf(that)
  }

  override def foreach[U](f: A => U) = synchronized {
    super.foreach(f)
  }

  override def retain(p: A => Boolean) = synchronized {
    super.retain(p)
  }

  override def toList: List[A] = synchronized {
    super.toList
  }

  override def toString = synchronized {
    super.toString
  }

  @deprecated("scripting is deprecated", "2.11.0")
  override def <<(cmd: Message[A]): Unit = synchronized {
    super.<<(cmd)
  }

  override def clone(): Self = synchronized {
    super.clone()
  }
}
