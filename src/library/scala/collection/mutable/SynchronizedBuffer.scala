/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package mutable

import script._

/** This class should be used as a mixin. It synchronizes the `Buffer`
 *  methods of the class into which it is mixed in.
 *
 *  @tparam A    type of the elements contained in this buffer.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 *  @since   1
 *  @define Coll `SynchronizedBuffer`
 *  @define coll synchronized buffer
 */
@deprecated("Synchronization via traits is deprecated as it is inherently unreliable. Consider java.util.concurrent.ConcurrentLinkedQueue as an alternative.", "2.11.0")
trait SynchronizedBuffer[A] extends Buffer[A] {

  import scala.collection.Traversable

  abstract override def length: Int = synchronized {
    super.length
  }

  abstract override def iterator: Iterator[A] = synchronized {
    super.iterator
  }

  abstract override def apply(n: Int): A = synchronized {
    super.apply(n)
  }

  /** Append a single element to this buffer.
   *
   *  @param elem  the element to append.
   */
  abstract override def +=(elem: A): this.type = synchronized[this.type] {
    super.+=(elem)
  }

  /** Appends a number of elements provided by a traversable object via
   *  its `foreach` method.
   *  The identity of the buffer is returned.
   *
   *  @param xs the traversable object.
   */
  override def ++(xs: GenTraversableOnce[A]): Self = synchronized {
    super.++(xs)
  }

  /** Appends a number of elements provided by a traversable object
   *  via its `foreach` method.
   *
   *  @param xs   the iterable object.
   */
  override def ++=(xs: TraversableOnce[A]): this.type = synchronized[this.type] {
    super.++=(xs)
  }

  /** Appends a sequence of elements to this buffer.
   *
   *  @param elems  the elements to append.
   */
  override def append(elems: A*): Unit = synchronized {
    super.++=(elems)
  }

  /** Appends a number of elements provided by a traversable object
   *  via its `foreach` method.
   *
   *  @param xs the traversable object.
   */
  override def appendAll(xs: TraversableOnce[A]): Unit = synchronized {
    super.appendAll(xs)
  }

  /** Prepend a single element to this buffer and return
   *  the identity of the buffer.
   *
   *  @param elem  the element to append.
   */
  abstract override def +=:(elem: A): this.type = synchronized[this.type] {
    super.+=:(elem)
  }

  /** Prepends a number of elements provided by a traversable object
   *  via its `foreach` method. The identity of the buffer is returned.
   *
   *  @param xs the traversable object.
   */
  override def ++=:(xs: TraversableOnce[A]): this.type = synchronized[this.type] { super.++=:(xs) }

  /** Prepend an element to this list.
   *
   *  @param elems  the elements to prepend.
   */
  override def prepend(elems: A*): Unit = prependAll(elems)

  /** Prepends a number of elements provided by a traversable object
   *  via its `foreach` method. The identity of the buffer is returned.
   *
   *  @param xs the traversable object.
   */
  override def prependAll(xs: TraversableOnce[A]): Unit = synchronized {
    super.prependAll(xs)
  }

  /** Inserts new elements at the index `n`. Opposed to method `update`,
   *  this method will not replace an element with a one.
   *  Instead, it will insert the new elements at index `n`.
   *
   *  @param n      the index where a new element will be inserted.
   *  @param elems  the new elements to insert.
   */
  override def insert(n: Int, elems: A*): Unit = synchronized {
    super.insertAll(n, elems)
  }

  /** Inserts new elements at the index `n`. Opposed to method `update`,
   *  this method will not replace an element with a one.
   *  Instead, it will insert a new element at index `n`.
   *
   *  @param n     the index where a new element will be inserted.
   *  @param xs    the traversable object providing all elements to insert.
   */
  abstract override def insertAll(n: Int, xs: Traversable[A]): Unit = synchronized {
     super.insertAll(n, xs)
  }

  /** Replace element at index `n` with the new element `newelem`.
   *
   *  @param n       the index of the element to replace.
   *  @param newelem the new element.
   */
  abstract override def update(n: Int, newelem: A): Unit = synchronized {
    super.update(n, newelem)
  }

  /** Removes the element on a given index position.
   *
   *  @param n  the index which refers to the element to delete.
   */
  abstract override def remove(n: Int): A = synchronized {
    super.remove(n)
  }

  /** Clears the buffer contents.
   */
  abstract override def clear(): Unit = synchronized {
    super.clear()
  }

  @deprecated("scripting is deprecated", "2.11.0")
  override def <<(cmd: Message[A]): Unit = synchronized {
    super.<<(cmd)
  }

  /** Return a clone of this buffer.
   *
   *  @return an `ArrayBuffer` with the same elements.
   */
  override def clone(): Self = synchronized {
    super.clone()
  }

  /** The `hashCode` method always yields an error, since it is not
   *  safe to use buffers as keys in hash tables.
   *
   *  @return never.
   */
  override def hashCode(): Int = synchronized {
    super.hashCode()
  }
}
