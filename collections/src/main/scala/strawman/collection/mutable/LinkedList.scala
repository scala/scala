/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package strawman.collection
package mutable

import scala.{Int, Unit, Serializable, Option, NoSuchElementException, IndexOutOfBoundsException, Some, None, Boolean}
import scala.Predef.require
import scala.annotation.tailrec

/** A linked list implementation which is used internally in MutableList and Queue.
  */
private[mutable] final class LinkedList[A]() extends AbstractSeq[A]
  with strawman.collection.LinearSeq[A]
  with LinearSeqOps[A, LinkedList, LinkedList[A]]
  with Growable[A]
  with Shrinkable[A]
  with Serializable { self =>

  /** Creates a new list. If the parameter next is null, the result is an empty list. Otherwise, the result is
    * a list with elem at the head, followed by the contents of next.
    *
    * Note that next is part of the new list, as opposed to the +: operator,
    * which makes a new copy of the original list.
    *
    * @example
    * {{{
    *     scala> val m = LinkedList(1)
    *     m: scala.collection.mutable.LinkedList[Int] = LinkedList(1)
    *
    *     scala> val n = new LinkedList[Int](2, m)
    *     n: scala.collection.mutable.LinkedList[Int] = LinkedList(2, 1)
    * }}}
    */
  def this(elem: A, next: LinkedList[A]) = {
    this()
    if (next != null) {
      this.elem = elem
      this.next = next
    }
  }

  var elem: A = _
  var next: LinkedList[A] = this

  def iterableFactory: SeqFactory[LinkedList] = LinkedList
  protected[this] def newSpecificBuilder(): Builder[A, LinkedList[A]] = LinkedList.newBuilder()
  protected[this] def fromSpecificIterable(coll: strawman.collection.Iterable[A]): LinkedList[A] = LinkedList.from(coll)

  def clear(): Unit = {
    elem = null.asInstanceOf[A]
    next = this
  }

  def addOne(elem: A): this.type = { append(LinkedList(elem)); this }

  // Members declared in strawman.collection.mutable.IterableOps
  def filterInPlace(p: A => Boolean): this.type = {
    var these = this
    while (these.nonEmpty) {
      if(!p(these.elem)) {
        these.elem = these.next.elem
        these.next = these.next.next
      } else these = these.next
    }
    this
  }

  def flatMapInPlace(f: A => IterableOnce[A]): this.type = {
    val l = flatMap(f)
    this.elem = l.elem
    this.next = l.next
    this
  }

  def mapInPlace(f: A => A): this.type = {
    var these = this
    while (these.nonEmpty) {
      these.elem = f(these.elem)
      these = these.next
    }
    this
  }

  def insert(idx: Int,elem: A): Unit = atLocation(idx)(_.insert(LinkedList(elem)))

  def insertAll(idx: Int, elems: IterableOnce[A]): Unit = atLocation(idx)(_.insert(LinkedList.from(elems)))

  def patchInPlace(from: Int, patch: strawman.collection.Seq[A], replaced: Int): this.type = {
    remove(from, replaced)
    insertAll(from, patch)
    this
  }

  def remove(idx: Int, count: Int): Unit = {
    var i = 0
    while(i < count) {
      remove(idx)
      i += 1
    }
  }

  def remove(idx: Int): A = atLocation(idx) { l =>
    val e = l.elem
    l.elem = l.next.elem
    l.next = l.next.next
    e
  }

  def subtractOne(elem: A): this.type = {
    var these = this
    while (these.nonEmpty) {
      if(these.elem == elem) {
        these.elem = these.next.elem
        these.next = these.next.next
        return this
      }
      these = these.next
    }
    this
  }

  override def isEmpty = next eq this

  /** Determines the length of this $coll by traversing and counting every
    * node.
    */
  override def length: Int = size0(this, 0)

  @tailrec private def size0(elem: LinkedList[A], acc: Int): Int =
    if (elem.isEmpty) acc else size0(elem.next, acc + 1)

  override def head: A =
    if (isEmpty) throw new NoSuchElementException
    else elem

  override def tail: LinkedList[A] = {
    require(nonEmpty, "tail of empty list")
    next
  }

  /** If `this` is empty then it does nothing and returns `that`. Otherwise, appends `that` to `this`. The append
    * requires a full traversal of `this`.
    *
    * Examples:
    *
    * {{{
    *      scala> val a = LinkedList(1, 2)
    *      a: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2)
    *
    *      scala> val b = LinkedList(1, 2)
    *      b: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2)
    *
    *      scala> a.append(b)
    *      res0: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2, 1, 2)
    *
    *      scala> println(a)
    *      LinkedList(1, 2, 1, 2)
    * }}}
    *
    * {{{
    *    scala> val a = new LinkedList[Int]()
    *    a: scala.collection.mutable.LinkedList[Int] = LinkedList()
    *
    *    scala> val b = LinkedList(1, 2)
    *    b: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2)
    *
    *    scala> val c = a.append(b)
    *    c: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2)
    *
    *    scala> println(a)
    *    LinkedList()
    * }}}
    *
    *  @return the list after append (this is the list itself if nonempty,
    *  or list `that` if list this is empty. )
    */
  def append(that: LinkedList[A]): LinkedList[A] = {
    @tailrec
    def loop(x: LinkedList[A]): Unit = {
      if (x.next.isEmpty) x.next = that
      else loop(x.next)
    }
    if (isEmpty) that
    else { loop(this); this }
  }

  /** Insert linked list `that` at current position of this linked list
    *  @note this linked list must not be empty
    */
  def insert(that: LinkedList[A]): Unit = {
    require(nonEmpty, "insert into empty list")
    if (that.nonEmpty) {
      that append next
      next = that
    }
  }

  override def drop(n: Int): LinkedList[A] = {
    var i = 0
    var these: LinkedList[A] = this
    while (i < n && !these.isEmpty) {
      these = these.next
      i += 1
    }
    these
  }

  private def atLocation[T](n: Int)(f: LinkedList[A] => T): T = {
    val loc = drop(n)
    if (loc.nonEmpty) f(loc)
    else throw new IndexOutOfBoundsException(n.toString)
  }

  override def apply(n: Int): A   = atLocation(n)(_.elem)
  def update(n: Int, x: A): Unit  = atLocation(n)(_.elem = x)

  def get(n: Int): Option[A] = {
    val loc = drop(n)
    if (loc.nonEmpty) Some(loc.elem)
    else None
  }

  override def iterator(): Iterator[A] = new AbstractIterator[A] {
    var elems = self
    def hasNext = elems.nonEmpty
    def next() = {
      val res = elems.elem
      elems = elems.next
      res
    }
  }

  override def foreach[U](f: A => U): Unit = {
    var these = this
    while (these.nonEmpty) {
      f(these.elem)
      these = these.next
    }
  }

  /** Return a clone of this list.
    *
    *  @return a `LinkedList` with the same elements.
    */
  override def clone(): LinkedList[A] = {
    val bf = newSpecificBuilder()
    bf ++= this
    bf.result()
  }
}

private[mutable] object LinkedList extends StrictOptimizedSeqFactory[LinkedList] {
  def from[A](coll: IterableOnce[A]): LinkedList[A] = new LinkedList[A] ++= coll
  def newBuilder[A](): Builder[A, LinkedList[A]] =
    (new MutableList) mapResult ((l: MutableList[A]) => l.toLinkedList)
  def empty[A]: LinkedList[A] = new LinkedList[A]
}
