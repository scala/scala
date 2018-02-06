/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package strawman.collection
package mutable

import scala.{Unit, Int, Serializable, Option, NoSuchElementException, Boolean, IndexOutOfBoundsException}
import scala.Predef.require
import immutable.List

/**
  *  This class is used internally to represent mutable lists. It is the
  *  basis for the implementation of the class `Queue`.
  *
  *  @author  Matthias Zenger
  *  @author  Martin Odersky
  *  @version 2.8
  *  @since   1
  *  @define Coll `mutable.MutableList`
  *  @define coll mutable list
  *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#mutable_lists "Scala's Collection Library overview"]]
  *  section on `Mutable Lists` for more information.
  */
private[mutable] class MutableList[A]
  extends AbstractSeq[A]
    with LinearSeq[A]
    with StrictOptimizedSeqOps[A, MutableList, MutableList[A]]
    with LinearSeqOps[A, MutableList, MutableList[A]]
    with Builder[A, MutableList[A]]
    with Serializable
{
  protected var first0: LinkedList[A] = new LinkedList[A]
  protected var last0: LinkedList[A] = first0
  protected var len: Int = 0

  def iterableFactory: SeqFactory[MutableList] = MutableList
  protected[this] def fromSpecificIterable(coll: strawman.collection.Iterable[A]): MutableList[A] = iterableFactory.from(coll)
  protected[this] def newSpecificBuilder(): Builder[A, MutableList[A]] = iterableFactory.newBuilder()

  def filterInPlace(p: A => Boolean): this.type = {
    var these = first0
    while (these.nonEmpty) {
      if(!p(these.elem)) {
        these.elem = these.next.elem
        if(these.next eq last0) last0 = these
        these.next = these.next.next
        len -= 1
      } else these = these.next
    }
    this
  }

  def flatMapInPlace(f: A => IterableOnce[A]): this.type = {
    val l = flatMap(f)
    this.first0 = l.first0
    this.last0 = l.last0
    this.len = l.len
    this
  }

  def mapInPlace(f: A => A): this.type = { first0.mapInPlace(f); this }

  def insert(idx: Int, elem: A): Unit = insertAll(idx, LinkedList(elem))

  def insertAll(idx: Int, elems: IterableOnce[A]): Unit = {
    val loc = first0.drop(idx)
    if(loc.isEmpty) throw new IndexOutOfBoundsException(idx.toString)
    val it = elems.iterator()
    while(it.hasNext) {
      val elem = it.next()
      val l = LinkedList(elem)
      if(last0 eq loc.next) last0 = l
      loc.next = l
      len += 1
    }
  }

  def patchInPlace(from: Int, patch: strawman.collection.Seq[A], replaced: Int): this.type = {
    remove(from, replaced)
    insertAll(from, patch)
    this
  }

  def remove(idx: Int, count: Int): Unit = {
    var loc = first0.drop(idx)
    var i = 0
    while(i < count) {
      if(loc.isEmpty) throw new IndexOutOfBoundsException(idx.toString)
      loc.elem = loc.next.elem
      if(last0 == loc.next) last0 = loc.next.next
      loc.next = loc.next.next
      len -= 1
      loc = loc.next
    }
  }

  def remove(idx: Int): A = {
    val loc = first0.drop(idx)
    if(loc.isEmpty) throw new IndexOutOfBoundsException(idx.toString)
    val e = loc.elem
    loc.elem = loc.next.elem
    if(last0 eq loc.next) last0 = loc.next.next
    loc.next = loc.next.next
    len -= 1
    e
  }

  def subtract(elem: A): this.type = {
    var these = first0
    while (these.nonEmpty) {
      if(these.elem == elem) {
        these.elem = these.next.elem
        if(last0 eq these.next) last0 = these.next.next
        these.next = these.next.next
        len -= 1
        return this
      }
      these = these.next
    }
    this
  }

  def toQueue = new Queue(first0, last0, len)

  /** Is the list empty?
    */
  override def isEmpty = len == 0

  /** Returns the first element in this list
    */
  override def head: A = if (nonEmpty) first0.head else throw new NoSuchElementException

  /** Returns the rest of this list
    */
  override def tail: MutableList[A] = {
    val tl = new MutableList[A]
    tailImpl(tl)
    tl
  }

  protected final def tailImpl(tl: MutableList[A]): Unit = {
    require(nonEmpty, "tail of empty list")
    tl.first0 = first0.tail
    tl.len = len - 1
    tl.last0 = if (tl.len == 0) tl.first0 else last0
  }

  /** Prepends a single element to this list. This operation takes constant
    *  time.
    *  @param elem  the element to prepend.
    *  @return   this $coll.
    */
  def +=: (elem: A): this.type = { prependElem(elem); this }

  /** Returns the length of this list.
    */
  override def length: Int = len

  /** Returns the `n`-th element of this list.
    *  @throws IndexOutOfBoundsException if index does not exist.
    */
  override def apply(n: Int): A = first0.apply(n)

  /** Updates the `n`-th element of this list to a new value.
    *  @throws IndexOutOfBoundsException if index does not exist.
    */
  def update(n: Int, x: A): Unit = first0.update(n, x)

  /** Returns the `n`-th element of this list or `None`
    *  if index does not exist.
    */
  def get(n: Int): Option[A] = first0.get(n)

  protected def prependElem(elem: A): Unit = {
    first0 = new LinkedList[A](elem, first0)
    if (len == 0) last0 = first0
    len = len + 1
  }

  protected def appendElem(elem: A): Unit = {
    if (len == 0) {
      prependElem(elem)
    } else {
      last0.next = new LinkedList[A]
      last0 = last0.next
      last0.elem = elem
      last0.next = new LinkedList[A] // for performance, use sentinel `object` instead?
      len = len + 1
    }
  }

  /** Returns an iterator over up to `length` elements of this list.
    */
  override def iterator(): Iterator[A] = if (isEmpty) Iterator.empty else
    new AbstractIterator[A] {
      var elems   = first0
      var count   = len
      def hasNext = count > 0 && elems.nonEmpty
      def next()  = {
        if (!hasNext) throw new NoSuchElementException
        count = count - 1
        val e = elems.elem
        elems = if (count == 0) null else elems.next
        e
      }
    }

  override def last = {
    if (isEmpty) throw new NoSuchElementException("MutableList.empty.last")
    last0.elem
  }

  /** Returns an instance of [[scala.List]] containing the same
    *  sequence of elements.
    */
  override def toList: List[A] = first0.toList

  /** Returns the current list of elements as a linked List
    *  sequence of elements.
    */
  private[mutable] def toLinkedList: LinkedList[A] = first0

  /** Appends a single element to this buffer. This takes constant time.
    *
    *  @param elem  the element to append.
    */
  def add(elem: A): this.type = { appendElem(elem); this }

  def clear(): Unit = {
    first0 = new LinkedList[A]
    last0 = first0
    len = 0
  }

  def result(): MutableList[A] = this

  override def clone(): MutableList[A]  = {
    val bf = newSpecificBuilder()
    bf ++= this
    bf.result()
  }
}

private[mutable] object MutableList extends StrictOptimizedSeqFactory[MutableList] {
  def newBuilder[A](): Builder[A, MutableList[A]] = new MutableList[A]
  def empty[A]: MutableList[A] = new MutableList[A]
  def from[A](source: IterableOnce[A]): MutableList[A] = {
    val l = new MutableList[A]
    l ++= source
    l
  }
}
