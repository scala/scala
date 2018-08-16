package scala.collection
package mutable

import scala.annotation.tailrec
import scala.collection.immutable.{List, Nil, ::}
import scala.annotation.tailrec
import java.lang.{IllegalArgumentException, IndexOutOfBoundsException}
import scala.runtime.Statics.releaseFence

/** A `Buffer` implementation backed by a list. It provides constant time
  *  prepend and append. Most other operations are linear.
  *
  *  @author  Matthias Zenger
  *  @author  Martin Odersky
  *  @since   1
  *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#list-buffers "Scala's Collection Library overview"]]
  *  section on `List Buffers` for more information.
  *
  *  @tparam A    the type of this list buffer's elements.
  *
  *  @define Coll `ListBuffer`
  *  @define coll list buffer
  *  @define orderDependent
  *  @define orderDependentFold
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */
class ListBuffer[A]
  extends AbstractBuffer[A]
     with SeqOps[A, ListBuffer, ListBuffer[A]]
     with StrictOptimizedSeqOps[A, ListBuffer, ListBuffer[A]]
     with ReusableBuilder[A, immutable.List[A]] {

  private var first: List[A] = Nil
  private var last0: ::[A] = null
  private[this] var aliased = false
  private[this] var len = 0

  private type Predecessor[A0] = ::[A0] /*| Null*/

  def iterator = first.iterator

  override def iterableFactory: SeqFactory[ListBuffer] = ListBuffer

  @throws[IndexOutOfBoundsException]
  def apply(i: Int) = first.apply(i)

  def length = len
  override def knownSize = len

  override def isEmpty: Boolean = len == 0

  private def copyElems(): Unit = {
    val buf = ListBuffer.from(this)
    first = buf.first
    last0 = buf.last0
    aliased = false
  }

  private def ensureUnaliased() = if (aliased) copyElems()

  // Avoids copying where possible.
  override def toList: List[A] = {
    aliased = nonEmpty
    // We've accumulated a number of mutations to `List.tail` by this stage.
    // Make sure they are visible to threads that the client of this ListBuffer might be about
    // to share this List with.
    releaseFence()
    first
  }

  def result(): immutable.List[A] = toList

  /** Prepends the elements of this buffer to a given list
    *
    *  @param xs   the list to which elements are prepended
    */
  def prependToList(xs: List[A]): List[A] = {
    if (isEmpty) xs
    else {
      ensureUnaliased()
      last0.next = xs
      toList
    }
  }

  def clear(): Unit = {
    first = Nil
    len = 0
    last0 = null
    aliased = false
  }

  def addOne(elem: A): this.type = {
    ensureUnaliased()
    val last1 = new ::[A](elem, Nil)
    if (len == 0) first = last1 else last0.next = last1
    last0 = last1
    len += 1
    this
  }

  override def subtractOne(elem: A): this.type = {
    ensureUnaliased()
    if (isEmpty) {}
    else if (first.head == elem) {
      first = first.tail
      reduceLengthBy(1)
    }
    else {
      var cursor = first
      while (!cursor.tail.isEmpty && cursor.tail.head != elem) {
        cursor = cursor.tail
      }
      if (!cursor.tail.isEmpty) {
        val z = cursor.asInstanceOf[::[A]]
        if (z.next == last0)
          last0 = z
        z.next = cursor.tail.tail
        reduceLengthBy(1)
      }
    }
    this
  }

  /** Reduce the length of the buffer, and null out last0
    *  if this reduces the length to 0.
    */
  private def reduceLengthBy(num: Int): Unit = {
    len -= num
    if (len <= 0)   // obviously shouldn't be < 0, but still better not to leak
      last0 = null
  }

  private def locate(i: Int): Predecessor[A] =
    if (i == 0) null
    else if (i == len) last0
    else {
      var j = i - 1
      var p = first
      while (j > 0) {
        p = p.tail
        j -= 1
      }
      p.asInstanceOf[Predecessor[A]]
    }

  private def getNext(p: Predecessor[A]): List[A] =
    if (p == null) first else p.next

  private def setNext(p: Predecessor[A], nx: List[A]): Unit = {
    if (p == null) first = nx else p.next = nx
    if (nx.isEmpty) last0 = p
    else {
      var l = nx.asInstanceOf[::[A]]
      while (l.next.nonEmpty) l = l.next.asInstanceOf[::[A]]
      last0 = l
    }
  }

  def update(idx: Int, elem: A): Unit = {
    ensureUnaliased()
    if (idx < 0 || idx >= len) throw new IndexOutOfBoundsException(idx.toString)
    val p = locate(idx)
    setNext(p, elem :: getNext(p).tail)
  }

  def insert(idx: Int, elem: A): Unit = {
    ensureUnaliased()
    if (idx < 0 || idx > len) throw new IndexOutOfBoundsException(idx.toString)
    if (idx == len) +=(elem)
    else {
      val p = locate(idx)
      setNext(p, elem :: getNext(p))
      len += 1
    }
  }

  def prepend(elem: A): this.type = {
    insert(0, elem)
    this
  }

  private def insertAfter(p: Predecessor[A], it: Iterator[A]): Predecessor[A] = {
    var prev = p
    val follow = getNext(prev)
    while (it.hasNext) {
      len += 1
      val next = (it.next() :: follow).asInstanceOf[::[A]]
      setNext(prev, next)
      prev = next
    }
    prev
  }

  def insertAll(idx: Int, elems: IterableOnce[A]): Unit = {
    ensureUnaliased()
    val it = elems.iterator
    if (it.hasNext) {
      ensureUnaliased()
      if (idx < 0 || idx > len) throw new IndexOutOfBoundsException(idx.toString)
      if (idx == len) ++=(elems)
      else insertAfter(locate(idx), it)
    }
  }

  def remove(idx: Int): A = {
    ensureUnaliased()
    if (idx < 0 || idx >= len) throw new IndexOutOfBoundsException(idx.toString)
    val p = locate(idx)
    val nx = getNext(p)
    setNext(p, nx.tail)
    len -= 1
    nx.head
  }

  def remove(idx: Int, count: Int): Unit =
    if (count > 0) {
      ensureUnaliased()
      if (idx < 0 || idx + count > len) throw new IndexOutOfBoundsException(idx.toString)
      removeAfter(locate(idx), count)
    } else if (count < 0) {
      throw new IllegalArgumentException("removing negative number of elements: " + count)
    }

  private def removeAfter(prev: Predecessor[A], n: Int) = {
    @tailrec def ahead(p: List[A], n: Int): List[A] =
      if (n == 0) p else ahead(p.tail, n - 1)
    setNext(prev, ahead(getNext(prev), n))
    len -= n
  }

  def mapInPlace(f: A => A): this.type = {
    ensureUnaliased()
    val buf = new ListBuffer[A]
    for (elem <- this) buf += f(elem)
    first = buf.first
    last0 = buf.last0
    this
  }

  def flatMapInPlace(f: A => IterableOnce[A]): this.type = {
    ensureUnaliased()
    var prev: Predecessor[A] = null
    var cur: List[A] = first
    while (!cur.isEmpty) {
      val follow = cur.tail
      setNext(prev, follow)
      len -= 1
      prev = insertAfter(prev, f(cur.head).iterator)
      cur = follow
    }
    this
  }

  def filterInPlace(p: A => Boolean): this.type = {
    ensureUnaliased()
    var prev: Predecessor[A] = null
    var cur: List[A] = first
    while (!cur.isEmpty) {
      val follow = cur.tail
      if (!p(cur.head)) {
        setNext(prev, follow)
        len -= 1
      } else {
        prev = cur.asInstanceOf[Predecessor[A]]
      }
      cur = follow
    }
    this
  }

  def patchInPlace(from: Int, patch: collection.Seq[A], replaced: Int): this.type = {
    val i = math.min(math.max(from, 0), length)
    val n = math.min(math.max(replaced, 0), length)
    ensureUnaliased()
    val p = locate(i)
    removeAfter(p, math.min(n, len - i))
    insertAfter(p, patch.iterator)
    this
  }

  override protected[this] def stringPrefix = "ListBuffer"

}

@SerialVersionUID(3L)
object ListBuffer extends StrictOptimizedSeqFactory[ListBuffer] {

  def from[A](coll: collection.IterableOnce[A]): ListBuffer[A] = new ListBuffer[A] ++= coll

  def newBuilder[A]: Builder[A, ListBuffer[A]] = new GrowableBuilder(empty[A])

  def empty[A]: ListBuffer[A] = new ListBuffer[A]
}
