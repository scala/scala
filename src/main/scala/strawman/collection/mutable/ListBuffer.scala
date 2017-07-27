package strawman.collection
package mutable

import scala.annotation.unchecked.uncheckedVariance
import scala.{Any, Boolean, Int, Unit, throws}
import scala.Int._
import strawman.collection
import strawman.collection.immutable.{List, Nil, ::}
import scala.annotation.tailrec
import java.lang.IndexOutOfBoundsException
import scala.Predef.{assert, intWrapper}

/** Concrete collection type: ListBuffer */
class ListBuffer[A]
  extends GrowableSeq[A]
     with SeqOps[A, ListBuffer, ListBuffer[A]]
     with StrictOptimizedIterableOps[A, ListBuffer, ListBuffer[A]] {

  private var first: List[A] = Nil
  private var last0: ::[A] = null
  private var aliased = false
  private var len = 0

  private type Predecessor[A] = ::[A] /*| Null*/

  def iterator() = first.iterator()

  def iterableFactory = ListBuffer

  protected[this] def fromSpecificIterable(coll: collection.Iterable[A]): ListBuffer[A] = fromIterable(coll)

  @throws[IndexOutOfBoundsException]
  def apply(i: Int) = first.apply(i)

  def length = len
  override def knownSize = len

  protected[this] def newSpecificBuilder(): Builder[A, ListBuffer[A]] = ListBuffer.newBuilder()

  private def copyElems(): Unit = {
    val buf = ListBuffer.fromIterable(this)
    first = buf.first
    last0 = buf.last0
    aliased = false
  }

  private def ensureUnaliased() = if (aliased) copyElems()

  /** Convert to list; avoids copying where possible. */
  def toList = {
    aliased = true
    first
  }

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
  }

  def add(elem: A) = {
    ensureUnaliased()
    val last1 = (elem :: Nil).asInstanceOf[::[A]]
    if (len == 0) first = last1 else last0.next = last1
    last0 = last1
    len += 1
    this
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

  private def setNext(p: Predecessor[A], nx: List[A]): Unit =
    if (p == null) first = nx else p.next = nx

  def update(idx: Int, elem: A): Unit = {
    ensureUnaliased()
    if (idx < 0 || idx >= len) throw new IndexOutOfBoundsException
    val p = locate(idx)
    setNext(p, elem :: getNext(p).tail)
  }

  def insert(idx: Int, elem: A): Unit = {
    ensureUnaliased()
    if (idx < 0 || idx > len) throw new IndexOutOfBoundsException
    if (idx == len) +=(elem)
    else {
      val p = locate(idx)
      setNext(p, elem :: getNext(p))
      len += 1
    }
  }

  private def insertAfter(p: Predecessor[A], it: Iterator[A]) = {
    var prev = p
    val follow = getNext(prev)
    while (it.hasNext) {
      len += 1
      val next = (it.next() :: follow).asInstanceOf[::[A]]
      setNext(prev, next)
      prev = next
    }
  }

  def insertAll(idx: Int, elems: IterableOnce[A]): Unit = {
    ensureUnaliased()
    val it = elems.iterator()
    if (it.hasNext) {
      ensureUnaliased()
      if (idx < 0 || idx > len) throw new IndexOutOfBoundsException
      if (idx == len) ++=(elems)
      else insertAfter(locate(idx), it)
    }
  }

  def remove(idx: Int): A = {
    ensureUnaliased()
    if (idx < 0 || idx >= len) throw new IndexOutOfBoundsException
    len -= 1
    val p = locate(idx)
    val nx = getNext(p)
    setNext(p, nx.tail)
    nx.head
  }

  def remove(idx: Int, n: Int): Unit =
    if (n > 0) {
      ensureUnaliased()
      if (idx < 0 || idx + n > len) throw new IndexOutOfBoundsException
      removeAfter(locate(idx), n)
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
    val prev: Predecessor[A] = null
    var cur: List[A] = first
    while (!cur.isEmpty) {
      val follow = cur.tail
      setNext(prev, follow)
      len -= 1
      insertAfter(prev, f(cur.head).iterator())
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
      }
      prev = cur.asInstanceOf[Predecessor[A]]
      cur = follow
    }
    this
  }

  def patchInPlace(from: Int, patch: collection.Seq[A], replaced: Int): this.type = {
    ensureUnaliased()
    val p = locate(from)
    removeAfter(p, replaced `min` (length - from))
    insertAfter(p, patch.iterator())
    this
  }

  override def className = "ListBuffer"
}

object ListBuffer extends IterableFactory[ListBuffer] {

  def fromIterable[A](coll: collection.Iterable[A]): ListBuffer[A] = new ListBuffer[A] ++= coll

  def newBuilder[A](): Builder[A, ListBuffer[A]] = new GrowableBuilder(empty[A])

  def empty[A]: ListBuffer[A] = new ListBuffer[A]
}
