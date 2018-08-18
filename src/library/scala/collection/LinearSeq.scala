package scala
package collection

import scala.annotation.tailrec
import scala.language.higherKinds

/** Base trait for linearly accessed sequences that have efficient `head` and
  *  `tail` operations.
  *  Known subclasses: List, LazyList
  */
trait LinearSeq[+A] extends Seq[A] with LinearSeqOps[A, LinearSeq, LinearSeq[A]] {
  override protected[this] def stringPrefix: String = "LinearSeq"
}

@SerialVersionUID(3L)
object LinearSeq extends SeqFactory.Delegate[LinearSeq](immutable.List)

/** Base trait for linear Seq operations */
trait LinearSeqOps[+A, +CC[X] <: LinearSeq[X], +C <: LinearSeq[A] with LinearSeqOps[A, CC, C]] extends Any with SeqOps[A, CC, C] {

  // To be overridden in implementations:
  def isEmpty: Boolean
  def head: A
  def tail: C

  def iterator: Iterator[A] =
    if (knownSize == 0) Iterator.empty
    else new LinearSeqIterator[A](toSeq)

  def length: Int = {
    var these = coll
    var len = 0
    while (these.nonEmpty) {
      len += 1
      these = these.tail
    }
    len
  }

  override def last: A = {
    if (isEmpty) throw new NoSuchElementException("LinearSeq.last")
    else {
      var these = coll
      var scout = tail
      while (scout.nonEmpty) {
        these = scout
        scout = scout.tail
      }
      these.head
    }
  }

  override def lengthCompare(len: Int): Int = {
    @tailrec def loop(i: Int, xs: LinearSeq[A]): Int = {
      if (i == len)
        if (xs.isEmpty) 0 else 1
      else if (xs.isEmpty)
        -1
      else
        loop(i + 1, xs.tail)
    }
    if (len < 0) 1
    else loop(0, coll)
  }

  override def isDefinedAt(x: Int): Boolean = x >= 0 && lengthCompare(x) > 0

  // `apply` is defined in terms of `drop`, which is in turn defined in
  //  terms of `tail`.
  @throws[IndexOutOfBoundsException]
  override def apply(n: Int): A = {
    if (n < 0) throw new IndexOutOfBoundsException(n.toString)
    val skipped = drop(n)
    if (skipped.isEmpty) throw new IndexOutOfBoundsException(n.toString)
    skipped.head
  }

  override def foreach[U](f: A => U): Unit = {
    var these: LinearSeq[A] = coll
    while (!these.isEmpty) {
      f(these.head)
      these = these.tail
    }
  }

  override def forall(p: A => Boolean): Boolean = {
    var these: LinearSeq[A] = coll
    while (!these.isEmpty) {
      if (!p(these.head)) return false
      these = these.tail
    }
    true
  }

  override def exists(p: A => Boolean): Boolean = {
    var these: LinearSeq[A] = coll
    while (!these.isEmpty) {
      if (p(these.head)) return true
      these = these.tail
    }
    false
  }

  override def contains[A1 >: A](elem: A1): Boolean = {
    var these: LinearSeq[A] = coll
    while (!these.isEmpty) {
      if (these.head == elem) return true
      these = these.tail
    }
    false
  }

  override def find(p: A => Boolean): Option[A] = {
    var these: LinearSeq[A] = coll
    while (!these.isEmpty) {
      if (p(these.head)) return Some(these.head)
      these = these.tail
    }
    None
  }

  override def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var acc = z
    var these: LinearSeq[A] = coll
    while (!these.isEmpty) {
      acc = op(acc, these.head)
      these = these.tail
    }
    acc
  }

  override def sameElements[B >: A](that: IterableOnce[B]): Boolean = {
    @tailrec def linearSeqEq(a: LinearSeq[B], b: LinearSeq[B]): Boolean =
      (a eq b) || {
        if (a.nonEmpty && b.nonEmpty && a.head == b.head) {
          linearSeqEq(a.tail, b.tail)
        }
        else {
          a.isEmpty && b.isEmpty
        }
      }

    that match {
      case that: LinearSeq[B] => linearSeqEq(coll, that)
      case _ => super.sameElements(that)
    }
  }


  override def indexWhere(p: A => Boolean, from: Int): Int = {
    var i = math.max(from, 0)
    var these: LinearSeq[A] = this drop from
    while (these.nonEmpty) {
      if (p(these.head))
        return i

      i += 1
      these = these.tail
    }
    -1
  }

  override def lastIndexWhere(p: A => Boolean, end: Int): Int = {
    var i = 0
    var these: LinearSeq[A] = coll
    var last = -1
    while (!these.isEmpty && i <= end) {
      if (p(these.head)) last = i
      these = these.tail
      i += 1
    }
    last
  }

  /** $willForceEvaluation */
  override def tails: Iterator[C] =
    Iterator.iterate(coll)(_.tail).takeWhile(_.nonEmpty) ++ Iterator.single(newSpecificBuilder.result())
}

trait StrictOptimizedLinearSeqOps[+A, +CC[X] <: LinearSeq[X], +C <: LinearSeq[A] with StrictOptimizedLinearSeqOps[A, CC, C]] extends LinearSeqOps[A, CC, C] with StrictOptimizedSeqOps[A, CC, C] {
  // A more efficient iterator implementation than the default LinearSeqIterator
  override def iterator: Iterator[A] = new AbstractIterator[A] {
    private[this] var current: Iterable[A] = toIterable
    def hasNext = !current.isEmpty
    def next() = { val r = current.head; current = current.tail; r }
  }

  // Optimized version of `drop` that avoids copying
  override def drop(n: Int): C = {
    @tailrec def loop(n: Int, s: C): C =
      if (n <= 0 || s.isEmpty) s
      else loop(n - 1, s.tail)
    loop(n, coll)
  }

  override def dropWhile(p: A => Boolean): C = {
    @tailrec def loop(s: C): C =
      if (s.nonEmpty && p(s.head)) loop(s.tail)
      else s
    loop(coll)
  }
}

/** A specialized Iterator for LinearSeqs that is lazy enough for Stream and LazyList. This is accomplished by not
  * evaluating the tail after returning the current head.
  */
private[collection] final class LinearSeqIterator[A](coll: Seq[A]) extends AbstractIterator[A] {
  // A call-by-need cell
  private[this] final class LazyCell(st: => Seq[A]) { lazy val v = st }

  private[this] var these: LazyCell = new LazyCell(coll)

  def hasNext: Boolean = these.v.nonEmpty

  def next(): A =
    if (isEmpty) Iterator.empty.next()
    else {
      val cur    = these.v
      val result = cur.head
      these = new LazyCell(cur.tail)
      result
    }
}
