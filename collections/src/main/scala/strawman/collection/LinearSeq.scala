package strawman
package collection

import scala.{Any, Boolean, IndexOutOfBoundsException, Int, None, Option, Some, Unit, math, throws}
import scala.annotation.tailrec

/** Base trait for linearly accessed sequences that have efficient `head` and
  *  `tail` operations.
  *  Known subclasses: List, LazyList
  */
trait LinearSeq[+A] extends Seq[A] with LinearSeqOps[A, LinearSeq, LinearSeq[A]]

/** Base trait for linear Seq operations */
trait LinearSeqOps[+A, +CC[X] <: LinearSeq[X], +C <: LinearSeq[A]] extends Any with SeqOps[A, CC, C] {

  // To be overridden in implementations:
  def isEmpty: Boolean
  def head: A
  def tail: LinearSeq[A]

  // `iterator` is implemented in terms of `head` and `tail`
  def iterator() = new Iterator[A] {
    private[this] var current: Iterable[A] = toIterable
    def hasNext = !current.isEmpty
    def next() = { val r = current.head; current = current.tail; r }
  }

  def length: Int = {
    var these = toIterable
    var len = 0
    while (!these.isEmpty) {
      len += 1
      these = these.tail
    }
    len
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

  // Optimized version of `drop` that avoids copying
  override def drop(n: Int): C = {
    def loop(n: Int, s: Iterable[A]): C =
      if (n <= 0) s.asInstanceOf[C]
      // implicit contract to guarantee success of asInstanceOf:
      //   (1) coll is of type C[A]
      //   (2) The tail of a LinearSeq is of the same type as the type of the sequence itself
      // it's surprisingly tricky/ugly to turn this into actual types, so we
      // leave this contract implicit.
      else loop(n - 1, s.tail)
    loop(n, toIterable)
  }

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

  override def sameElements[B >: A](that: IterableOnce[B]): Boolean = that match {
    case that1: LinearSeq[B] =>
      // Probably immutable, so check reference identity first (it's quick anyway)
      (coll eq that1) || {
        var these: LinearSeq[A] = coll
        var those: LinearSeq[B] = that1
        while (!these.isEmpty && !those.isEmpty && these.head == those.head) {
          these = these.tail
          those = those.tail
        }
        these.isEmpty && those.isEmpty
      }
    case _ =>
      super.sameElements(that)
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
}
