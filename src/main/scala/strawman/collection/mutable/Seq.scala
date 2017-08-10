package strawman.collection.mutable

import scala.{Int, Long, Unit, Boolean, Array}
import strawman.collection
import strawman.collection.{IterableOnce, toNewSeq, toOldSeq}
import scala.Predef.intWrapper

trait Seq[A]
  extends Iterable[A]
    with collection.Seq[A]
    with SeqOps[A, Seq, Seq[A]]

trait SeqOps[A, +CC[X] <: Seq[X], +C]
  extends IterableOps[A, CC, C]
    with collection.SeqOps[A, CC, C]
    with Shrinkable[A] {

  def update(idx: Int, elem: A): Unit
  def insert(idx: Int, elem: A): Unit
  def insertAll(idx: Int, elems: IterableOnce[A]): Unit
  def remove(idx: Int): A
  def remove(from: Int, n: Int): Unit
  def patchInPlace(from: Int, patch: collection.Seq[A], replaced: Int): this.type

  // +=, ++=, clear inherited from Growable
  // Per remark of @ichoran, we should preferably not have these:
  //
  // def +=:(elem: A): this.type = { insert(0, elem); this }
  // def +=:(elem1: A, elem2: A, elems: A*): this.type = elem1 +=: elem2 +=: elems.toStrawman ++=: this
  // def ++=:(elems: IterableOnce[A]): this.type = { insertAll(0, elems); this }

  def dropInPlace(n: Int): this.type = { remove(0, n); this }
  def dropRightInPlace(n: Int): this.type = { remove(length - n, n); this }
  def takeInPlace(n: Int): this.type = { remove(n, length); this }
  def takeRightInPlace(n: Int): this.type = { remove(0, length - n); this }
  def sliceInPlace(start: Int, end: Int): this.type = takeInPlace(end).dropInPlace(start)

  def dropWhileInPlace(p: A => Boolean): this.type = {
    val idx = indexWhere(!p(_))
    if (idx < 0) { clear(); this } else dropInPlace(idx)
  }
  def takeWhileInPlace(p: A => Boolean): this.type = {
    val idx = indexWhere(!p(_))
    if (idx < 0) this else takeInPlace(idx)
  }
  def padToInPlace(len: Int, elem: A): this.type = {
    while (length < len) +=(elem)
    this
  }
}

trait IndexedOptimizedSeq[A] extends Seq[A] {

  def mapInPlace(f: A => A): this.type = {
    var i = 0
    val siz = size
    while (i < siz) { this(i) = f(this(i)); i += 1 }
    this
  }

  def flatMapInPlace(f: A => IterableOnce[A]): this.type = {
    // There's scope for a better implementation which copies elements in place.
    var i = 0
    val newElemss = new Array[IterableOnce[A]](size)
    while (i < size) { newElemss(i) = f(this(i)); i += 1 }
    clear()
    i = 0
    while (i < size) { ++=(newElemss(i)); i += 1 }
    this
  }

  def filterInPlace(p: A => Boolean): this.type = {
    var i = 0
    while (i < size && p(apply(i))) i += 1
    var j = 1
    while (i < size) {
      if (p(apply(i))) {
        this(j) = this(i)
        j += 1
      }
      i += 1
    }
    takeInPlace(j)
  }

  def patchInPlace(from: Int, patch: collection.Seq[A], replaced: Int): this.type = {
    val n = patch.length min replaced
    var i = 0
    while (i < n) { update(from + i, patch(i)); i += 1 }
    if (i < patch.length) insertAll(from + i, patch.iterator().drop(i))
    else if (i < replaced) remove(from + i, replaced - i)
    this
  }
}

