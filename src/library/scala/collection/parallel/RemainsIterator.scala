/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection.parallel

import scala.collection.generic.Signalling
import scala.collection.generic.DelegatedSignalling
import scala.collection.generic.IdleSignalling
import scala.collection.mutable.Builder
import scala.collection.GenTraversableOnce
import scala.collection.parallel.immutable.repetition

private[collection] trait RemainsIterator[+T] extends Iterator[T] {
  /** The number of elements this iterator has yet to iterate.
   *  This method doesn't change the state of the iterator.
   */
  def remaining: Int

  /** For most collections, this is a cheap operation.
   *  Exceptions can override this method.
   */
  def isRemainingCheap = true
}

/** Augments iterators with additional methods, mostly transformers,
 *  assuming they iterate an iterable collection.
 *
 *  @tparam T         type of the elements iterated.
 */
private[collection] trait AugmentedIterableIterator[+T] extends RemainsIterator[T] {

  /* accessors */

  override def count(p: T => Boolean): Int = {
    var i = 0
    while (hasNext) if (p(next())) i += 1
    i
  }

  override def reduce[U >: T](op: (U, U) => U): U = {
    var r: U = next()
    while (hasNext) r = op(r, next())
    r
  }

  override def fold[U >: T](z: U)(op: (U, U) => U): U = {
    var r = z
    while (hasNext) r = op(r, next())
    r
  }

  override def sum[U >: T](implicit num: Numeric[U]): U = {
    var r: U = num.zero
    while (hasNext) r = num.plus(r, next())
    r
  }

  override def product[U >: T](implicit num: Numeric[U]): U = {
    var r: U = num.one
    while (hasNext) r = num.times(r, next())
    r
  }

  override def min[U >: T](implicit ord: Ordering[U]): T = {
    var r = next()
    while (hasNext) {
      val curr = next()
      if (ord.lteq(curr, r)) r = curr
    }
    r
  }

  override def max[U >: T](implicit ord: Ordering[U]): T = {
    var r = next()
    while (hasNext) {
      val curr = next()
      if (ord.gteq(curr, r)) r = curr
    }
    r
  }

  override def copyToArray[U >: T](array: Array[U], from: Int, len: Int) {
    var i = from
    val until = from + len
    while (i < until && hasNext) {
      array(i) = next()
      i += 1
    }
  }

  def reduceLeft[U >: T](howmany: Int, op: (U, U) => U): U = {
    var i = howmany - 1
    var u: U = next()
    while (i > 0 && hasNext) {
      u = op(u, next())
      i -= 1
    }
    u
  }

  /* transformers to combiners */

  def map2combiner[S, That](f: T => S, cb: Combiner[S, That]): Combiner[S, That] = {
    //val cb = pbf(repr)
    if (isRemainingCheap) cb.sizeHint(remaining)
    while (hasNext) cb += f(next())
    cb
  }

  def collect2combiner[S, That](pf: PartialFunction[T, S], cb: Combiner[S, That]): Combiner[S, That] = {
    //val cb = pbf(repr)
    val runWith = pf.runWith(cb += _)
    while (hasNext) {
      val curr = next()
      runWith(curr)
    }
    cb
  }

  def flatmap2combiner[S, That](f: T => GenTraversableOnce[S], cb: Combiner[S, That]): Combiner[S, That] = {
    //val cb = pbf(repr)
    while (hasNext) {
      val traversable = f(next()).seq
      if (traversable.isInstanceOf[Iterable[_]]) cb ++= traversable.asInstanceOf[Iterable[S]].iterator
      else cb ++= traversable
    }
    cb
  }

  def copy2builder[U >: T, Coll, Bld <: Builder[U, Coll]](b: Bld): Bld = {
    if (isRemainingCheap) b.sizeHint(remaining)
    while (hasNext) b += next
    b
  }

  def filter2combiner[U >: T, This](pred: T => Boolean, cb: Combiner[U, This]): Combiner[U, This] = {
    while (hasNext) {
      val curr = next()
      if (pred(curr)) cb += curr
    }
    cb
  }

  def filterNot2combiner[U >: T, This](pred: T => Boolean, cb: Combiner[U, This]): Combiner[U, This] = {
    while (hasNext) {
      val curr = next()
      if (!pred(curr)) cb += curr
    }
    cb
  }

  def partition2combiners[U >: T, This](pred: T => Boolean, btrue: Combiner[U, This], bfalse: Combiner[U, This]) = {
    while (hasNext) {
      val curr = next()
      if (pred(curr)) btrue += curr
      else bfalse += curr
    }
    (btrue, bfalse)
  }

  def take2combiner[U >: T, This](n: Int, cb: Combiner[U, This]): Combiner[U, This] = {
    cb.sizeHint(n)
    var left = n
    while (left > 0) {
      cb += next
      left -= 1
    }
    cb
  }

  def drop2combiner[U >: T, This](n: Int, cb: Combiner[U, This]): Combiner[U, This] = {
    drop(n)
    if (isRemainingCheap) cb.sizeHint(remaining)
    while (hasNext) cb += next
    cb
  }

  def slice2combiner[U >: T, This](from: Int, until: Int, cb: Combiner[U, This]): Combiner[U, This] = {
    drop(from)
    var left = scala.math.max(until - from, 0)
    cb.sizeHint(left)
    while (left > 0) {
      cb += next
      left -= 1
    }
    cb
  }

  def splitAt2combiners[U >: T, This](at: Int, before: Combiner[U, This], after: Combiner[U, This]) = {
    before.sizeHint(at)
    if (isRemainingCheap) after.sizeHint(remaining - at)
    var left = at
    while (left > 0) {
      before += next
      left -= 1
    }
    while (hasNext) after += next
    (before, after)
  }

  def takeWhile2combiner[U >: T, This](p: T => Boolean, cb: Combiner[U, This]) = {
    var loop = true
    while (hasNext && loop) {
      val curr = next()
      if (p(curr)) cb += curr
      else loop = false
    }
    (cb, loop)
  }

  def span2combiners[U >: T, This](p: T => Boolean, before: Combiner[U, This], after: Combiner[U, This]) = {
    var isBefore = true
    while (hasNext && isBefore) {
      val curr = next()
      if (p(curr)) before += curr
      else {
        if (isRemainingCheap) after.sizeHint(remaining + 1)
        after += curr
        isBefore = false
      }
    }
    while (hasNext) after += next
    (before, after)
  }

  def scanToArray[U >: T, A >: U](z: U, op: (U, U) => U, array: Array[A], from: Int) {
    var last = z
    var i = from
    while (hasNext) {
      last = op(last, next())
      array(i) = last
      i += 1
    }
  }

  def scanToCombiner[U >: T, That](startValue: U, op: (U, U) => U, cb: Combiner[U, That]) = {
    var curr = startValue
    while (hasNext) {
      curr = op(curr, next())
      cb += curr
    }
    cb
  }

  def scanToCombiner[U >: T, That](howmany: Int, startValue: U, op: (U, U) => U, cb: Combiner[U, That]) = {
    var curr = startValue
    var left = howmany
    while (left > 0) {
      curr = op(curr, next())
      cb += curr
      left -= 1
    }
    cb
  }

  def zip2combiner[U >: T, S, That](otherpit: RemainsIterator[S], cb: Combiner[(U, S), That]): Combiner[(U, S), That] = {
    if (isRemainingCheap && otherpit.isRemainingCheap) cb.sizeHint(remaining min otherpit.remaining)
    while (hasNext && otherpit.hasNext) {
      cb += ((next(), otherpit.next()))
    }
    cb
  }

  def zipAll2combiner[U >: T, S, That](that: RemainsIterator[S], thiselem: U, thatelem: S, cb: Combiner[(U, S), That]): Combiner[(U, S), That] = {
    if (isRemainingCheap && that.isRemainingCheap) cb.sizeHint(remaining max that.remaining)
    while (this.hasNext && that.hasNext) cb += ((this.next(), that.next()))
    while (this.hasNext) cb += ((this.next(), thatelem))
    while (that.hasNext) cb += ((thiselem, that.next()))
    cb
  }

}


private[collection] trait AugmentedSeqIterator[+T] extends AugmentedIterableIterator[T] {

  /** The exact number of elements this iterator has yet to iterate.
   *  This method doesn't change the state of the iterator.
   */
  def remaining: Int

  /* accessors */

  def prefixLength(pred: T => Boolean): Int = {
    var total = 0
    var loop = true
    while (hasNext && loop) {
      if (pred(next())) total += 1
      else loop = false
    }
    total
  }

  override def indexWhere(pred: T => Boolean): Int = {
    var i = 0
    var loop = true
    while (hasNext && loop) {
      if (pred(next())) loop = false
      else i += 1
    }
    if (loop) -1 else i
  }

  def lastIndexWhere(pred: T => Boolean): Int = {
    var pos = -1
    var i = 0
    while (hasNext) {
      if (pred(next())) pos = i
      i += 1
    }
    pos
  }

  def corresponds[S](corr: (T, S) => Boolean)(that: Iterator[S]): Boolean = {
    while (hasNext && that.hasNext) {
      if (!corr(next(), that.next())) return false
    }
    hasNext == that.hasNext
  }

  /* transformers */

  def reverse2combiner[U >: T, This](cb: Combiner[U, This]): Combiner[U, This] = {
    if (isRemainingCheap) cb.sizeHint(remaining)
    var lst = List[T]()
    while (hasNext) lst ::= next
    while (lst != Nil) {
      cb += lst.head
      lst = lst.tail
    }
    cb
  }

  def reverseMap2combiner[S, That](f: T => S, cb: Combiner[S, That]): Combiner[S, That] = {
    //val cb = cbf(repr)
    if (isRemainingCheap) cb.sizeHint(remaining)
    var lst = List[S]()
    while (hasNext) lst ::= f(next())
    while (lst != Nil) {
      cb += lst.head
      lst = lst.tail
    }
    cb
  }

  def updated2combiner[U >: T, That](index: Int, elem: U, cb: Combiner[U, That]): Combiner[U, That] = {
    //val cb = cbf(repr)
    if (isRemainingCheap) cb.sizeHint(remaining)
    var j = 0
    while (hasNext) {
      if (j == index) {
        cb += elem
        next()
      } else cb += next
      j += 1
    }
    cb
  }

}


/** Parallel iterators allow splitting and provide a `remaining` method to
 *  obtain the number of elements remaining in the iterator.
 *
 *  @tparam T          type of the elements iterated.
 */
trait IterableSplitter[+T]
extends AugmentedIterableIterator[T]
   with Splitter[T]
   with Signalling
   with DelegatedSignalling
{
self =>

  var signalDelegate: Signalling = IdleSignalling

  /** Creates a copy of this iterator. */
  def dup: IterableSplitter[T]

  def split: Seq[IterableSplitter[T]]

  def splitWithSignalling: Seq[IterableSplitter[T]] = {
    val pits = split
    pits foreach { _.signalDelegate = signalDelegate }
    pits
  }

  def shouldSplitFurther[S](coll: ParIterable[S], parallelismLevel: Int) = remaining > thresholdFromSize(coll.size, parallelismLevel)

  /** The number of elements this iterator has yet to traverse. This method
   *  doesn't change the state of the iterator.
   *
   *  This method is used to provide size hints to builders and combiners, and
   *  to approximate positions of iterators within a data structure.
   *
   *  '''Note''': This method may be implemented to return an upper bound on the number of elements
   *  in the iterator, instead of the exact number of elements to iterate.
   *  Parallel collections which have such iterators are called non-strict-splitter collections.
   *
   *  In that case, 2 considerations must be taken into account:
   *
   *    1) classes that inherit `ParIterable` must reimplement methods `take`, `drop`, `slice`, `splitAt`, `copyToArray`
   *       and all others using this information.
   *
   *    2) if an iterator provides an upper bound on the number of elements, then after splitting the sum
   *       of `remaining` values of split iterators must be less than or equal to this upper bound.
   */
  def remaining: Int

  protected def buildString(closure: (String => Unit) => Unit): String = {
    var output = ""
    def appendln(s: String) = output += s + "\n"
    closure(appendln)
    output
  }

  private[parallel] def debugInformation = {
    // can be overridden in subclasses
    "Parallel iterator: " + this.getClass
  }

  /* iterator transformers */

  class Taken(taken: Int) extends IterableSplitter[T] {
    var remaining = taken min self.remaining
    def hasNext = remaining > 0
    def next = { remaining -= 1; self.next() }
    def dup: IterableSplitter[T] = self.dup.take(taken)
    def split: Seq[IterableSplitter[T]] = takeSeq(self.split) { (p, n) => p.take(n) }
    protected[this] def takeSeq[PI <: IterableSplitter[T]](sq: Seq[PI])(taker: (PI, Int) => PI) = {
      val sizes = sq.scanLeft(0)(_ + _.remaining)
      val shortened = for ((it, (from, until)) <- sq zip (sizes.init zip sizes.tail)) yield
        if (until < remaining) it else taker(it, remaining - from)
      shortened filter { _.remaining > 0 }
    }
  }
  /** To lower "virtual class" boilerplate tax, implement creation
   *  in method and override this method in the subclass.
   */
  private[collection] def newTaken(until: Int): Taken = new Taken(until)
  private[collection] def newSliceInternal[U <: Taken](it: U, from1: Int): U = {
    var count = from1
    while (count > 0 && it.hasNext) {
      it.next
      count -= 1
    }
    it
  }
  /** Drop implemented as simple eager consumption. */
  override def drop(n: Int): IterableSplitter[T] = {
    var i = 0
    while (i < n && hasNext) {
      next()
      i += 1
    }
    this
  }
  override def take(n: Int): IterableSplitter[T] = newTaken(n)
  override def slice(from1: Int, until1: Int): IterableSplitter[T] = newSliceInternal(newTaken(until1), from1)

  class Mapped[S](f: T => S) extends IterableSplitter[S] {
    signalDelegate = self.signalDelegate
    def hasNext = self.hasNext
    def next = f(self.next())
    def remaining = self.remaining
    def dup: IterableSplitter[S] = self.dup map f
    def split: Seq[IterableSplitter[S]] = self.split.map { _ map f }
  }

  override def map[S](f: T => S) = new Mapped(f)

  class Appended[U >: T, PI <: IterableSplitter[U]](protected val that: PI) extends IterableSplitter[U] {
    signalDelegate = self.signalDelegate
    protected var curr: IterableSplitter[U] = self
    def hasNext = if (curr.hasNext) true else if (curr eq self) {
      curr = that
      curr.hasNext
    } else false
    def next = if (curr eq self) {
      hasNext
      curr.next()
    } else curr.next()
    def remaining = if (curr eq self) curr.remaining + that.remaining else curr.remaining
    protected def firstNonEmpty = (curr eq self) && curr.hasNext
    def dup: IterableSplitter[U] = self.dup.appendParIterable[U, PI](that)
    def split: Seq[IterableSplitter[U]] = if (firstNonEmpty) Seq(curr, that) else curr.split
  }

  def appendParIterable[U >: T, PI <: IterableSplitter[U]](that: PI) = new Appended[U, PI](that)

  class Zipped[S](protected val that: SeqSplitter[S]) extends IterableSplitter[(T, S)] {
    signalDelegate = self.signalDelegate
    def hasNext = self.hasNext && that.hasNext
    def next = (self.next(), that.next())
    def remaining = self.remaining min that.remaining
    def dup: IterableSplitter[(T, S)] = self.dup.zipParSeq(that)
    def split: Seq[IterableSplitter[(T, S)]] = {
      val selfs = self.split
      val sizes = selfs.map(_.remaining)
      val thats = that.psplit(sizes: _*)
      (selfs zip thats) map { p => p._1 zipParSeq p._2 }
    }
  }

  def zipParSeq[S](that: SeqSplitter[S]) = new Zipped(that)

  class ZippedAll[U >: T, S](protected val that: SeqSplitter[S], protected val thiselem: U, protected val thatelem: S)
  extends IterableSplitter[(U, S)] {
    signalDelegate = self.signalDelegate
    def hasNext = self.hasNext || that.hasNext
    def next = if (self.hasNext) {
      if (that.hasNext) (self.next(), that.next())
      else (self.next(), thatelem)
    } else (thiselem, that.next())

    def remaining = self.remaining max that.remaining
    def dup: IterableSplitter[(U, S)] = self.dup.zipAllParSeq(that, thiselem, thatelem)
    def split: Seq[IterableSplitter[(U, S)]] = {
      val selfrem = self.remaining
      val thatrem = that.remaining
      val thisit = if (selfrem < thatrem) self.appendParIterable[U, SeqSplitter[U]](repetition[U](thiselem, thatrem - selfrem).splitter) else self
      val thatit = if (selfrem > thatrem) that.appendParSeq(repetition(thatelem, selfrem - thatrem).splitter) else that
      val zipped = thisit zipParSeq thatit
      zipped.split
    }
  }

  def zipAllParSeq[S, U >: T, R >: S](that: SeqSplitter[S], thisElem: U, thatElem: R) = new ZippedAll[U, R](that, thisElem, thatElem)
}

/** Parallel sequence iterators allow splitting into arbitrary subsets.
 *
 *  @tparam T          type of the elements iterated.
 */
trait SeqSplitter[+T]
extends IterableSplitter[T]
   with AugmentedSeqIterator[T]
   with PreciseSplitter[T]
{
self =>
  def dup: SeqSplitter[T]
  def split: Seq[SeqSplitter[T]]
  def psplit(sizes: Int*): Seq[SeqSplitter[T]]

  override def splitWithSignalling: Seq[SeqSplitter[T]] = {
    val pits = split
    pits foreach { _.signalDelegate = signalDelegate }
    pits
  }

  def psplitWithSignalling(sizes: Int*): Seq[SeqSplitter[T]] = {
    val pits = psplit(sizes: _*)
    pits foreach { _.signalDelegate = signalDelegate }
    pits
  }

  /** The number of elements this iterator has yet to traverse. This method
   *  doesn't change the state of the iterator. Unlike the version of this method in the supertrait,
   *  method `remaining` in `ParSeqLike.this.ParIterator` must return an exact number
   *  of elements remaining in the iterator.
   *
   *  @return   an exact number of elements this iterator has yet to iterate
   */
  def remaining: Int

  /* iterator transformers */

  class Taken(tk: Int) extends super.Taken(tk) with SeqSplitter[T] {
    override def dup = super.dup.asInstanceOf[SeqSplitter[T]]
    override def split: Seq[SeqSplitter[T]] = super.split.asInstanceOf[Seq[SeqSplitter[T]]]
    def psplit(sizes: Int*): Seq[SeqSplitter[T]] = takeSeq(self.psplit(sizes: _*)) { (p, n) => p.take(n) }
  }
  override private[collection] def newTaken(until: Int): Taken = new Taken(until)
  override def take(n: Int): SeqSplitter[T] = newTaken(n)
  override def slice(from1: Int, until1: Int): SeqSplitter[T] = newSliceInternal(newTaken(until1), from1)

  class Mapped[S](f: T => S) extends super.Mapped[S](f) with SeqSplitter[S] {
    override def dup = super.dup.asInstanceOf[SeqSplitter[S]]
    override def split: Seq[SeqSplitter[S]] = super.split.asInstanceOf[Seq[SeqSplitter[S]]]
    def psplit(sizes: Int*): Seq[SeqSplitter[S]] = self.psplit(sizes: _*).map { _ map f }
  }

  override def map[S](f: T => S) = new Mapped(f)

  class Appended[U >: T, PI <: SeqSplitter[U]](it: PI) extends super.Appended[U, PI](it) with SeqSplitter[U] {
    override def dup = super.dup.asInstanceOf[SeqSplitter[U]]
    override def split: Seq[SeqSplitter[U]] = super.split.asInstanceOf[Seq[SeqSplitter[U]]]
    def psplit(sizes: Int*): Seq[SeqSplitter[U]] = if (firstNonEmpty) {
      val selfrem = self.remaining

      // split sizes
      var appendMiddle = false
      val szcum = sizes.scanLeft(0)(_ + _)
      val splitsizes = sizes.zip(szcum.init zip szcum.tail).flatMap { t =>
        val (sz, (from, until)) = t
        if (from < selfrem && until > selfrem) {
          appendMiddle = true
          Seq(selfrem - from, until - selfrem)
        } else Seq(sz)
      }
      val (selfszfrom, thatszfrom) = splitsizes.zip(szcum.init).span(_._2 < selfrem)
      val (selfsizes, thatsizes) = (selfszfrom map { _._1 }, thatszfrom map { _._1 })

      // split iterators
      val selfs = self.psplit(selfsizes: _*)
      val thats = that.psplit(thatsizes: _*)

      // appended last in self with first in rest if necessary
      if (appendMiddle) selfs.init ++ Seq(selfs.last.appendParSeq[U, SeqSplitter[U]](thats.head)) ++ thats.tail
      else selfs ++ thats
    } else curr.asInstanceOf[SeqSplitter[U]].psplit(sizes: _*)
  }

  def appendParSeq[U >: T, PI <: SeqSplitter[U]](that: PI) = new Appended[U, PI](that)

  class Zipped[S](ti: SeqSplitter[S]) extends super.Zipped[S](ti) with SeqSplitter[(T, S)] {
    override def dup = super.dup.asInstanceOf[SeqSplitter[(T, S)]]
    override def split: Seq[SeqSplitter[(T, S)]] = super.split.asInstanceOf[Seq[SeqSplitter[(T, S)]]]
    def psplit(szs: Int*) = (self.psplit(szs: _*) zip that.psplit(szs: _*)) map { p => p._1 zipParSeq p._2 }
  }

  override def zipParSeq[S](that: SeqSplitter[S]) = new Zipped(that)

  class ZippedAll[U >: T, S](ti: SeqSplitter[S], thise: U, thate: S) extends super.ZippedAll[U, S](ti, thise, thate) with SeqSplitter[(U, S)] {
    override def dup = super.dup.asInstanceOf[SeqSplitter[(U, S)]]
    private def patchem = {
      val selfrem = self.remaining
      val thatrem = that.remaining
      val thisit = if (selfrem < thatrem) self.appendParSeq[U, SeqSplitter[U]](repetition[U](thiselem, thatrem - selfrem).splitter) else self
      val thatit = if (selfrem > thatrem) that.appendParSeq(repetition(thatelem, selfrem - thatrem).splitter) else that
      (thisit, thatit)
    }
    override def split: Seq[SeqSplitter[(U, S)]] = {
      val (thisit, thatit) = patchem
      val zipped = thisit zipParSeq thatit
      zipped.split
    }
    def psplit(sizes: Int*): Seq[SeqSplitter[(U, S)]] = {
      val (thisit, thatit) = patchem
      val zipped = thisit zipParSeq thatit
      zipped.psplit(sizes: _*)
    }
  }

  override def zipAllParSeq[S, U >: T, R >: S](that: SeqSplitter[S], thisElem: U, thatElem: R) = new ZippedAll[U, R](that, thisElem, thatElem)

  def reverse: SeqSplitter[T] = {
    val pa = mutable.ParArray.fromTraversables(self).reverse
    new pa.ParArrayIterator {
      override def reverse = self
    }
  }

  class Patched[U >: T](from: Int, patch: SeqSplitter[U], replaced: Int) extends SeqSplitter[U] {
    signalDelegate = self.signalDelegate
    private[this] val trio = {
      val pits = self.psplit(from, replaced, self.remaining - from - replaced)
      (pits(0).appendParSeq[U, SeqSplitter[U]](patch)) appendParSeq pits(2)
    }
    def hasNext = trio.hasNext
    def next = trio.next
    def remaining = trio.remaining
    def dup = self.dup.patchParSeq(from, patch, replaced)
    def split = trio.split
    def psplit(sizes: Int*) = trio.psplit(sizes: _*)
  }

  def patchParSeq[U >: T](from: Int, patchElems: SeqSplitter[U], replaced: Int) = new Patched(from, patchElems, replaced)

}
