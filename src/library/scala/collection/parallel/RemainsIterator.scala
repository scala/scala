package scala.collection.parallel



import scala.collection.Parallel
import scala.collection.generic.Signalling
import scala.collection.generic.DelegatedSignalling
import scala.collection.generic.CanCombineFrom
import scala.collection.mutable.Builder
import scala.collection.Iterator.empty


trait RemainsIterator[+T] extends Iterator[T] {
  /** The number of elements this iterator has yet to iterate.
   *  This method doesn't change the state of the iterator.
   */
  def remaining: Int
}


/** Augments iterators with additional methods, mostly transformers,
 *  assuming they iterate an iterable collection.
 *
 *  @param T       type of the elements iterated.
 */
trait AugmentedIterableIterator[+T] extends RemainsIterator[T] {

  /* accessors */

  override def count(p: T => Boolean): Int = {
    var i = 0
    while (hasNext) if (p(next)) i += 1
    i
  }

  def reduce[U >: T](op: (U, U) => U): U = {
    var r: U = next
    while (hasNext) r = op(r, next)
    r
  }

  def fold[U >: T](z: U)(op: (U, U) => U): U = {
    var r = z
    while (hasNext) r = op(r, next)
    r
  }

  override def sum[U >: T](implicit num: Numeric[U]): U = {
    var r: U = num.zero
    while (hasNext) r = num.plus(r, next)
    r
  }

  override def product[U >: T](implicit num: Numeric[U]): U = {
    var r: U = num.one
    while (hasNext) r = num.times(r, next)
    r
  }

  override def min[U >: T](implicit ord: Ordering[U]): T = {
    var r = next
    while (hasNext) {
      val curr = next
      if (ord.lteq(curr, r)) r = curr
    }
    r
  }

  override def max[U >: T](implicit ord: Ordering[U]): T = {
    var r = next
    while (hasNext) {
      val curr = next
      if (ord.gteq(curr, r)) r = curr
    }
    r
  }

  override def copyToArray[U >: T](array: Array[U], from: Int, len: Int) {
    var i = from
    val until = from + len
    while (i < until && hasNext) {
      array(i) = next
      i += 1
    }
  }

  /* transformers to combiners */

  def map2combiner[S, That](f: T => S, cb: Combiner[S, That]): Combiner[S, That] = {
    //val cb = pbf(repr)
    cb.sizeHint(remaining)
    while (hasNext) cb += f(next)
    cb
  }

  def collect2combiner[S, That](pf: PartialFunction[T, S], cb: Combiner[S, That]): Combiner[S, That] = {
    //val cb = pbf(repr)
    while (hasNext) {
      val curr = next
      if (pf.isDefinedAt(curr)) cb += pf(curr)
    }
    cb
  }

  def flatmap2combiner[S, That](f: T => Traversable[S], cb: Combiner[S, That]): Combiner[S, That] = {
    //val cb = pbf(repr)
    while (hasNext) {
      val traversable = f(next)
      if (traversable.isInstanceOf[Iterable[_]]) cb ++= traversable.asInstanceOf[Iterable[S]].iterator
      else cb ++= traversable
    }
    cb
  }

  def copy2builder[U >: T, Coll, Bld <: Builder[U, Coll]](b: Bld): Bld = {
    b.sizeHint(remaining)
    while (hasNext) b += next
    b
  }

  def filter2combiner[U >: T, This](pred: T => Boolean, cb: Combiner[U, This]): Combiner[U, This] = {
    while (hasNext) {
      val curr = next
      if (pred(curr)) cb += curr
    }
    cb
  }

  def filterNot2combiner[U >: T, This](pred: T => Boolean, cb: Combiner[U, This]): Combiner[U, This] = {
    while (hasNext) {
      val curr = next
      if (!pred(curr)) cb += curr
    }
    cb
  }

  def partition2combiners[U >: T, This](pred: T => Boolean, btrue: Combiner[U, This], bfalse: Combiner[U, This]) = {
    while (hasNext) {
      val curr = next
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
    cb.sizeHint(remaining)
    while (hasNext) cb += next
    cb
  }

  def slice2combiner[U >: T, This](from: Int, until: Int, cb: Combiner[U, This]): Combiner[U, This] = {
    drop(from)
    var left = until - from
    cb.sizeHint(left)
    while (left > 0) {
      cb += next
      left -= 1
    }
    cb
  }

  def splitAt2combiners[U >: T, This](at: Int, before: Combiner[U, This], after: Combiner[U, This]) = {
    before.sizeHint(at)
    after.sizeHint(remaining - at)
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
      val curr = next
      if (p(curr)) cb += curr
      else loop = false
    }
    (cb, loop)
  }

  def span2combiners[U >: T, This](p: T => Boolean, before: Combiner[U, This], after: Combiner[U, This]) = {
    var isBefore = true
    while (hasNext && isBefore) {
      val curr = next
      if (p(curr)) before += curr
      else {
        after.sizeHint(remaining + 1)
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
      last = op(last, next)
      array(i) = last
      i += 1
    }
  }

}


trait AugmentedSeqIterator[+T] extends AugmentedIterableIterator[T] {

  /** The exact number of elements this iterator has yet to iterate.
   *  This method doesn't change the state of the iterator.
   */
  def remaining: Int

  /* accessors */

  def prefixLength(pred: T => Boolean): Int = {
    var total = 0
    var loop = true
    while (hasNext && loop) {
      if (pred(next)) total += 1
      else loop = false
    }
    total
  }

  override def indexWhere(pred: T => Boolean): Int = {
    var i = 0
    var loop = true
    while (hasNext && loop) {
      if (pred(next)) loop = false
      else i += 1
    }
    if (loop) -1 else i
  }

  def lastIndexWhere(pred: T => Boolean): Int = {
    var pos = -1
    var i = 0
    while (hasNext) {
      if (pred(next)) pos = i
      i += 1
    }
    pos
  }

  def corresponds[S](corr: (T, S) => Boolean)(that: Iterator[S]): Boolean = {
    while (hasNext && that.hasNext) {
      if (!corr(next, that.next)) return false
    }
    hasNext == that.hasNext
  }

  /* transformers */

  def reverse2combiner[U >: T, This](cb: Combiner[U, This]): Combiner[U, This] = {
    cb.sizeHint(remaining)
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
    cb.sizeHint(remaining)
    var lst = List[S]()
    while (hasNext) lst ::= f(next)
    while (lst != Nil) {
      cb += lst.head
      lst = lst.tail
    }
    cb
  }

  def updated2combiner[U >: T, That](index: Int, elem: U, cb: Combiner[U, That]): Combiner[U, That] = {
    //val cb = cbf(repr)
    cb.sizeHint(remaining)
    var j = 0
    while (hasNext) {
      if (j == index) {
        cb += elem
        next
      } else cb += next
      j += 1
    }
    cb
  }

  /** Iterator `otherpit` must have equal or more elements.
   */
  def zip2combiner[U >: T, S, That](otherpit: Iterator[S], cb: Combiner[(U, S), That]): Combiner[(U, S), That] = {
    //val cb = cbf(repr)
    cb.sizeHint(remaining)
    while (hasNext) {
      cb += ((next, otherpit.next))
    }
    cb
  }

}


trait ParIterableIterator[+T]
extends AugmentedIterableIterator[T]
   with Splitter[T]
   with Signalling
   with DelegatedSignalling
{
  def split: Seq[ParIterableIterator[T]]

  /** The number of elements this iterator has yet to traverse. This method
   *  doesn't change the state of the iterator.
   *
   *  This method is used to provide size hints to builders and combiners, and
   *  to approximate positions of iterators within a data structure.
   *
   *  '''Note''': This method may be implemented to return an upper bound on the number of elements
   *  in the iterator, instead of the exact number of elements to iterate.
   *
   *  In that case, 2 considerations must be taken into account:
   *
   *    1) classes that inherit `ParIterable` must reimplement methods `take`, `drop`, `slice`, `splitAt` and `copyToArray`.
   *
   *    2) if an iterator provides an upper bound on the number of elements, then after splitting the sum
   *       of `remaining` values of split iterators must be less than or equal to this upper bound.
   */
  def remaining: Int
}


trait ParSeqIterator[+T]
extends ParIterableIterator[T]
   with AugmentedSeqIterator[T]
   with PreciseSplitter[T]
{
  def split: Seq[ParSeqIterator[T]]
  def psplit(sizes: Int*): Seq[ParSeqIterator[T]]

  /** The number of elements this iterator has yet to traverse. This method
   *  doesn't change the state of the iterator. Unlike the version of this method in the supertrait,
   *  method `remaining` in `ParSeqLike.this.ParIterator` must return an exact number
   *  of elements remaining in the iterator.
   *
   *  @return   an exact number of elements this iterator has yet to iterate
   */
  def remaining: Int
}


































