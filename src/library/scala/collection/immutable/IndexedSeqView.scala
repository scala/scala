package scala.collection.immutable

import scala.collection.AnyConstr
import scala.language.higherKinds

/** An IndexedSeqView whose underlying collections are immutable */
trait IndexedSeqView[+A] extends collection.IndexedSeqView[A] with IndexedSeqOps[A, View, View[A]] with SeqView[A] { self =>

  override def view: IndexedSeqView[A] = this

  def prependedBy[B >: A](prefix: IndexedSeq[B]): IndexedSeqView[B] = IndexedSeqView.prependedBy(this, prefix)
  override def prepended[B >: A](elem: B): IndexedSeqView[B] = IndexedSeqView.prepended(this, elem)
  override def take(n: Int): IndexedSeqView[A] = IndexedSeqView.take(this, n)
  override def takeRight(n: Int): IndexedSeqView[A] = IndexedSeqView.takeRight(this, n)
  override def drop(n: Int): IndexedSeqView[A] = IndexedSeqView.drop(this, n)
  override def dropRight(n: Int): IndexedSeqView[A] = IndexedSeqView.dropRight(this, n)
  override def map[B](f: A => B): IndexedSeqView[B] = IndexedSeqView.map[A, B](this, f)
  override def reverse: IndexedSeqView[A] = IndexedSeqView.reverse(this)
  override def slice(from: Int, until: Int): IndexedSeqView[A] = IndexedSeqView.slice(this, from, until)
  override def appended[B >: A](elem: B): IndexedSeqView[B] = IndexedSeqView.appended(this, elem)
  def appendedBy[B >: A](suffix: IndexedSeq[B]): IndexedSeqView[B] = IndexedSeqView.appendedAll(this, suffix)
}

object IndexedSeqView {

  /** An `IndexedSeqOps` whose collection type and collection type constructor are unknown */
  type SomeIndexedSeqOps[A] = IndexedSeqOps[A, AnyConstr, _]

  def empty[A]: IndexedSeqView[A] = Empty

  def single[A](elem: A): IndexedSeqView[A] = new Single(elem)

  def id[A](underlying: SomeIndexedSeqOps[A]): IndexedSeqView[A] = new Id(underlying)

  def previouslyEvaluated[A](underlying: Vector[A]): IndexedSeqView[A] =
    if (underlying.isEmpty) Empty else new PreviouslyEvaluated(underlying)

  def take[A](underlying: SomeIndexedSeqOps[A], n: Int): IndexedSeqView[A] = slice(underlying, 0, n)

  def takeRight[A](underlying: SomeIndexedSeqOps[A], n: Int): IndexedSeqView[A] = {
    val len = underlying.length
    slice(underlying, len - n, len)
  }

  def drop[A](underlying: SomeIndexedSeqOps[A], n: Int): IndexedSeqView[A] = slice(underlying, n, underlying.length)

  def dropRight[A](underlying: SomeIndexedSeqOps[A], n: Int): IndexedSeqView[A] =
    slice(underlying, 0, underlying.length - n)

  def reverse[A](underlying: SomeIndexedSeqOps[A]): IndexedSeqView[A] =
    if (underlying.isEmpty) Empty else new Slice(Empty, underlying, 0, underlying.length, Empty, true, IndexedSeq.empty)

  def slice[A](underlying: SomeIndexedSeqOps[A], from: Int, until: Int): IndexedSeqView[A] = {
    val length = underlying.length
    val _from = from.max(0)
    val _until = until.max(_from).min(length)
    if (_from >= _until) Empty
    else new Slice(Empty, underlying, _from, _until, Empty, false, IndexedSeq.empty)
  }


  def map[A, B](underlying: SomeIndexedSeqOps[A], f: A => B): IndexedSeqView[B] = {
    val len = underlying.length
    if (len <= 0) Empty
    else new Slice(Empty, underlying, 0, len, Empty, false, IndexedSeq(f.asInstanceOf[Any => Any]))
  }

  def appended[A](underlying: SomeIndexedSeqOps[A], elem: A): IndexedSeqView[A] = {
    val len = underlying.length
    val _single = single(elem)
    if (len <= 0) _single else new Slice(Empty, underlying, 0, len, _single, false, IndexedSeq.empty)
  }

  def appendedAll[A](underlying: SomeIndexedSeqOps[A], suffix: IndexedSeq[A]): IndexedSeqView[A] = {
    val suffixLen = suffix.length
    val underlyingLen = underlying.length
    if (suffixLen <= 0) {
      if (underlyingLen <= 0) Empty else id(underlying)
    } else if (underlyingLen <= 0) {
      id(suffix)
    } else {
      new Slice[A, A](Empty, underlying, 0, underlyingLen, id(suffix), false, IndexedSeq.empty)
    }
  }

  def prepended[A](underlying: SomeIndexedSeqOps[A], elem: A): IndexedSeqView[A] = {
    val len = underlying.length
    if (len <= 0) single(elem) else
      new Slice[A, A](single(elem), underlying, 0, len, Empty, false, IndexedSeq.empty)
  }
  def prependedBy[A](underlying: SomeIndexedSeqOps[A], prefix: IndexedSeq[A]): IndexedSeqView[A] = {
    val prefixLen = prefix.length
    val underlyingLen = underlying.length
    if (prefixLen <= 0) {
      if (underlyingLen <= 0) Empty else id(underlying)
    } else if (underlyingLen <= 0) {
      id(prefix)
    } else {
      new Slice[A, A](id(prefix), underlying, 0, underlyingLen, Empty, false, IndexedSeq.empty)
    }
  }

  @SerialVersionUID(3L)
  private[scala] case object Empty extends AbstractIndexedSeqView[Nothing] {
    override def iterator = Iterator.empty
    override def apply(i: Int): Nothing = throw new IndexOutOfBoundsException(i.toString)
    override def length = 0
    override def prepended[B >: Nothing](elem: B) = single(elem)
    override def take(n: Int) = this
    override def takeRight(n: Int) = this
    override def drop(n: Int) = this
    override def dropRight(n: Int) = this
    override def map[B](f: Nothing => B) = this
    override def reverse = this
    override def slice(from: Int, until: Int) = this
    override def appended[B >: Nothing](elem: B) = single(elem)
  }

  @SerialVersionUID(3L)
  private[scala] final class Single[+A](underlying: A) extends AbstractIndexedSeqView[A] {
    override def apply(i: Int) = if (i == 0) underlying else throw new IndexOutOfBoundsException(i.toString)
    override def length = 1
    override def prepended[B >: A](elem: B) = new PreviouslyEvaluated(Vector(elem, underlying))
    override def take(n: Int) = if (n <= 0) Empty else this
    override def takeRight(n: Int) = take(n)
    override def drop(n: Int) = if (n > 0) Empty else this
    override def dropRight(n: Int) = drop(n)
    override def map[B](f: A => B) = new Slice(Empty, this, 0, 1, Empty, false, IndexedSeq(f.asInstanceOf[Any => Any]))
    override def reverse = this
    override def slice(from: Int, until: Int) = if (from > 0 || until < 1) Empty else this
    override def appended[B >: A](elem: B) = new PreviouslyEvaluated(Vector(underlying, elem))
  }

  /** In cases where a user constructs previously-evaluated elements, such as in the case of appending/prepending, etc,
    * it makes to store these elements in our own data structure which can perform appropriate optimizations. Namely,
    * storing repeated prepends/appends in a Vector rather than in the call stack.
    *
    * Note that we will not use this to represent a user-passed IndexedSeq, because it could potentially have much worse
    * prepend/append performance.
    *
    * All other operations are still completely lazy though, in keeping with the idea of a view.
    */
  @SerialVersionUID(3L)
  private[scala] final class PreviouslyEvaluated[+A](underlying: Vector[A]) extends AbstractIndexedSeqView[A] {
    override def apply(i: Int) = underlying(i)
    override def length = underlying.length
    override def prepended[B >: A](elem: B) = new PreviouslyEvaluated(underlying.prepended(elem))
    override def take(n: Int) = IndexedSeqView.take(underlying, n)
    override def takeRight(n: Int) = IndexedSeqView.takeRight(underlying, n)
    override def drop(n: Int) = IndexedSeqView.drop(underlying, n)
    override def dropRight(n: Int) = IndexedSeqView.dropRight(underlying, n)
    override def map[B](f: A => B) = IndexedSeqView.map(underlying, f)
    override def reverse = IndexedSeqView.reverse(underlying)
    override def slice(from: Int, until: Int) = IndexedSeqView.slice(underlying, from, until)
    override def appended[B >: A](elem: B) = new PreviouslyEvaluated(underlying.appended(elem))
  }

  @SerialVersionUID(3L)
  private[scala] final class Id[+A](underlying: SomeIndexedSeqOps[A])
    extends SeqView.Id(underlying) with IndexedSeqView[A] {
    override def prepended[B >: A](elem: B) = IndexedSeqView.prepended(underlying, elem)
    override def take(n: Int) = IndexedSeqView.take(underlying, n)
    override def takeRight(n: Int) = IndexedSeqView.takeRight(underlying, n)
    override def drop(n: Int) = IndexedSeqView.drop(underlying, n)
    override def dropRight(n: Int) = IndexedSeqView.dropRight(underlying, n)
    override def map[B](f: A => B) = IndexedSeqView.map(underlying, f)
    override def reverse = IndexedSeqView.reverse(underlying)
    override def slice(from: Int, until: Int) = IndexedSeqView.slice(underlying, from, until)
    override def appended[B >: A](elem: B) = IndexedSeqView.appended(underlying, elem)
  }

  /** A data structure that is the main implementation for [[IndexedSeqView]].
    *
    * The benefit of this data structure is that by tracking more information about the underlying data the view
    * represents, it is able to more intelligently perform transformations by:
    *   * avoiding wrapping layers, and in some cases even allocations:
    *       Most transformations do not add additional wrapping but instead rearrange underlying data in a slice
    *       of the same (or lower) depth
    *   * discarding data when possible
    *       When we are able to throw away data, such as during a .drop or .take transformation, we do so
    *   * combining repeated map transformations into a heap data structure, which can be traversed stack-safely
    *
    * It works by tracking three separate component collections. These are:
    *   underlying:
    *     The main collection this is a slice of.
    *     Offsets within the underlying collection are also maintained, these are `lo` and `hi`
    *   prefix: The IndexedSeqView of elements which precedes `underlying`, usually obtained through prepending
    *   suffix: The IndexedSeqView of elements which follows `underlying`, usually obtained through appending
    *
    * The overall picture is then:
    *
    * prefix                           underlying                          suffix
    * [*******************************][     *********************        ][*******************************]
    *                                  |----|lo
    *                                  |--------------------------|hi
    *
    *
    * Also tracked are:
    *   isReversed:
    *     a flag that tracks whether at read time, the elements should be read:
    *       right to left (true)
    *       left to right (false)
    *
    *     The flag will not be set, unless a previous `reverse` transformation has been applied
    *
    *   mapFunctions:
    *     a queue of functions, possibly of different types, that are applied to each element
    *     from left to right at read time. This is used as a stack-safe alternative to arbitrily
    *     nested functions during repeated calls to .map(f) transformations.
    *
    * @param underlying The main sequence of elements over which this slice manages a view of. Will not typically be
    *                   empty, but there is no breakage caused if it is empty
    * @param prefix the elements which precede the elements in underlying
    * @param from the offset from the left, of elements in underlying. This is the lowest index which is included
    * @param until the offset from the left, of elements in underlying. This is the lowest index which is EXCLUDED
    * @param suffix the elements which succeed the elements in underlying
    * @param isReversed if true, then this slice is to be read right-to-left. Otherwise, left-to-right
    * @param mapFunctions an IndexedSeq of functions which represent previous mapping transformations, to be applied
    *                     left-to-right, when this view is strictly evaluated.
    *
    *                     Care is taken to ensure that the first input type of the first map function
    *                     is of type `A`, and the last has output type `B`
    *
    * @tparam A the type of element in underlying
    * @tparam B the type of element this is a view of, which are obtained by mapping elements of type `A` through
    *           mapFunctions
    */
  @SerialVersionUID(3L)
  private[scala] final class Slice[+A, +B](
    prefix: IndexedSeqView[B],
    underlying: SomeIndexedSeqOps[A],
    from: Int,
    until: Int,
    suffix: IndexedSeqView[B],
    isReversed: Boolean,
    mapFunctions: IndexedSeq[Any => Any]) extends AbstractIndexedSeqView[B] {
    /** The actual low underlying offset */
    protected val lo = from max 0
    /** The actual high underlying offset */
    protected val hi = (until max 0) min underlying.length

    /** Length of prefix */
    protected val prefixLen = prefix.length
    /** Length of underlying */
    protected val len = (hi - lo) max 0
    /** Length of suffix */
    protected val suffixLen = suffix.length

    override def length = prefixLen + len + suffixLen

    @throws[IndexOutOfBoundsException]
    def apply(i: Int): B = {
      // the underlying index, after accounting for reversals
      val j = if(isReversed) length - i - 1 else i
      if (j < prefixLen) {
        prefix(j)
      } else if (j < prefixLen + len) {
        mapFunctions.foldLeft(underlying(lo + (j - prefixLen)) : Any)((arg, f) => f(arg)).asInstanceOf[B]
      } else {
        val postIndex = j - prefixLen - len
        if (postIndex < suffixLen) {
          suffix(postIndex)
        } else throw new IndexOutOfBoundsException(i.toString)
      }
    }

    override def take(n: Int): IndexedSeqView[B] = dropRight(length - n)

    override def takeRight(n: Int): IndexedSeqView[B] = drop(length - n)

    override def drop(n: Int): IndexedSeqView[B] = {
      val dropped = (n max 0) min length
      if (dropped == 0) {
        this
      } else if (dropped >= length) {
        Empty
      } else if (isReversed) {
        if (dropped <= suffixLen) {
          new Slice(prefix, underlying, lo, hi, suffix.dropRight(dropped), isReversed, mapFunctions)
        } else if (dropped <= suffixLen + len) {
          val newHi = hi - (dropped - suffixLen)
          if (newHi <= lo) {
            prefix.reverse
          } else {
            new Slice(prefix, underlying, lo, newHi, Empty, isReversed, mapFunctions)
          }
        } else {
          prefix.dropRight(dropped - suffixLen - len).reverse
        }
      } else {
        if (dropped <= prefixLen) {
          new Slice(prefix.drop(dropped), underlying, lo, hi, suffix, isReversed, mapFunctions)
        } else if (dropped <= prefixLen + len) {
          val newLo = lo + (dropped - prefixLen)
          if (newLo >= hi) {
            suffix
          } else {
            new Slice(Empty, underlying, newLo, hi, suffix, isReversed, mapFunctions)
          }
        } else {
          suffix.drop(dropped - prefixLen - len)
        }
      }
    }
    override def dropRight(n: Int): IndexedSeqView[B] =
      if (n <= 0) {
        this
      } else {
        val dropped = n.max(0).min(length)
        if (dropped >= length) Empty
        else if (isReversed) {
          if (dropped <= prefixLen) {
            new Slice(prefix.drop(dropped), underlying, lo, hi, suffix, isReversed, mapFunctions)
          } else if (dropped <= prefixLen + len) {
            val newLo = lo + (dropped - prefixLen)
            if (newLo >= hi) {
              suffix.reverse
            } else {
              new Slice(Empty, underlying, newLo, hi, suffix, isReversed, mapFunctions)
            }
          } else {
            suffix.drop(dropped - prefixLen - len).reverse
          }
        }
        else if (dropped <= suffixLen) new Slice(prefix, underlying, lo, hi, suffix.dropRight(dropped), isReversed, mapFunctions)
        else if (dropped <= suffixLen + len) {
          val newHi = hi - dropped + suffixLen
          if (newHi <= lo) {
            prefix
          } else {
            new Slice(prefix, underlying, lo, newHi, Empty, isReversed, mapFunctions)
          }
        } else {
          prefix.dropRight(dropped - suffixLen - len)
        }
      }

    override def reverse: IndexedSeqView[B] = new Slice(prefix, underlying, lo, hi, suffix, !isReversed, mapFunctions)

    override def slice(from: Int, until: Int): IndexedSeqView[B] = {
      val _from = from.max(0).min(length)
      val _until = until.max(0).min(length)
      val newLen = (_until - _from).max(0)

      if (newLen == 0) {
        Empty
      } else if (newLen >= length) {
        this
      } else if (isReversed) {
        if (_from >= suffixLen) {
          if (_from >= suffixLen + len) {
            if (_from >= length) {
              Empty
            } else {
              prefix.reverse.slice(_from - suffixLen - len, _until - suffixLen - len)
            }
          } else {
            if (_until <= suffixLen + len) {
              new Slice(Empty, underlying, hi - (_until - suffixLen), hi - (_from - suffixLen), Empty, isReversed, mapFunctions)
            } else {
              new Slice(prefix.takeRight(_until - (suffixLen + len)), underlying, lo, hi - (from - suffixLen), Empty, isReversed, mapFunctions)
            }
          }
        } else {
          if (_until <= suffixLen) {
            suffix.reverse.slice(_from, _until)
          } else if (_until <= suffixLen + len){
            new Slice(Empty, underlying, hi - (_until - suffixLen), hi, suffix.dropRight(_from), isReversed, mapFunctions)
          } else {
            new Slice(prefix.takeRight(_until - suffixLen - len), underlying, lo, hi, suffix.take(suffixLen - _from), isReversed, mapFunctions)
          }
        }
      } else {
        if (_from >= prefixLen) {
          if (_from >= prefixLen + len) {
            if (_from >= length) {
              Empty
            } else {
              suffix.slice(_from - prefixLen - len, _until - prefixLen - len)
            }
          } else {
            if (_until <= prefixLen + len) {
              new Slice(Empty, underlying, _from - prefixLen, _until - prefixLen, Empty, isReversed, mapFunctions)
            } else {
              new Slice(Empty, underlying, lo + _from - prefixLen , hi, suffix.take(_until - prefixLen - len), isReversed, mapFunctions)
            }
          }
        } else {
          if (_until <= prefixLen) {
            prefix.slice(_from, _until)
          } else if (_until <= prefixLen + len){
            new Slice(prefix.drop(_from), underlying, lo, lo + _until - prefixLen, Empty, isReversed, mapFunctions)
          } else {
            new Slice(prefix.drop(_from), underlying, lo, hi, suffix.take(_until - prefixLen - len), isReversed, mapFunctions)
          }
        }
      }
    }

    override def map[B0](f: B => B0): IndexedSeqView[B0] =
      new Slice(prefix.map(f), underlying, lo, hi, suffix.map(f), isReversed, mapFunctions :+ f.asInstanceOf[Any => Any])

    override def prepended[B0 >: B](elem: B0): IndexedSeqView[B0] =
      if (isReversed) new Slice(prefix, underlying, lo, hi, suffix.appended(elem), isReversed, mapFunctions)
      else new Slice(prefix.prepended(elem), underlying, lo, hi, suffix, isReversed, mapFunctions)

    override def appended[B0 >: B](elem: B0): IndexedSeqView[B0] =
      if (isReversed) new Slice(prefix.prepended(elem), underlying, lo, hi, suffix, isReversed, mapFunctions)
      else new Slice(prefix, underlying, lo, hi, suffix.appended(elem), isReversed, mapFunctions)
  }

}

/** Explicit instantiation of the `IndexedSeqView` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractIndexedSeqView[+A] extends collection.AbstractIndexedSeqView[A] with IndexedSeqView[A]
