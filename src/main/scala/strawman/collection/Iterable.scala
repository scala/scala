package strawman
package collection

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.ClassTag
import scala.{Any, Array, Boolean, `inline`, Int, Numeric, Ordering, StringContext, Unit}
import java.lang.{String, UnsupportedOperationException}

import strawman.collection.mutable.{ArrayBuffer, Builder, StringBuilder}
import java.lang.String

/** Base trait for generic collections */
trait Iterable[+A] extends IterableOnce[A] with IterableOps[A, Iterable, Iterable[A]] {

  /** The collection itself */
  protected def coll: this.type = this
}

/** Base trait for Iterable operations
  *
  *  VarianceNote
  *  ============
  *
  *  We require that for all child classes of Iterable the variance of
  *  the child class and the variance of the `C` parameter passed to `IterableOps`
  *  are the same. We cannot express this since we lack variance polymorphism. That's
  *  why we have to resort at some places to write `C[A @uncheckedVariance]`.
  */
trait IterableOps[+A, +CC[X], +C] extends Any {

  protected def coll: Iterable[A]

  protected[this] def fromSpecificIterable(coll: Iterable[A]): C

  def iterableFactory: IterableFactory[CC]

  protected[this] def fromIterable[E](it: Iterable[E]): CC[E] = iterableFactory.fromIterable(it)

  /** Apply `f` to each element for its side effects
   *  Note: [U] parameter needed to help scalac's type inference.
   */
  def foreach[U](f: A => U): Unit = coll.iterator().foreach(f)

  def forall(p: A => Boolean): Boolean = coll.iterator().forall(p)

  /** Fold left */
  def foldLeft[B](z: B)(op: (B, A) => B): B = coll.iterator().foldLeft(z)(op)

  /** Fold right */
  def foldRight[B](z: B)(op: (A, B) => B): B = coll.iterator().foldRight(z)(op)

  /** The index of the first element in this collection for which `p` holds. */
  def indexWhere(p: A => Boolean): Int = coll.iterator().indexWhere(p)

  /** Is the collection empty? */
  def isEmpty: Boolean = !coll.iterator().hasNext

  /** Is the collection not empty? */
  def nonEmpty: Boolean = coll.iterator().hasNext

  /** The first element of the collection. */
  def head: A = coll.iterator().next()

  /** Selects the last element.
    * $orderDependent
    * @return The last element of this $coll.
    * @throws NoSuchElementException If the $coll is empty.
    */
  def last: A = {
    val it = coll.iterator()
    var lst = it.next()
    while (it.hasNext) lst = it.next()
    lst
  }

  /** The number of elements in this collection, if it can be cheaply computed,
    *  -1 otherwise. Cheaply usually means: Not requiring a collection traversal.
    */
  def knownSize: Int = -1

  /** The number of elements in this collection. Does not terminate for
    *  infinite collections.
    */
  def size: Int = if (knownSize >= 0) knownSize else coll.iterator().length

  /** A view representing the elements of this collection. */
  def view: View[A] = View.fromIterator(coll.iterator())

  /** Given a collection factory `fi`, convert this collection to the appropriate
    * representation for the current element type `A`. Example uses:
    *
    *      xs.to(List)
    *      xs.to(ArrayBuffer)
    *      xs.to(BitSet) // for xs: Iterable[Int]
    */
  def to[C](f: FromSpecificIterable[A, C]): C = f.fromSpecificIterable(coll)

  /** Convert collection to array. */
  def toArray[B >: A: ClassTag]: Array[B] =
    if (knownSize >= 0) copyToArray(new Array[B](knownSize), 0)
    else ArrayBuffer.fromIterable(coll).toArray[B]

  /** Copy all elements of this collection to array `xs`, starting at `start`. */
  def copyToArray[B >: A](xs: Array[B], start: Int = 0): xs.type = {
    var i = start
    val it = coll.iterator()
    while (it.hasNext) {
      xs(i) = it.next()
      i += 1
    }
    xs
  }

  /** The class name of this collection. To be used for converting to string.
    *  Collections generally print like this:
    *
    *       <className>(elem_1, ..., elem_n)
    */
  def className = getClass.getName

  /** A string showing all elements of this collection, separated by string `sep`. */
  def mkString(start: String, sep: String, end: String): String = {
    var first: Boolean = true
    val b = new StringBuilder()
    b ++= start
    foreach { elem =>
      if (!first) b ++= sep
      first = false
      b ++= String.valueOf(elem)
    }
    b ++= end
    b.result()
  }

  def mkString(sep: String): String = mkString("", sep, "")

  def mkString: String = mkString("")

  override def toString = s"$className(${mkString(", ")})"


  /** Sums up the elements of this collection.
    *
    *   @param   num  an implicit parameter defining a set of numeric operations
    *                 which includes the `+` operator to be used in forming the sum.
    *   @tparam  B    the result type of the `+` operator.
    *   @return       the sum of all elements of this $coll with respect to the `+` operator in `num`.
    *
    *   @usecase def sum: A
    *     @inheritdoc
    *
    *     @return       the sum of all elements in this $coll of numbers of type `Int`.
    *     Instead of `Int`, any other type `T` with an implicit `Numeric[T]` implementation
    *     can be used as element type of the $coll and as result type of `sum`.
    *     Examples of such types are: `Long`, `Float`, `Double`, `BigInt`.
    *
    */
  def sum[B >: A](implicit num: Numeric[B]): B = foldLeft(num.zero)(num.plus)

  /** Selects all elements of this $coll which satisfy a predicate.
    *
    *  @param pred  the predicate used to test elements.
    *  @return      a new $coll consisting of all elements of this $coll that satisfy the given
    *               predicate `pred`. Their order may not be preserved.
    */
  def filter(pred: A => Boolean): C = fromSpecificIterable(View.Filter(coll, pred))

  /** Selects all elements of this $coll which do not satisfy a predicate.
    *
    *  @param pred  the predicate used to test elements.
    *  @return      a new $coll consisting of all elements of this $coll that do not satisfy the given
    *               predicate `pred`. Their order may not be preserved.
    */
  def filterNot(pred: A => Boolean): C = fromSpecificIterable(View.Filter(coll, (a: A) => !pred(a)))

  /** A pair of, first, all elements that satisfy prediacte `p` and, second,
    *  all elements that do not. Interesting because it splits a collection in two.
    *
    *  The default implementation provided here needs to traverse the collection twice.
    *  Strict collections have an overridden version of `partition` in `Buildable`,
    *  which requires only a single traversal.
    */
  def partition(p: A => Boolean): (C, C) = {
    val pn = View.Partition(coll, p)
    (fromSpecificIterable(pn.left), fromSpecificIterable(pn.right))
  }

  /** Splits this $coll into two at a given position.
    *  Note: `c splitAt n` is equivalent to (but possibly more efficient than)
    *         `(c take n, c drop n)`.
    *  $orderDependent
    *
    *  @param n the position at which to split.
    *  @return  a pair of ${coll}s consisting of the first `n`
    *           elements of this $coll, and the other elements.
    */
  def splitAt(n: Int): (C, C) = (take(n), drop(n))

  /** A collection containing the first `n` elements of this collection. */
  def take(n: Int): C = fromSpecificIterable(View.Take(coll, n))

  /** The rest of the collection without its `n` first elements. For
    *  linear, immutable collections this should avoid making a copy.
    */
  def drop(n: Int): C = fromSpecificIterable(View.Drop(coll, n))

  /** The rest of the collection without its first element. */
  def tail: C = {
    if (coll.isEmpty) throw new UnsupportedOperationException
    drop(1)
  }

  /** Selects an interval of elements.  The returned collection is made up
    *  of all elements `x` which satisfy the invariant:
    *  {{{
    *    from <= indexOf(x) < until
    *  }}}
    *  $orderDependent
    *
    *  @param from   the lowest index to include from this $coll.
    *  @param until  the lowest index to EXCLUDE from this $coll.
    *  @return  a $coll containing the elements greater than or equal to
    *           index `from` extending up to (but not including) index `until`
    *           of this $coll.
    */
  def slice(from: Int, until: Int): C =
    fromSpecificIterable(View.Take(View.Drop(coll, from), until - from))


  /** Map */
  def map[B](f: A => B): CC[B] = fromIterable(View.Map(coll, f))

  /** Flatmap */
  def flatMap[B](f: A => IterableOnce[B]): CC[B] = fromIterable(View.FlatMap(coll, f))

  /** Returns a new $coll containing the elements from the left hand operand followed by the elements from the
    *  right hand operand. The element type of the $coll is the most specific superclass encompassing
    *  the element types of the two operands.
    *
    *  @param xs   the traversable to append.
    *  @tparam B   the element type of the returned collection.
    *  @return     a new collection of type `CC[B]` which contains all elements
    *              of this $coll followed by all elements of `xs`.
    */
  def concat[B >: A](xs: IterableOnce[B]): CC[B] = fromIterable(View.Concat(coll, xs))

  /** Alias for `concat` */
  @`inline` final def ++ [B >: A](xs: IterableOnce[B]): CC[B] = concat(xs)

  /** Zip. Interesting because it requires to align to source collections. */
  def zip[B](xs: IterableOnce[B]): CC[(A @uncheckedVariance, B)] = fromIterable(View.Zip(coll, xs))
  // sound bcs of VarianceNote
}

/** Base trait for strict collections that can be built using a builder.
  * @tparam  A    the element type of the collection
  * @tparam C  the type of the underlying collection
  */
trait Buildable[+A, +C] extends Any with IterableOps[A, AnyConstr, C]  {

  /** Creates a new builder. */
  protected[this] def newBuilder: Builder[A, C]

  /** Optimized, push-based version of `partition`. */
  override def partition(p: A => Boolean): (C, C) = {
    val l, r = newBuilder
    coll.iterator().foreach(x => (if (p(x)) l else r) += x)
    (l.result(), r.result())
  }

  // one might also override other transforms here to avoid generating
  // iterators if it helps efficiency.
}


