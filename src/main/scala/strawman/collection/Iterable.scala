package strawman
package collection

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.ClassTag
import scala.{Any, Array, Boolean, Int, Numeric, StringContext, Unit}
import java.lang.{String, UnsupportedOperationException}

import strawman.collection.mutable.{ArrayBuffer, Builder, StringBuilder}
import java.lang.String

/** Base trait for generic collections */
trait Iterable[+A] extends IterableOnce[A] with IterableLike[A, Iterable] {
  /** The collection itself */
  protected def coll: this.type = this
}

/** Base trait for Iterable operations
  *
  *  VarianceNote
  *  ============
  *
  *  We require that for all child classes of Iterable the variance of
  *  the child class and the variance of the `C` parameter passed to `IterableLike`
  *  are the same. We cannot express this since we lack variance polymorphism. That's
  *  why we have to resort at some places to write `C[A @uncheckedVariance]`.
  *
  */
trait IterableLike[+A, +C[X] <: Iterable[X]]
  extends FromIterable[C]
    with IterableOps[A]
    with IterableMonoTransforms[A, C[A @uncheckedVariance]] // sound bcs of VarianceNote
    with IterablePolyTransforms[A, C] {

  /** Create a collection of type `C[A]` from the elements of `coll`, which has
    *  the same element type as this collection. Overridden in StringOps and ArrayOps.
    */
  protected[this] def fromIterableWithSameElemType(coll: Iterable[A]): C[A]
}

/** Operations over iterables. No operation defined here is generic in the
  *  type of the underlying collection.
  */
trait IterableOps[+A] extends Any {
  protected def coll: Iterable[A]
  private def iterator(): Iterator[A] = coll.iterator()

  /** Apply `f` to each element for its side effects
   *  Note: [U] parameter needed to help scalac's type inference.
   */
  def foreach[U](f: A => U): Unit = iterator().foreach(f)

  def forall(p: A => Boolean): Boolean = iterator().forall(p)

  /** Fold left */
  def foldLeft[B](z: B)(op: (B, A) => B): B = iterator().foldLeft(z)(op)

  /** Fold right */
  def foldRight[B](z: B)(op: (A, B) => B): B = iterator().foldRight(z)(op)

  /** The index of the first element in this collection for which `p` holds. */
  def indexWhere(p: A => Boolean): Int = iterator().indexWhere(p)

  /** Is the collection empty? */
  def isEmpty: Boolean = !iterator().hasNext

  /** Is the collection not empty? */
  def nonEmpty: Boolean = iterator().hasNext

  /** The first element of the collection. */
  def head: A = iterator().next()

  /** Selects the last element.
    * $orderDependent
    * @return The last element of this $coll.
    * @throws NoSuchElementException If the $coll is empty.
    */
  def last: A = {
    val it = iterator()
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
  def size: Int = if (knownSize >= 0) knownSize else iterator().length

  /** A view representing the elements of this collection. */
  def view: View[A] = View.fromIterator(iterator())

  /** Given a collection factory `fi`, convert this collection to the appropriate
    * representation for the current element type `A`. Example uses:
    *
    *      xs.to(List)
    *      xs.to(ArrayBuffer)
    *      xs.to(BitSet) // for xs: Iterable[Int]
    */
  def to(bf: BuildFrom[Iterable[A], A]): bf.To = bf.fromIterable(coll)(coll)
  // Note that `bf` is not implicit. We never want it to be inferred (because a collection could only rebuild itself
  // that way) but we do rely on the implicit conversions from the various factory types to BuildFrom.

  /** Optimized version of `to(BuildFrom)` */
  def to(fi: BoundedIterableFactory[A]): fi.To[A @uncheckedVariance] = fi.fromIterable(coll)

  /** Convert collection to array. */
  def toArray[B >: A: ClassTag]: Array[B] =
    if (knownSize >= 0) copyToArray(new Array[B](knownSize), 0)
    else ArrayBuffer.fromIterable(coll).toArray[B]

  /** Copy all elements of this collection to array `xs`, starting at `start`. */
  def copyToArray[B >: A](xs: Array[B], start: Int = 0): xs.type = {
    var i = start
    val it = iterator()
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
  def mkString(sep: String): String = {
    var first: Boolean = true
    val b = new StringBuilder()
    foreach { elem =>
      if (!first) b ++= sep
      first = false
      b ++= String.valueOf(elem)
    }
    b.result
  }

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

}


/** Type-preserving transforms over iterables.
  *  Operations defined here return in their result iterables of the same type
  *  as the one they are invoked on.
  */
trait IterableMonoTransforms[+A, +Repr] extends Any {
  protected def coll: Iterable[A]
  protected[this] def fromIterableWithSameElemType(coll: Iterable[A]): Repr

  /** All elements satisfying predicate `p` */
  def filter(p: A => Boolean): Repr = fromIterableWithSameElemType(View.Filter(coll, p))

  /** A pair of, first, all elements that satisfy prediacte `p` and, second,
    *  all elements that do not. Interesting because it splits a collection in two.
    *
    *  The default implementation provided here needs to traverse the collection twice.
    *  Strict collections have an overridden version of `partition` in `Buildable`,
    *  which requires only a single traversal.
    */
  def partition(p: A => Boolean): (Repr, Repr) = {
    val pn = View.Partition(coll, p)
    (fromIterableWithSameElemType(pn.left), fromIterableWithSameElemType(pn.right))
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
  def splitAt(n: Int): (Repr, Repr) = (take(n), drop(n))

  /** A collection containing the first `n` elements of this collection. */
  def take(n: Int): Repr = fromIterableWithSameElemType(View.Take(coll, n))

  /** The rest of the collection without its `n` first elements. For
    *  linear, immutable collections this should avoid making a copy.
    */
  def drop(n: Int): Repr = fromIterableWithSameElemType(View.Drop(coll, n))

  /** The rest of the collection without its first element. */
  def tail: Repr = {
    if (coll.isEmpty) throw new UnsupportedOperationException
    drop(1)
  }
}

/** Transforms over iterables that can return collections of different element types.
  */
trait IterablePolyTransforms[+A, +C[_]] extends Any {
  protected def coll: Iterable[A]
  def fromIterable[B](coll: Iterable[B]): C[B]

  /** Map */
  def map[B](f: A => B): C[B] = fromIterable(View.Map(coll, f))

  /** Flatmap */
  def flatMap[B](f: A => IterableOnce[B]): C[B] = fromIterable(View.FlatMap(coll, f))

  /** Concatenation */
  def ++[B >: A](xs: IterableOnce[B]): C[B] = fromIterable(View.Concat(coll, xs))

  /** Zip. Interesting because it requires to align to source collections. */
  def zip[B](xs: IterableOnce[B]): C[(A @uncheckedVariance, B)] = fromIterable(View.Zip(coll, xs))
  // sound bcs of VarianceNote
}

/** Transforms over iterables that can return collections of different element types for which an implicit
  * evidence is required. Every constrained collection type `CC` extends an unconstrained collection type `C`
  * (e.g. `SortedSet[X] extends Set[X]`) so it inherits methods from `IterablePolyTransforms` that do not require
  * the implicit evidence. These methods can only build a default representation of `C` (e.g. a `HashSet` in the
  * case of `Set`). The methods in this trait are the same as the ones in `IterablePolyTransforms` but they do
  * require the implicit evidence, so they can build a new instance of `CC`.
  */
trait ConstrainedIterablePolyTransforms[+A, +C[_], +CC[X] <: C[X]] extends Any with IterablePolyTransforms[A, C] {
  type Ev[_]

  protected def coll: Iterable[A]
  protected def constrainedFromIterable[B: Ev](it: Iterable[B]): CC[B]

  /** Map */
  def map[B : Ev](f: A => B): CC[B] = constrainedFromIterable(View.Map(coll, f))

  /** Flatmap */
  def flatMap[B : Ev](f: A => IterableOnce[B]): CC[B] = constrainedFromIterable(View.FlatMap(coll, f))

  /** Concatenation */
  def ++[B >: A : Ev](xs: IterableOnce[B]): CC[B] = constrainedFromIterable(View.Concat(coll, xs))

  /** Zip. Interesting because it requires to align to source collections. */
  def zip[B](xs: IterableOnce[B])(implicit ev: Ev[(A @uncheckedVariance, B)]): CC[(A @uncheckedVariance, B)] = constrainedFromIterable(View.Zip(coll, xs))
  // sound bcs of VarianceNote

  def collect[B: Ev](pf: scala.PartialFunction[A, B]): CC[B] = flatMap(a => 
    if (pf.isDefinedAt(a)) View.Elems(pf(a))
    else View.Empty
  )

  /** Widen this collection to the most specific unconstrained collection type. This is required in order to
    * call methods from `IterablePolyTransforms` to build a new unconstrained collection when no implicit evidence
    * is available. */
  def unconstrained: C[A @uncheckedVariance]
}

/** Base trait for strict collections that can be built using a builder.
  * @tparam  A    the element type of the collection
  * @tparam Repr  the type of the underlying collection
  */
trait MonoBuildable[+A, +Repr] extends Any with IterableMonoTransforms[A, Repr]  {

  /** Creates a new builder. */
  protected[this] def newBuilderWithSameElemType: Builder[A, Repr]

  /** Optimized, push-based version of `partition`. */
  override def partition(p: A => Boolean): (Repr, Repr) = {
    val l, r = newBuilderWithSameElemType
    coll.iterator().foreach(x => (if (p(x)) l else r) += x)
    (l.result, r.result)
  }

  // one might also override other transforms here to avoid generating
  // iterators if it helps efficiency.
}

/** Base trait for strict collections that can be built for arbitrary element types using a builder.
  * @tparam  A    the element type of the collection
  * @tparam C     the type constructor of the underlying collection
  */
trait PolyBuildable[+A, +C[_]] extends Any with FromIterable[C] {

  /** Creates a new builder. */
  def newBuilder[E]: Builder[E, C[E]]
}

/** Base trait for strict collections that can be built using a builder for element types with an implicit evidence.
  * @tparam  A    the element type of the collection
  * @tparam CC    the type constructor of the underlying collection
  */
trait ConstrainedPolyBuildable[+A, +CC[_], Ev[_]] extends Any with ConstrainedFromIterable[CC, Ev] {

  /** Creates a new builder. */
  def newConstrainedBuilder[E : Ev]: Builder[E, CC[E]]
}
