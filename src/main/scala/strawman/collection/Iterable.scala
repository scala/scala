package strawman
package collection

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.ClassTag
import scala.{Int, Boolean, Array, Any, Unit, StringContext}
import java.lang.{String, UnsupportedOperationException}
import strawman.collection.mutable.{ArrayBuffer, StringBuilder}

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
  protected[this] def fromIterableWithSameElemType(coll: Iterable[A]): C[A] = fromIterable(coll)
}

/** Base trait for instances that can construct a collection from an iterable */
trait FromIterable[+C[X] <: Iterable[X]] {
  def fromIterable[B](it: Iterable[B]): C[B]
}

/** Base trait for companion objects of collections */
trait IterableFactory[+C[X] <: Iterable[X]] extends FromIterable[C] {
  def empty[X]: C[X] = fromIterable(View.Empty)
  def apply[A](xs: A*): C[A] = fromIterable(View.Elems(xs: _*))
  def fill[A](n: Int)(elem: => A): C[A] = fromIterable(View.Fill(n)(elem))
}

/** Operations over iterables. No operation defined here is generic in the
  *  type of the underlying collection.
  */
trait IterableOps[+A] extends Any {
  protected def coll: Iterable[A]
  private def iterator() = coll.iterator()

  /** Apply `f` to each element for its side effects
   *  Note: [U] parameter needed to help scalac's type inference.
   */
  def foreach[U](f: A => U): Unit = iterator().foreach(f)

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

  /** Given a collection factory `fi` for collections of type constructor `C`,
    *  convert this collection to one of type `C[A]`. Example uses:
    *
    *      xs.to(List)
    *      xs.to(ArrayBuffer)
    */
  def to[C[X] <: Iterable[X]](fi: FromIterable[C]): C[A @uncheckedVariance] =
  // variance seems sound because `to` could just as well have been added
  // as a decorator. We should investigate this further to be sure.
    fi.fromIterable(coll)

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
trait IterablePolyTransforms[+A, +C[A]] extends Any {
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
