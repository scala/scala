/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala
package collection
package generic

import scala.language.higherKinds

/** A template for companion objects of `Traversable` and subclasses thereof.
 *  This class provides a set of operations to create `$Coll` objects.
 *  It is typically inherited by companion objects of subclasses of `Traversable`.
 *
 *  @since 2.8
 *
 *  @define coll collection
 *  @define Coll `Traversable`
 *  @define factoryInfo
 *    This object provides a set of operations to create `$Coll` values.
 *    @author Martin Odersky
 *    @version 2.8
 *  @define canBuildFromInfo
 *    The standard `CanBuildFrom` instance for $Coll objects.
 *    @see CanBuildFrom
 *  @define genericCanBuildFromInfo
 *    The standard `CanBuildFrom` instance for $Coll objects.
 *    The created value is an instance of class `GenericCanBuildFrom`,
 *    which forwards calls to create a new builder to the
 *    `genericBuilder` method of the requesting collection.
 *    @see CanBuildFrom
 *    @see GenericCanBuildFrom
 */
abstract class GenTraversableFactory[CC[X] <: GenTraversable[X] with GenericTraversableTemplate[X, CC]]
extends GenericCompanion[CC] {

  private[this] val ReusableCBFInstance: GenericCanBuildFrom[Nothing] = new GenericCanBuildFrom[Nothing] {
    override def apply() = newBuilder[Nothing]
  }
  def ReusableCBF: GenericCanBuildFrom[Nothing] = ReusableCBFInstance

  /** A generic implementation of the `CanBuildFrom` trait, which forwards
   *  all calls to `apply(from)` to the `genericBuilder` method of
   *  $coll `from`, and which forwards all calls of `apply()` to the
   *  `newBuilder` method of this factory.
   */
  class GenericCanBuildFrom[A] extends CanBuildFrom[CC[_], A, CC[A]] {
    /** Creates a new builder on request of a collection.
     *  @param from  the collection requesting the builder to be created.
     *  @return the result of invoking the `genericBuilder` method on `from`.
     */
    def apply(from: Coll) = from.genericBuilder[A]

    /** Creates a new builder from scratch
     *  @return the result of invoking the `newBuilder` method of this factory.
     */
    def apply() = newBuilder[A]
  }

  /** Concatenates all argument collections into a single $coll.
   *
   *  @param xss the collections that are to be concatenated.
   *  @return the concatenation of all the collections.
   */
  def concat[A](xss: Traversable[A]*): CC[A] = {
    val b = newBuilder[A]
    // At present we're using IndexedSeq as a proxy for "has a cheap size method".
    if (xss forall (_.isInstanceOf[IndexedSeq[_]]))
      b.sizeHint(xss.map(_.size).sum)

    for (xs <- xss.seq) b ++= xs
    b.result()
  }

  /** Produces a $coll containing the results of some element computation a number of times.
   *  @param   n  the number of elements contained in the $coll.
   *  @param   elem the element computation
   *  @return  A $coll that contains the results of `n` evaluations of `elem`.
   */
  def fill[A](n: Int)(elem: => A): CC[A] = {
    val b = newBuilder[A]
    b.sizeHint(n)
    var i = 0
    while (i < n) {
      b += elem
      i += 1
    }
    b.result()
  }

  /** Produces a two-dimensional $coll containing the results of some element computation a number of times.
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   elem the element computation
   *  @return  A $coll that contains the results of `n1 x n2` evaluations of `elem`.
   */
  def fill[A](n1: Int, n2: Int)(elem: => A): CC[CC[A]] =
    tabulate(n1)(_ => fill(n2)(elem))

  /** Produces a three-dimensional $coll containing the results of some element computation a number of times.
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   elem the element computation
   *  @return  A $coll that contains the results of `n1 x n2 x n3` evaluations of `elem`.
   */
  def fill[A](n1: Int, n2: Int, n3: Int)(elem: => A): CC[CC[CC[A]]] =
    tabulate(n1)(_ => fill(n2, n3)(elem))

  /** Produces a four-dimensional $coll containing the results of some element computation a number of times.
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   elem the element computation
   *  @return  A $coll that contains the results of `n1 x n2 x n3 x n4` evaluations of `elem`.
   */
  def fill[A](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => A): CC[CC[CC[CC[A]]]] =
    tabulate(n1)(_ => fill(n2, n3, n4)(elem))

  /** Produces a five-dimensional $coll containing the results of some element computation a number of times.
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   n5  the number of elements in the 5th dimension
   *  @param   elem the element computation
   *  @return  A $coll that contains the results of `n1 x n2 x n3 x n4 x n5` evaluations of `elem`.
   */
  def fill[A](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(elem: => A): CC[CC[CC[CC[CC[A]]]]] =
    tabulate(n1)(_ => fill(n2, n3, n4, n5)(elem))

  /** Produces a $coll containing values of a given function over a range of integer values starting from 0.
   *  @param  n   The number of elements in the $coll
   *  @param  f   The function computing element values
   *  @return A $coll consisting of elements `f(0), ..., f(n -1)`
   */
  def tabulate[A](n: Int)(f: Int => A): CC[A] = {
    val b = newBuilder[A]
    b.sizeHint(n)
    var i = 0
    while (i < n) {
      b += f(i)
      i += 1
    }
    b.result()
  }

  /** Produces a two-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   f   The function computing element values
   *  @return A $coll consisting of elements `f(i1, i2)`
   *          for `0 <= i1 < n1` and `0 <= i2 < n2`.
   */
  def tabulate[A](n1: Int, n2: Int)(f: (Int, Int) => A): CC[CC[A]] =
    tabulate(n1)(i1 => tabulate(n2)(f(i1, _)))

  /** Produces a three-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   f   The function computing element values
   *  @return A $coll consisting of elements `f(i1, i2, i3)`
   *          for `0 <= i1 < n1`, `0 <= i2 < n2`, and `0 <= i3 < n3`.
   */
  def tabulate[A](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => A): CC[CC[CC[A]]] =
    tabulate(n1)(i1 => tabulate(n2, n3)(f(i1, _, _)))

  /** Produces a four-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   f   The function computing element values
   *  @return A $coll consisting of elements `f(i1, i2, i3, i4)`
   *          for `0 <= i1 < n1`, `0 <= i2 < n2`, `0 <= i3 < n3`, and `0 <= i4 < n4`.
   */
  def tabulate[A](n1: Int, n2: Int, n3: Int, n4: Int)(f: (Int, Int, Int, Int) => A): CC[CC[CC[CC[A]]]] =
    tabulate(n1)(i1 => tabulate(n2, n3, n4)(f(i1, _, _, _)))

  /** Produces a five-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   n5  the number of elements in the 5th dimension
   *  @param   f   The function computing element values
   *  @return A $coll consisting of elements `f(i1, i2, i3, i4, i5)`
   *          for `0 <= i1 < n1`, `0 <= i2 < n2`, `0 <= i3 < n3`, `0 <= i4 < n4`, and `0 <= i5 < n5`.
   */
  def tabulate[A](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(f: (Int, Int, Int, Int, Int) => A): CC[CC[CC[CC[CC[A]]]]] =
    tabulate(n1)(i1 => tabulate(n2, n3, n4, n5)(f(i1, _, _, _, _)))

  /** Produces a $coll containing a sequence of increasing of integers.
   *
   *  @param start the first element of the $coll
   *  @param end   the end value of the $coll (the first value NOT contained)
   *  @return  a $coll with values `start, start + 1, ..., end - 1`
   */
  def range[T: Integral](start: T, end: T): CC[T] = range(start, end, implicitly[Integral[T]].one)

  /** Produces a $coll containing equally spaced values in some integer interval.
   *  @param start the start value of the $coll
   *  @param end   the end value of the $coll (the first value NOT contained)
   *  @param step  the difference between successive elements of the $coll (must be positive or negative)
   *  @return      a $coll with values `start, start + step, ...` up to, but excluding `end`
   */
  def range[T: Integral](start: T, end: T, step: T): CC[T] = {
    val num = implicitly[Integral[T]]
    import num._

    if (step == zero) throw new IllegalArgumentException("zero step")
    val b = newBuilder[T]
    b sizeHint immutable.NumericRange.count(start, end, step, isInclusive = false)
    var i = start
    while (if (step < zero) end < i else i < end) {
      b += i
      i += step
    }
    b.result()
  }

  /** Produces a $coll containing repeated applications of a function to a start value.
   *
   *  @param start the start value of the $coll
   *  @param len   the number of elements contained inthe $coll
   *  @param f     the function that's repeatedly applied
   *  @return      a $coll with `len` values in the sequence `start, f(start), f(f(start)), ...`
   */
  def iterate[A](start: A, len: Int)(f: A => A): CC[A] = {
    val b = newBuilder[A]
    if (len > 0) {
      b.sizeHint(len)
      var acc = start
      var i = 1
      b += acc

      while (i < len) {
        acc = f(acc)
        i += 1
        b += acc
      }
    }
    b.result()
  }
}
