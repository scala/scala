/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import scala.collection.generic._
import scala.collection.{ mutable, immutable }
import mutable.{ ArrayBuilder, ArraySeq }
import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime.{ array_apply, array_update }

/** Contains a fallback builder for arrays when the element type
 *  does not have a class tag. In that case a generic array is built.
 */
class FallbackArrayBuilding {

  /** A builder factory that generates a generic array.
   *  Called instead of `Array.newBuilder` if the element type of an array
   *  does not have a class tag. Note that fallbackBuilder factory
   *  needs an implicit parameter (otherwise it would not be dominated in
   *  implicit search by `Array.canBuildFrom`). We make sure that
   *  implicit search is always successful.
   */
  implicit def fallbackCanBuildFrom[T](implicit m: DummyImplicit): CanBuildFrom[Array[_], T, ArraySeq[T]] =
    new CanBuildFrom[Array[_], T, ArraySeq[T]] {
      def apply(from: Array[_]) = ArraySeq.newBuilder[T]
      def apply() = ArraySeq.newBuilder[T]
    }
}

/** Utility methods for operating on arrays.
 *  For example:
 *  {{{
 *  val a = Array(1, 2)
 *  val b = Array.ofDim[Int](2)
 *  val c = Array.concat(a, b)
 *  }}}
 *  where the array objects `a`, `b` and `c` have respectively the values
 *  `Array(1, 2)`, `Array(0, 0)` and `Array(1, 2, 0, 0)`.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
object Array extends FallbackArrayBuilding {
  val emptyBooleanArray = new Array[Boolean](0)
  val emptyByteArray    = new Array[Byte](0)
  val emptyCharArray    = new Array[Char](0)
  val emptyDoubleArray  = new Array[Double](0)
  val emptyFloatArray   = new Array[Float](0)
  val emptyIntArray     = new Array[Int](0)
  val emptyLongArray    = new Array[Long](0)
  val emptyShortArray   = new Array[Short](0)
  val emptyObjectArray  = new Array[Object](0)

  implicit def canBuildFrom[T](implicit t: ClassTag[T]): CanBuildFrom[Array[_], T, Array[T]] =
    new CanBuildFrom[Array[_], T, Array[T]] {
      def apply(from: Array[_]) = ArrayBuilder.make[T]()(t)
      def apply() = ArrayBuilder.make[T]()(t)
    }

  /**
   * Returns a new [[scala.collection.mutable.ArrayBuilder]].
   */
  def newBuilder[T](implicit t: ClassTag[T]): ArrayBuilder[T] = ArrayBuilder.make[T]()(t)

  private def slowcopy(src : AnyRef,
                       srcPos : Int,
                       dest : AnyRef,
                       destPos : Int,
                       length : Int) {
    var i = srcPos
    var j = destPos
    val srcUntil = srcPos + length
    while (i < srcUntil) {
      array_update(dest, j, array_apply(src, i))
      i += 1
      j += 1
    }
  }

  /** Copy one array to another.
   *  Equivalent to Java's
   *    `System.arraycopy(src, srcPos, dest, destPos, length)`,
   *  except that this also works for polymorphic and boxed arrays.
   *
   *  Note that the passed-in `dest` array will be modified by this call.
   *
   *  @param src the source array.
   *  @param srcPos  starting position in the source array.
   *  @param dest destination array.
   *  @param destPos starting position in the destination array.
   *  @param length the number of array elements to be copied.
   *
   *  @see `java.lang.System#arraycopy`
   */
  def copy(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int) {
    val srcClass = src.getClass
    if (srcClass.isArray && dest.getClass.isAssignableFrom(srcClass))
      java.lang.System.arraycopy(src, srcPos, dest, destPos, length)
    else
      slowcopy(src, srcPos, dest, destPos, length)
  }

  /** Returns an array of length 0 */
  def empty[T: ClassTag]: Array[T] = new Array[T](0)

  /** Creates an array with given elements.
   *
   *  @param xs the elements to put in the array
   *  @return an array containing all elements from xs.
   */
  // Subject to a compiler optimization in Cleanup.
  // Array(e0, ..., en) is translated to { val a = new Array(3); a(i) = ei; a }
  def apply[T: ClassTag](xs: T*): Array[T] = {
    val array = new Array[T](xs.length)
    var i = 0
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  /** Creates an array of `Boolean` objects */
  // Subject to a compiler optimization in Cleanup, see above.
  def apply(x: Boolean, xs: Boolean*): Array[Boolean] = {
    val array = new Array[Boolean](xs.length + 1)
    array(0) = x
    var i = 1
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  /** Creates an array of `Byte` objects */
  // Subject to a compiler optimization in Cleanup, see above.
  def apply(x: Byte, xs: Byte*): Array[Byte] = {
    val array = new Array[Byte](xs.length + 1)
    array(0) = x
    var i = 1
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  /** Creates an array of `Short` objects */
  // Subject to a compiler optimization in Cleanup, see above.
  def apply(x: Short, xs: Short*): Array[Short] = {
    val array = new Array[Short](xs.length + 1)
    array(0) = x
    var i = 1
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  /** Creates an array of `Char` objects */
  // Subject to a compiler optimization in Cleanup, see above.
  def apply(x: Char, xs: Char*): Array[Char] = {
    val array = new Array[Char](xs.length + 1)
    array(0) = x
    var i = 1
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  /** Creates an array of `Int` objects */
  // Subject to a compiler optimization in Cleanup, see above.
  def apply(x: Int, xs: Int*): Array[Int] = {
    val array = new Array[Int](xs.length + 1)
    array(0) = x
    var i = 1
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  /** Creates an array of `Long` objects */
  // Subject to a compiler optimization in Cleanup, see above.
  def apply(x: Long, xs: Long*): Array[Long] = {
    val array = new Array[Long](xs.length + 1)
    array(0) = x
    var i = 1
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  /** Creates an array of `Float` objects */
  // Subject to a compiler optimization in Cleanup, see above.
  def apply(x: Float, xs: Float*): Array[Float] = {
    val array = new Array[Float](xs.length + 1)
    array(0) = x
    var i = 1
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  /** Creates an array of `Double` objects */
  // Subject to a compiler optimization in Cleanup, see above.
  def apply(x: Double, xs: Double*): Array[Double] = {
    val array = new Array[Double](xs.length + 1)
    array(0) = x
    var i = 1
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  /** Creates an array of `Unit` objects */
  def apply(x: Unit, xs: Unit*): Array[Unit] = {
    val array = new Array[Unit](xs.length + 1)
    array(0) = x
    var i = 1
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  /** Creates array with given dimensions */
  def ofDim[T: ClassTag](n1: Int): Array[T] =
    new Array[T](n1)
  /** Creates a 2-dimensional array */
  def ofDim[T: ClassTag](n1: Int, n2: Int): Array[Array[T]] = {
    val arr: Array[Array[T]] = (new Array[Array[T]](n1): Array[Array[T]])
    for (i <- 0 until n1) arr(i) = new Array[T](n2)
    arr
    // tabulate(n1)(_ => ofDim[T](n2))
  }
  /** Creates a 3-dimensional array */
  def ofDim[T: ClassTag](n1: Int, n2: Int, n3: Int): Array[Array[Array[T]]] =
    tabulate(n1)(_ => ofDim[T](n2, n3))
  /** Creates a 4-dimensional array */
  def ofDim[T: ClassTag](n1: Int, n2: Int, n3: Int, n4: Int): Array[Array[Array[Array[T]]]] =
    tabulate(n1)(_ => ofDim[T](n2, n3, n4))
  /** Creates a 5-dimensional array */
  def ofDim[T: ClassTag](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int): Array[Array[Array[Array[Array[T]]]]] =
    tabulate(n1)(_ => ofDim[T](n2, n3, n4, n5))

  /** Concatenates all arrays into a single array.
   *
   *  @param xss the given arrays
   *  @return   the array created from concatenating `xss`
   */
  def concat[T: ClassTag](xss: Array[T]*): Array[T] = {
    val b = newBuilder[T]
    b.sizeHint(xss.map(_.length).sum)
    for (xs <- xss) b ++= xs
    b.result()
  }

  /** Returns an array that contains the results of some element computation a number
   *  of times.
   *
   *  Note that this means that `elem` is computed a total of n times:
   *  {{{
   * scala> Array.fill(3){ math.random }
   * res3: Array[Double] = Array(0.365461167592537, 1.550395944913685E-4, 0.7907242137333306)
   *  }}}
   *
   *  @param   n  the number of elements desired
   *  @param   elem the element computation
   *  @return an Array of size n, where each element contains the result of computing
   *  `elem`.
   */
  def fill[T: ClassTag](n: Int)(elem: => T): Array[T] = {
    val b = newBuilder[T]
    b.sizeHint(n)
    var i = 0
    while (i < n) {
      b += elem
      i += 1
    }
    b.result()
  }

  /** Returns a two-dimensional array that contains the results of some element
   *  computation a number of times.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   elem the element computation
   */
  def fill[T: ClassTag](n1: Int, n2: Int)(elem: => T): Array[Array[T]] =
    tabulate(n1)(_ => fill(n2)(elem))

  /** Returns a three-dimensional array that contains the results of some element
   *  computation a number of times.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   elem the element computation
   */
  def fill[T: ClassTag](n1: Int, n2: Int, n3: Int)(elem: => T): Array[Array[Array[T]]] =
    tabulate(n1)(_ => fill(n2, n3)(elem))

  /** Returns a four-dimensional array that contains the results of some element
   *  computation a number of times.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   elem the element computation
   */
  def fill[T: ClassTag](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => T): Array[Array[Array[Array[T]]]] =
    tabulate(n1)(_ => fill(n2, n3, n4)(elem))

  /** Returns a five-dimensional array that contains the results of some element
   *  computation a number of times.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   n5  the number of elements in the 5th dimension
   *  @param   elem the element computation
   */
  def fill[T: ClassTag](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(elem: => T): Array[Array[Array[Array[Array[T]]]]] =
    tabulate(n1)(_ => fill(n2, n3, n4, n5)(elem))

  /** Returns an array containing values of a given function over a range of integer
   *  values starting from 0.
   *
   *  @param  n   The number of elements in the array
   *  @param  f   The function computing element values
   *  @return A traversable consisting of elements `f(0),f(1), ..., f(n - 1)`
   */
  def tabulate[T: ClassTag](n: Int)(f: Int => T): Array[T] = {
    val b = newBuilder[T]
    b.sizeHint(n)
    var i = 0
    while (i < n) {
      b += f(i)
      i += 1
    }
    b.result()
  }

  /** Returns a two-dimensional array containing values of a given function
   *  over ranges of integer values starting from `0`.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   f   The function computing element values
   */
  def tabulate[T: ClassTag](n1: Int, n2: Int)(f: (Int, Int) => T): Array[Array[T]] =
    tabulate(n1)(i1 => tabulate(n2)(f(i1, _)))

  /** Returns a three-dimensional array containing values of a given function
   *  over ranges of integer values starting from `0`.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3rd dimension
   *  @param   f   The function computing element values
   */
  def tabulate[T: ClassTag](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => T): Array[Array[Array[T]]] =
    tabulate(n1)(i1 => tabulate(n2, n3)(f(i1, _, _)))

  /** Returns a four-dimensional array containing values of a given function
   *  over ranges of integer values starting from `0`.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3rd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   f   The function computing element values
   */
  def tabulate[T: ClassTag](n1: Int, n2: Int, n3: Int, n4: Int)(f: (Int, Int, Int, Int) => T): Array[Array[Array[Array[T]]]] =
    tabulate(n1)(i1 => tabulate(n2, n3, n4)(f(i1, _, _, _)))

  /** Returns a five-dimensional array containing values of a given function
   *  over ranges of integer values starting from `0`.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3rd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   n5  the number of elements in the 5th dimension
   *  @param   f   The function computing element values
   */
  def tabulate[T: ClassTag](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(f: (Int, Int, Int, Int, Int) => T): Array[Array[Array[Array[Array[T]]]]] =
    tabulate(n1)(i1 => tabulate(n2, n3, n4, n5)(f(i1, _, _, _, _)))

  /** Returns an array containing a sequence of increasing integers in a range.
   *
   *  @param start  the start value of the array
   *  @param end    the end value of the array, exclusive (in other words, this is the first value '''not''' returned)
   *  @return  the array with values in range `start, start + 1, ..., end - 1`
   *  up to, but excluding, `end`.
   */
  def range(start: Int, end: Int): Array[Int] = range(start, end, 1)

  /** Returns an array containing equally spaced values in some integer interval.
   *
   *  @param start the start value of the array
   *  @param end   the end value of the array, exclusive (in other words, this is the first value '''not''' returned)
   *  @param step  the increment value of the array (may not be zero)
   *  @return      the array with values in `start, start + step, ...` up to, but excluding `end`
   */
  def range(start: Int, end: Int, step: Int): Array[Int] = {
    if (step == 0) throw new IllegalArgumentException("zero step")
    val b = newBuilder[Int]
    b.sizeHint(immutable.Range.count(start, end, step, isInclusive = false))

    var i = start
    while (if (step < 0) end < i else i < end) {
      b += i
      i += step
    }
    b.result()
  }

  /** Returns an array containing repeated applications of a function to a start value.
   *
   *  @param start the start value of the array
   *  @param len   the number of elements returned by the array
   *  @param f     the function that is repeatedly applied
   *  @return      the array returning `len` values in the sequence `start, f(start), f(f(start)), ...`
   */
  def iterate[T: ClassTag](start: T, len: Int)(f: T => T): Array[T] = {
    val b = newBuilder[T]

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

  /** Called in a pattern match like `{ case Array(x,y,z) => println('3 elements')}`.
   *
   *  @param x the selector value
   *  @return  sequence wrapped in a [[scala.Some]], if `x` is a Seq, otherwise `None`
   */
  def unapplySeq[T](x: Array[T]): Option[IndexedSeq[T]] =
    if (x == null) None else Some(x.toIndexedSeq)
    // !!! the null check should to be necessary, but without it 2241 fails. Seems to be a bug
    // in pattern matcher.  @PP: I noted in #4364 I think the behavior is correct.
}

/** Arrays are mutable, indexed collections of values. `Array[T]` is Scala's representation
 *  for Java's `T[]`.
 *
 *  {{{
 *  val numbers = Array(1, 2, 3, 4)
 *  val first = numbers(0) // read the first element
 *  numbers(3) = 100 // replace the 4th array element with 100
 *  val biggerNumbers = numbers.map(_ * 2) // multiply all numbers by two
 *  }}}
 *
 *  Arrays make use of two common pieces of Scala syntactic sugar, shown on lines 2 and 3 of the above
 *  example code.
 *  Line 2 is translated into a call to `apply(Int)`, while line 3 is translated into a call to
 *  `update(Int, T)`.
 *
 *  Two implicit conversions exist in [[scala.Predef]] that are frequently applied to arrays: a conversion
 *  to [[scala.collection.mutable.ArrayOps]] (shown on line 4 of the example above) and a conversion
 *  to [[scala.collection.mutable.WrappedArray]] (a subtype of [[scala.collection.Seq]]).
 *  Both types make available many of the standard operations found in the Scala collections API.
 *  The conversion to `ArrayOps` is temporary, as all operations defined on `ArrayOps` return an `Array`,
 *  while the conversion to `WrappedArray` is permanent as all operations return a `WrappedArray`.
 *
 *  The conversion to `ArrayOps` takes priority over the conversion to `WrappedArray`. For instance,
 *  consider the following code:
 *
 *  {{{
 *  val arr = Array(1, 2, 3)
 *  val arrReversed = arr.reverse
 *  val seqReversed : Seq[Int] = arr.reverse
 *  }}}
 *
 *  Value `arrReversed` will be of type `Array[Int]`, with an implicit conversion to `ArrayOps` occurring
 *  to perform the `reverse` operation. The value of `seqReversed`, on the other hand, will be computed
 *  by converting to `WrappedArray` first and invoking the variant of `reverse` that returns another
 *  `WrappedArray`.
 *
 *  @author Martin Odersky
 *  @version 1.0
 *  @see [[http://www.scala-lang.org/files/archive/spec/2.11/ Scala Language Specification]], for in-depth information on the transformations the Scala compiler makes on Arrays (Sections 6.6 and 6.15 respectively.)
 *  @see [[http://docs.scala-lang.org/sips/completed/scala-2-8-arrays.html "Scala 2.8 Arrays"]] the Scala Improvement Document detailing arrays since Scala 2.8.
 *  @see [[http://docs.scala-lang.org/overviews/collections/arrays.html "The Scala 2.8 Collections' API"]] section on `Array` by Martin Odersky for more information.
 *  @hideImplicitConversion scala.Predef.booleanArrayOps
 *  @hideImplicitConversion scala.Predef.byteArrayOps
 *  @hideImplicitConversion scala.Predef.charArrayOps
 *  @hideImplicitConversion scala.Predef.doubleArrayOps
 *  @hideImplicitConversion scala.Predef.floatArrayOps
 *  @hideImplicitConversion scala.Predef.intArrayOps
 *  @hideImplicitConversion scala.Predef.longArrayOps
 *  @hideImplicitConversion scala.Predef.refArrayOps
 *  @hideImplicitConversion scala.Predef.shortArrayOps
 *  @hideImplicitConversion scala.Predef.unitArrayOps
 *  @hideImplicitConversion scala.LowPriorityImplicits.wrapRefArray
 *  @hideImplicitConversion scala.LowPriorityImplicits.wrapIntArray
 *  @hideImplicitConversion scala.LowPriorityImplicits.wrapDoubleArray
 *  @hideImplicitConversion scala.LowPriorityImplicits.wrapLongArray
 *  @hideImplicitConversion scala.LowPriorityImplicits.wrapFloatArray
 *  @hideImplicitConversion scala.LowPriorityImplicits.wrapCharArray
 *  @hideImplicitConversion scala.LowPriorityImplicits.wrapByteArray
 *  @hideImplicitConversion scala.LowPriorityImplicits.wrapShortArray
 *  @hideImplicitConversion scala.LowPriorityImplicits.wrapBooleanArray
 *  @hideImplicitConversion scala.LowPriorityImplicits.wrapUnitArray
 *  @hideImplicitConversion scala.LowPriorityImplicits.genericWrapArray
 *  @define coll array
 *  @define Coll `Array`
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *  @define collectExample
 *  @define undefinedorder
 *  @define thatinfo the class of the returned collection. In the standard library configuration,
 *    `That` is either `Array[B]` if an ClassTag is available for B or `ArraySeq[B]` otherwise.
 *  @define zipthatinfo $thatinfo
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the result class `That` from the current
 *    representation type `Repr` and the new element type `B`.
 */
final class Array[T](_length: Int) extends java.io.Serializable with java.lang.Cloneable {

  /** The length of the array */
  def length: Int = throw new Error()

  /** The element at given index.
   *
   *  Indices start at `0`; `xs.apply(0)` is the first element of array `xs`.
   *  Note the indexing syntax `xs(i)` is a shorthand for `xs.apply(i)`.
   *
   *  @param    i   the index
   *  @return       the element at the given index
   *  @throws       ArrayIndexOutOfBoundsException if `i < 0` or `length <= i`
   */
  def apply(i: Int): T = throw new Error()

  /** Update the element at given index.
   *
   *  Indices start at `0`; `xs.update(i, x)` replaces the i^th^ element in the array.
   *  Note the syntax `xs(i) = x` is a shorthand for `xs.update(i, x)`.
   *
   *  @param    i   the index
   *  @param    x   the value to be written at index `i`
   *  @throws       ArrayIndexOutOfBoundsException if `i < 0` or `length <= i`
   */
  def update(i: Int, x: T) { throw new Error() }

  /** Clone the Array.
   *
   *  @return A clone of the Array.
   */
  override def clone(): Array[T] = throw new Error()
}
