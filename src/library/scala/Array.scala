/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

import scala.collection.generic._
import scala.collection.mutable.{ArrayBuilder, ArraySeq}
import compat.Platform.arraycopy
import scala.reflect.ClassManifest
import scala.runtime.ScalaRunTime.{array_apply, array_update}

/** A class containing a fall back builder for arrays where the element type
 *  does not have a class manifest. In that case a generic array is built.
 */
class FallbackArrayBuilding {

  /** A builder factory that generates a generic array.
   *  Called instead of Array.newBuilder if the element type of an array
   *  does not have a class manifest. Note that fallbackBuilder factory
   *  needs an implicit parameter (otherwise it would not be dominated in implicit search
   *  by Array.canBuildFrom). We make sure that that implicit search is always
   *  successfull.
   */
  implicit def fallbackCanBuildFrom[T](implicit m: DummyImplicit): CanBuildFrom[Array[_], T, ArraySeq[T]] =
    new CanBuildFrom[Array[_], T, ArraySeq[T]] {
      def apply(from: Array[_]) = ArraySeq.newBuilder[T]
      def apply() = ArraySeq.newBuilder[T]
    }
}

/** This object contains utility methods operating on arrays.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
object Array extends FallbackArrayBuilding {
  implicit def canBuildFrom[T](implicit m: ClassManifest[T]): CanBuildFrom[Array[_], T, Array[T]] =
    new CanBuildFrom[Array[_], T, Array[T]] {
      def apply(from: Array[_]) = ArrayBuilder.make[T]()(m)
      def apply() = ArrayBuilder.make[T]()(m)
    }

  def newBuilder[T](implicit m: ClassManifest[T]): ArrayBuilder[T] = ArrayBuilder.make[T]()(m)

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
   *  Equivalent to
   *    <code>System.arraycopy(src, srcPos, dest, destPos, length)</code>,
   *  except that this works also for polymorphic and boxed arrays.
   *
   *  @param src     ...
   *  @param srcPos  ...
   *  @param dest    ...
   *  @param destPos ...
   *  @param length  ...
   */
  def copy(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int) {
    val srcClass = src.getClass
    if (srcClass.isArray && dest.getClass.isAssignableFrom(srcClass))
      arraycopy(src, srcPos, dest, destPos, length)
    else
      slowcopy(src, srcPos, dest, destPos, length)
  }

  /** Returns array of length 0 */
  def empty[T: ClassManifest]: Array[T] = new Array[T](0)

  /** Create an array with given elements.
   *
   *  @param xs the elements to put in the array
   *  @return the array containing elements xs.
   */
  def apply[T: ClassManifest](xs: T*): Array[T] = {
    val array = new Array[T](xs.length)
    var i = 0
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  def apply(x: Boolean, xs: Boolean*): Array[Boolean] = {
    val array = new Array[Boolean](xs.length + 1)
    array(0) = x
    var i = 1
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  def apply(x: Byte, xs: Byte*): Array[Byte] = {
    val array = new Array[Byte](xs.length + 1)
    array(0) = x
    var i = 1
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  def apply(x: Short, xs: Short*): Array[Short] = {
    val array = new Array[Short](xs.length + 1)
    array(0) = x
    var i = 1
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  def apply(x: Char, xs: Char*): Array[Char] = {
    val array = new Array[Char](xs.length + 1)
    array(0) = x
    var i = 1
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  def apply(x: Int, xs: Int*): Array[Int] = {
    val array = new Array[Int](xs.length + 1)
    array(0) = x
    var i = 1
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  def apply(x: Long, xs: Long*): Array[Long] = {
    val array = new Array[Long](xs.length + 1)
    array(0) = x
    var i = 1
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  def apply(x: Float, xs: Float*): Array[Float] = {
    val array = new Array[Float](xs.length + 1)
    array(0) = x
    var i = 1
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  def apply(x: Double, xs: Double*): Array[Double] = {
    val array = new Array[Double](xs.length + 1)
    array(0) = x
    var i = 1
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  def apply(x: Unit, xs: Unit*): Array[Unit] = {
    val array = new Array[Unit](xs.length + 1)
    array(0) = x
    var i = 1
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  /** Create array with given dimensions */
  def ofDim[T: ClassManifest](n1: Int): Array[T] =
    new Array[T](n1)
  def ofDim[T: ClassManifest](n1: Int, n2: Int): Array[Array[T]] = {
    val arr: Array[Array[T]] = (new Array[Array[T]](n1): Array[Array[T]])
    for (i <- 0 until n1) arr(i) = new Array[T](n2)
    arr
    // tabulate(n1)(_ => ofDim[T](n2))
  }
  def ofDim[T: ClassManifest](n1: Int, n2: Int, n3: Int): Array[Array[Array[T]]] =
    tabulate(n1)(_ => ofDim[T](n2, n3))
  def ofDim[T: ClassManifest](n1: Int, n2: Int, n3: Int, n4: Int): Array[Array[Array[Array[T]]]] =
    tabulate(n1)(_ => ofDim[T](n2, n3, n4))
  def ofDim[T: ClassManifest](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int): Array[Array[Array[Array[Array[T]]]]] =
    tabulate(n1)(_ => ofDim[T](n2, n3, n4, n5))

  /** Concatenate all argument sequences into a single array.
   *
   *  @param xs the given argument sequences
   *  @return   the array created from the concatenated arguments
   */
  def concat[T: ClassManifest](xss: Array[T]*): Array[T] = {
    val b = newBuilder[T]
    b.sizeHint(xss.map(_.size).sum)
    for (xs <- xss) b ++= xs
    b.result
  }

  /** An array that contains the results of some element computation a number
   *  of times.
   *
   *  @param   n  the number of elements returned
   *  @param   elem the element computation
   */
  def fill[T: ClassManifest](n: Int)(elem: => T): Array[T] = {
    val b = newBuilder[T]
    b.sizeHint(n)
    var i = 0
    while (i < n) {
      b += elem
      i += 1
    }
    b.result
  }

  /** A two-dimensional array that contains the results of some element
   *  computation a number of times.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   elem the element computation
   */
  def fill[T: ClassManifest](n1: Int, n2: Int)(elem: => T): Array[Array[T]] =
    tabulate(n1)(_ => fill(n2)(elem))

  /** A three-dimensional array that contains the results of some element
   *  computation a number of times.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   elem the element computation
   */
  def fill[T: ClassManifest](n1: Int, n2: Int, n3: Int)(elem: => T): Array[Array[Array[T]]] =
    tabulate(n1)(_ => fill(n2, n3)(elem))

  /** A four-dimensional array that contains the results of some element
   *  computation a number of times.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   elem the element computation
   */
  def fill[T: ClassManifest](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => T): Array[Array[Array[Array[T]]]] =
    tabulate(n1)(_ => fill(n2, n3, n4)(elem))

  /** A five-dimensional array that contains the results of some element
   *  computation a number of times.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   n5  the number of elements in the 5th dimension
   *  @param   elem the element computation
   */
  def fill[T: ClassManifest](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(elem: => T): Array[Array[Array[Array[Array[T]]]]] =
    tabulate(n1)(_ => fill(n2, n3, n4, n5)(elem))

  /** An array containing values of a given function over a range of integer
   *  values starting from 0.
   *
   *  @param  n   The number of elements in the traversable
   *  @param  f   The function computing element values
   *  @return A traversable consisting of elements `f(0), ..., f(n -1)`
   */
  def tabulate[T: ClassManifest](n: Int)(f: Int => T): Array[T] = {
    val b = newBuilder[T]
    b.sizeHint(n)
    var i = 0
    while (i < n) {
      b += f(i)
      i += 1
    }
    b.result
  }

  /** A two-dimensional array containing values of a given function over
   *  ranges of integer values starting from 0.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   f   The function computing element values
   */
  def tabulate[T: ClassManifest](n1: Int, n2: Int)(f: (Int, Int) => T): Array[Array[T]] =
    tabulate(n1)(i1 => tabulate(n2)(f(i1, _)))

  /** A three-dimensional array containing values of a given function over
   *  ranges of integer values starting from 0.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   f   The function computing element values
   */
  def tabulate[T: ClassManifest](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => T): Array[Array[Array[T]]] =
    tabulate(n1)(i1 => tabulate(n2, n3)(f(i1, _, _)))

  /** A four-dimensional array containing values of a given function over
   *  ranges of integer values starting from 0.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   f   The function computing element values
   */
  def tabulate[T: ClassManifest](n1: Int, n2: Int, n3: Int, n4: Int)(f: (Int, Int, Int, Int) => T): Array[Array[Array[Array[T]]]] =
    tabulate(n1)(i1 => tabulate(n2, n3, n4)(f(i1, _, _, _)))

  /** A five-dimensional array containing values of a given function over
   *  ranges of integer values starting from 0.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   n5  the number of elements in the 5th dimension
   *  @param   f   The function computing element values
   */
  def tabulate[T: ClassManifest](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(f: (Int, Int, Int, Int, Int) => T): Array[Array[Array[Array[Array[T]]]]] =
    tabulate(n1)(i1 => tabulate(n2, n3, n4, n5)(f(i1, _, _, _, _)))

  /** An array containing a sequence of increasing integers in a range.
   *
   *  @param from the start value of the array
   *  @param end the end value of the array (the first value NOT returned)
   *  @return  the array with values in range `start, start + 1, ..., end - 1`
   *  up to, but exclusding, `end`.
   */
  def range(start: Int, end: Int): Array[Int] = range(start, end, 1)

  /** An array containing equally spaced values in some integer interval.
   *
   *  @param start the start value of the array
   *  @param end   the end value of the array (the first value NOT returned)
   *  @param step  the increment value of the array (must be positive or negative)
   *  @return      the array with values in `start, start + step, ...` up to, but excluding `end`
   */
  def range(start: Int, end: Int, step: Int): Array[Int] = {
    if (step == 0) throw new IllegalArgumentException("zero step")
    val b = newBuilder[Int]
    b.sizeHint(Range.count(start, end, step, false))

    var i = start
    while (if (step < 0) end < i else i < end) {
      b += i
      i += step
    }
    b.result
  }

  /** An array containing repeated applications of a function to a start value.
   *
   *  @param start the start value of the array
   *  @param len   the number of elements returned by the array
   *  @param f     the function that's repeatedly applied
   *  @return      the array returning `len` values in the sequence `start, f(start), f(f(start)), ...`
   */
  def iterate[T: ClassManifest](start: T, len: Int)(f: T => T): Array[T] = {
    val b = newBuilder[T]
    b.sizeHint(len)
    var acc = start
    var i = 0
    while (i < len) {
      b += acc
      acc = f(acc)
      i += 1
    }
    b.result
  }

  /** This method is called in a pattern match { case Seq(...) => }.
   *
   *  @param x the selector value
   *  @return  sequence wrapped in an option, if this is a Seq, otherwise none
   */
  def unapplySeq[T](x: Array[T]): Option[IndexedSeq[T]] =
    if (x == null) None else Some(x.toIndexedSeq)
    // !!! the null check should to be necessary, but without it 2241 fails. Seems to be a bug
    // in pattern matcher.

  /** Create an array containing several copies of an element.
   *
   *  @param n    the length of the resulting array
   *  @param elem the element composing the resulting array
   *  @return     an array composed of n elements all equal to elem
   */
  @deprecated("use `Array.fill' instead")
  def make[T: ClassManifest](n: Int, elem: T): Array[T] = {
    val a = new Array[T](n)
    var i = 0
    while (i < n) {
      a(i) = elem
      i += 1
    }
    a
  }

  /** Create an array containing the values of a given function <code>f</code>
   *  over given range <code>[0..n)</code>
   */
  @deprecated("use `Array.tabulate' instead")
  def fromFunction[T: ClassManifest](f: Int => T)(n: Int): Array[T] = {
    val a = new Array[T](n)
    var i = 0
    while (i < n) {
      a(i) = f(i)
      i += 1
    }
    a
  }

  /** Create an array containing the values of a given function <code>f</code>
   *  over given range <code>[0..n1, 0..n2)</code>
   */
  @deprecated("use `Array.tabulate' instead")
  def fromFunction[T: ClassManifest](f: (Int, Int) => T)(n1: Int, n2: Int): Array[Array[T]] =
    fromFunction(i => fromFunction(f(i, _))(n2))(n1)

  /** Create an array containing the values of a given function <code>f</code>
   *  over given range <code>[0..n1, 0..n2, 0..n3)</code>
   */
  @deprecated("use `Array.tabulate' instead")
  def fromFunction[T: ClassManifest](f: (Int, Int, Int) => T)(n1: Int, n2: Int, n3: Int): Array[Array[Array[T]]] =
    fromFunction(i => fromFunction(f(i, _, _))(n2, n3))(n1)

  /** Create an array containing the values of a given function <code>f</code>
   *  over given range <code>[0..n1, 0..n2, 0..n3, 0..n4)</code>
   */
  @deprecated("use `Array.tabulate' instead")
  def fromFunction[T: ClassManifest](f: (Int, Int, Int, Int) => T)(n1: Int, n2: Int, n3: Int, n4: Int): Array[Array[Array[Array[T]]]] =
    fromFunction(i => fromFunction(f(i, _, _, _))(n2, n3, n4))(n1)

  /** Create an array containing the values of a given function <code>f</code>
   *  over given range <code>[0..n1, 0..n2, 0..n3, 0..n4, 0..n5)</code>
   */
  @deprecated("use `Array.tabulate' instead")
  def fromFunction[T: ClassManifest](f: (Int, Int, Int, Int, Int) => T)(n1: Int, n2: Int, n3: Int, n4: Int, n5: Int): Array[Array[Array[Array[Array[T]]]]] =
    fromFunction(i => fromFunction(f(i, _, _, _, _))(n2, n3, n4, n5))(n1)
}

/** This class represents polymorphic arrays. <code>Array[T]</code> is Scala's representation
 *  for Java's <code>T[]</code>.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
final class Array[T](_length: Int) {

   /** Multidimensional array creation */
   @deprecated("use `Array.ofDim' instead")
   def this(dim1: Int, dim2: Int) = {
     this(dim1)
     throw new Error()
   }

   /** Multidimensional array creation */
   @deprecated("use `Array.ofDim' instead")
   def this(dim1: Int, dim2: Int, dim3: Int) = {
     this(dim1)
     throw new Error()
   }

   /** Multidimensional array creation */
   @deprecated("use `Array.ofDim' instead")
   def this(dim1: Int, dim2: Int, dim3: Int, dim4: Int) = {
     this(dim1)
     throw new Error()
   }

   /** Multidimensional array creation */
   @deprecated("use `Array.ofDim' instead")
   def this(dim1: Int, dim2: Int, dim3: Int, dim4: Int, dim5: Int) = {
     this(dim1);
     throw new Error()
   }

   /** Multidimensional array creation */
   @deprecated("use `Array.ofDim' instead")
   def this(dim1: Int, dim2: Int, dim3: Int, dim4: Int, dim5: Int, dim6: Int) = {
     this(dim1)
     throw new Error()
   }

   /** Multidimensional array creation */
   @deprecated("use `Array.ofDim' instead")
   def this(dim1: Int, dim2: Int, dim3: Int, dim4: Int, dim5: Int, dim6: Int, dim7: Int) = {
     this(dim1)
     throw new Error()
   }

   /** Multidimensional array creation */
   @deprecated("use `Array.ofDim' instead")
   def this(dim1: Int, dim2: Int, dim3: Int, dim4: Int, dim5: Int, dim6: Int, dim7: Int, dim8: Int) = {
     this(dim1)
     throw new Error()
   }

   /** Multidimensional array creation */
   @deprecated("use `Array.ofDim' instead")
   def this(dim1: Int, dim2: Int, dim3: Int, dim4: Int, dim5: Int, dim6: Int, dim7: Int, dim8: Int, dim9: Int) = {
     this(dim1)
     throw new Error()
   }

  /** The length of the array */
  def length: Int = throw new Error()

  /** The element at given index.
   *  <p>
   *    Indices start a <code>0</code>; <code>xs.apply(0)</code> is the first
   *    element of array <code>xs</code>.
   *  </p>
   *  <p>
   *    Note the indexing syntax <code>xs(i)</code> is a shorthand for
   *    <code>xs.apply(i)</code>.
   *  </p>
   *
   *  @param i   the index
   *  @throws ArrayIndexOutOfBoundsException if <code>i < 0</code> or
   *          <code>length <= i</code>
   */
  def apply(i: Int): T = throw new Error()

  /** <p>
   *    Update the element at given index.
   *  </p>
   *  <p>
   *    Indices start a <code>0</code>; <code>xs.apply(0)</code> is the first
   *    element of array <code>xs</code>.
   *  </p>
   *  <p>
   *    Note the indexing syntax <code>xs(i) = x</code> is a shorthand
   *    for <code>xs.update(i, x)</code>.
   *  </p>
   *
   *  @param i   the index
   *  @param x   the value to be written at index <code>i</code>
   *  @throws ArrayIndexOutOfBoundsException if <code>i < 0</code> or
   *          <code>length <= i</code>
   */
  def update(i: Int, x: T) { throw new Error() }

  /** <p>
   *    Clone the Array.
   *  </p>
   *
   *  @return A clone of the Array.
   */
  override def clone: Array[T] = throw new Error()
}
