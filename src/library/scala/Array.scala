/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import scala.collection.generic._
import scala.collection.Traversable
import scala.collection.mutable.{Vector, ArrayBuilder}
import compat.Platform.arraycopy
import scala.reflect.ClassManifest

/** This object contains utility methods operating on arrays.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
object Array {

  import runtime.BoxedArray;
  import scala.runtime.ScalaRunTime.boxArray;

  implicit def builderFactory[A]/* !!!(implicit m: ClassManifest[A])*/: BuilderFactory[A, Array[A], Array[_]] =
    new BuilderFactory[A, Array[A], Array[_]] { def apply(from: Array[_]) = newBuilder[A](null) }

  def newBuilder[A](implicit m: ClassManifest[A]): Builder[A, Array[A]] =
    new ArrayBuilder[A](m).asInstanceOf[Builder[A, Array[A]]] // the cast is safe, because the erasure of Array[A] is BoxedArray[A]

  private def slowcopy(
                     src : AnyRef,
                  srcPos : Int,
                    dest : AnyRef,
                 destPos : Int,
                  length : Int) {

    val srcArray = boxArray(src).asInstanceOf[BoxedArray[AnyRef]]
    val destArray = boxArray(dest).asInstanceOf[BoxedArray[AnyRef]]

    var i = 0;
    while (i < length) {
      destArray(destPos + i) = srcArray(srcPos + i)
      i += 1
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
  def empty[A: ClassManifest]: Array[A] = new Array[A](0)

  /** Create an array with given elements.
   *
   *  @param xs the elements to put in the array
   *  @return the array containing elements xs.
   */
  def apply[A: ClassManifest](xs: A*): Array[A] = {
    val array = new Array[A](xs.length)
    var i = 0
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  def apply(xs: Boolean*): Array[Boolean] = {
    val array = new Array[Boolean](xs.length)
    var i = 0
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  def apply(xs: Byte*): Array[Byte] = {
    val array = new Array[Byte](xs.length)
    var i = 0
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  def apply(xs: Short*): Array[Short] = {
    val array = new Array[Short](xs.length)
    var i = 0
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  def apply(xs: Char*): Array[Char] = {
    val array = new Array[Char](xs.length)
    var i = 0
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  def apply(xs: Int*): Array[Int] = {
    val array = new Array[Int](xs.length)
    var i = 0
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  def apply(xs: Long*): Array[Long] = {
    val array = new Array[Long](xs.length)
    var i = 0
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  def apply(xs: Float*): Array[Float] = {
    val array = new Array[Float](xs.length)
    var i = 0
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  def apply(xs: Double*): Array[Double] = {
    val array = new Array[Double](xs.length)
    var i = 0
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  def apply(xs: Unit*): Array[Unit] = {
    val array = new Array[Unit](xs.length)
    var i = 0
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  /** Create array with given dimensions */
  def ofDim[A: ClassManifest](n1: Int): Array[A] =
    new Array[A](n1)
  def ofDim[A: ClassManifest](n1: Int, n2: Int): Array[Array[A]] =
    tabulate(n1)(_ => ofDim[A](n2))
  def ofDim[A: ClassManifest](n1: Int, n2: Int, n3: Int): Array[Array[Array[A]]] =
    tabulate(n1)(_ => ofDim[A](n2, n3))
  def ofDim[A: ClassManifest](n1: Int, n2: Int, n3: Int, n4: Int): Array[Array[Array[Array[A]]]] =
    tabulate(n1)(_ => ofDim[A](n2, n3, n4))
  def ofDim[A: ClassManifest](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int): Array[Array[Array[Array[Array[A]]]]] =
    tabulate(n1)(_ => ofDim[A](n2, n3, n4, n5))

  /** Concatenate all argument sequences into a single array.
   *
   *  @param xs the given argument sequences
   *  @return   the array created from the concatenated arguments
   */
  def concat[A: ClassManifest](xss: Traversable[A]*): Array[A] = {
    val b = newBuilder[A]
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
  def fill[A: ClassManifest](n: Int)(elem: => A): Array[A] = {
    val b = newBuilder[A]
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
  def fill[A: ClassManifest](n1: Int, n2: Int)(elem: => A): Array[Array[A]] =
    tabulate(n1)(_ => fill(n2)(elem))

  /** A three-dimensional array that contains the results of some element
   *  computation a number of times.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   elem the element computation
   */
  def fill[A: ClassManifest](n1: Int, n2: Int, n3: Int)(elem: => A): Array[Array[Array[A]]] =
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
  def fill[A: ClassManifest](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => A): Array[Array[Array[Array[A]]]] =
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
  def fill[A: ClassManifest](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(elem: => A): Array[Array[Array[Array[Array[A]]]]] =
    tabulate(n1)(_ => fill(n2, n3, n4, n5)(elem))

  /** An array containing values of a given function over a range of integer
   *  values starting from 0.
   *
   *  @param  n   The number of elements in the traversable
   *  @param  f   The function computing element values
   *  @return A traversable consisting of elements `f(0), ..., f(n -1)`
   */
  def tabulate[A: ClassManifest](n: Int)(f: Int => A): Array[A] = {
    val b = newBuilder[A]
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
  def tabulate[A: ClassManifest](n1: Int, n2: Int)(f: (Int, Int) => A): Array[Array[A]] =
    tabulate(n1)(i1 => tabulate(n2)(f(i1, _)))

  /** A three-dimensional array containing values of a given function over
   *  ranges of integer values starting from 0.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   f   The function computing element values
   */
  def tabulate[A: ClassManifest](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => A): Array[Array[Array[A]]] =
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
  def tabulate[A: ClassManifest](n1: Int, n2: Int, n3: Int, n4: Int)(f: (Int, Int, Int, Int) => A): Array[Array[Array[Array[A]]]] =
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
  def tabulate[A: ClassManifest](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(f: (Int, Int, Int, Int, Int) => A): Array[Array[Array[Array[Array[A]]]]] =
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
  def iterate[A: ClassManifest](start: A, len: Int)(f: A => A): Array[A] = {
    val b = newBuilder[A]
    var acc = start
    var i = 0
    while (i < len) {
      b += acc
      acc = f(acc)
      i += 1
    }
    b.result
  }

  /** This method is called in a pattern match { case Sequence(...) => }.
   *
   *  @param x the selector value
   *  @return  sequence wrapped in an option, if this is a Sequence, otherwise none
   */
  def unapplySeq[A](x: Array[A]): Some[Array[A]] = Some(x)

  /** Create an array containing several copies of an element.
   *
   *  @param n    the length of the resulting array
   *  @param elem the element composing the resulting array
   *  @return     an array composed of n elements all equal to elem
   */
  @deprecated("use `Array.fill' instead")
  def make[A: ClassManifest](n: Int, elem: A): Array[A] = {
    val a = new Array[A](n)
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
  def fromFunction[A: ClassManifest](f: Int => A)(n: Int): Array[A] = {
    val a = new Array[A](n)
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
  def fromFunction[A: ClassManifest](f: (Int, Int) => A)(n1: Int, n2: Int): Array[Array[A]] =
    fromFunction(i => fromFunction(f(i, _))(n2))(n1)

  /** Create an array containing the values of a given function <code>f</code>
   *  over given range <code>[0..n1, 0..n2, 0..n3)</code>
   */
  @deprecated("use `Array.tabulate' instead")
  def fromFunction[A: ClassManifest](f: (Int, Int, Int) => A)(n1: Int, n2: Int, n3: Int): Array[Array[Array[A]]] =
    fromFunction(i => fromFunction(f(i, _, _))(n2, n3))(n1)

  /** Create an array containing the values of a given function <code>f</code>
   *  over given range <code>[0..n1, 0..n2, 0..n3, 0..n4)</code>
   */
  @deprecated("use `Array.tabulate' instead")
  def fromFunction[A: ClassManifest](f: (Int, Int, Int, Int) => A)(n1: Int, n2: Int, n3: Int, n4: Int): Array[Array[Array[Array[A]]]] =
    fromFunction(i => fromFunction(f(i, _, _, _))(n2, n3, n4))(n1)

  /** Create an array containing the values of a given function <code>f</code>
   *  over given range <code>[0..n1, 0..n2, 0..n3, 0..n4, 0..n5)</code>
   */
  @deprecated("use `Array.tabulate' instead")
  def fromFunction[A: ClassManifest](f: (Int, Int, Int, Int, Int) => A)(n1: Int, n2: Int, n3: Int, n4: Int, n5: Int): Array[Array[Array[Array[Array[A]]]]] =
    fromFunction(i => fromFunction(f(i, _, _, _, _))(n2, n3, n4, n5))(n1)
}

/** This class represents polymorphic arrays. <code>Array[T]</code> is Scala's representation
 *  for Java's <code>T[]</code>.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
final class Array[A](_length: Int) extends Vector[A]
                                      with TraversableClass[A, Array]
                                      with VectorTemplate[A, Array[A]] {

  override def companion: Companion[Array] = throw new Error()

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
  def apply(i: Int): A = throw new Error()

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
  override def update(i: Int, x: A) { throw new Error() }

  /** Creates a possible nested vector which consists of all the elements
   *  of this array. If the elements are arrays themselves, the `deep' transformation
   *  is applied recursively to them. The stringPrefix of the vector is
   *  "Array", hence the vector prints like an array with all its
   *  elements shown, and the same recursively for any subarrays.
   *
   *  Example:   Array(Array(1, 2), Array(3, 4)).deep.toString
   *  prints:    Array(Array(1, 2), Array(3, 4))
   */
  def deep: Vector[Any] = throw new Error()

  /**
   *  @return a deep string representation of this array.
   */
  @deprecated("use deep.toString instead")
  def deepToString(): String = throw new Error()

  /** <p>
   *    Returns a string representation of this array object. The resulting string
   *    begins with the string <code>start</code> and is finished by the string
   *    <code>end</code>. Inside, the string representations of elements (w.r.t.
   *    the method <code>deepToString()</code>) are separated by the string
   *    <code>sep</code>. For example:
   *  </p>
   *  <p>
   *    <code>Array(Array(1, 2), Array(3)).deepMkString("[", "; ", "]") = "[[1; 2]; [3]]"</code>
   *  </p>
   *
   *  @param start starting string.
   *  @param sep separator string.
   *  @param end ending string.
   *  @return a string representation of this array object.
   */
  @deprecated("use deep.mkString instead")
  def deepMkString(start: String, sep: String, end: String): String =
    throw new Error()

  /** Returns a string representation of this array object. The string
   *  representations of elements (w.r.t. the method <code>deepToString()</code>)
   *  are separated by the string <code>sep</code>.
   *
   *  @param sep separator string.
   *  @return a string representation of this array object.
   */
  @deprecated("use deep.mkString instead")
  def deepMkString(sep: String): String = throw new Error()

  /** <p>
   *    Returns <code>true</code> if the two specified arrays are
   *    <em>deeply equal</em> to one another.
   *  </p>
   *  <p>
   *    Two array references are considered deeply equal if both are null,
   *    or if they refer to arrays that contain the same number of elements
   *    and all corresponding pairs of elements in the two arrays are deeply
   *    equal.
   *  </p>
   *  <p>
   *    See also method <code>deepEquals</code> in the Java class
   *    <a href="http://java.sun.com/javase/6/docs/api/java/util/Arrays.html"
   *    target="_top">java.utils.Arrays</a>
   *  </p>
   *
   *  @param that the second
   *  @return     <code>true</code> iff both arrays are deeply equal.
   */
  @deprecated("use array1.deep.equals(array2.deep) instead")
  def deepEquals(that: Any): Boolean = throw new Error()
}
