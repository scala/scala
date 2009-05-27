/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala


import Predef._
import compat.Platform.arraycopy

/** This object contains utility methods operating on arrays.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
object Array {

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
  def copy(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int): Unit = src match {
    case xs: runtime.BoxedArray =>
      xs.copyTo(srcPos, dest, destPos, length)
    case _ =>
      dest match {
        case xs: runtime.BoxedArray =>
          xs.copyFrom(src, srcPos, destPos, length)
        case _ =>
          arraycopy(src, srcPos, dest, destPos, length)
      }
  }

  /** Concatenate all argument arrays into a single array.
   *
   *  @param xs ...
   */
  def concat[T](xs: Array[T]*) = {
    var len = 0
    for (x <- xs) len += x.length
    val result = new Array[T](len)
    var start = 0
    for (x <- xs) {
      copy(x, 0, result, start, x.length)
      start += x.length
    }
    result
  }

  /** Create a an array containing of successive integers.
   *
   *  @param from the value of the first element of the array
   *  @param end  the value of the last element fo the array plus 1
   *  @return the sorted array of all integers in range [from;end).
   */
  def range(start: Int, end: Int): Array[Int] = {
    val result = new Array[Int](end - start)
    for (i <- start until end) result(i - start) = i
    result
  }

  /** Create an array with given elements.
   *
   *  @param xs the elements to put in the array
   *  @return the array containing elements xs.
   */
  def apply[A <: AnyRef](xs: A*): Array[A] = {
    val array = new Array[A](xs.length)
    var i = 0
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }


/* The following metod clashes with the previous one, and has therefore been
 * removed. Note that this is a choice between efficiency and generality.
 * The previous factory method is more efficient than the one that has been
 * commented out. Since it is anyway possible to create a polymorphic array
 * using
 *        new Array[T]
 * it was preferred to restrict the definition of the factory method.

   def Array[A](xs: A*): Array[A] = {
    val array = new Array[A](xs.length)
    var i = 0
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }
*/

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
  def apply(xs: Unit*): Array[Unit] = {
    val array = new Array[Unit](xs.length)
    var i = 0
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  /** Create an array containing several copies of an element.
   *
   *  @param n    the length of the resulting array
   *  @param elem the element composing the resulting array
   *  @return     an array composed of n elements all equal to elem
   */
  def make[A](n: Int, elem: A): Array[A] = {
    val a = new Array[A](n)
    var i = 0
    while (i < n) {
      a(i) = elem
      i += 1
    }
    a
  }

 /** This method is called as a result of a pattern match { case Array(...) => } or val Array(...) = ....
   *
   *  @param x the selector value
   *  @return  array wrapped in an option
   */
   def unapplySeq[A](x: Array[A]): Option[Seq[A]] = Some(x)

   trait Projection[A] extends RandomAccessSeq.MutableProjection[A] {
     protected def newArray[B >: A](length : Int, elements : Iterator[A]) : Array[B]
     override def toArray[B >: A] = (newArray(length, elements))//:Any).asInstanceOf[Array[B]]
     override def force : Array[A] = toArray
     override def drop( from: Int) = slice(from, length)
     override def take(until: Int) = slice(0, until)
     override def slice(from0 : Int, until0 : Int) : Projection[A] = new RandomAccessSeq.MutableSlice[A] with Projection[A] {
       override def from = from0
       override def until = until0
       override def underlying = Projection.this
       override protected def newArray[B >: A](length : Int, elements : Iterator[A]) =
         underlying.newArray(length, elements)
       override def slice(from0 : Int, until0 : Int) =
         Projection.this.slice(from + from0, from + until0)
     }
     override def reverse : Projection[A] = new Projection[A] {
       override protected def newArray[B >: A](length : Int, elements : Iterator[A]) =
         Projection.this.newArray(length, elements)
       def update(idx : Int, what : A) : Unit = Projection.this.update(length - idx - 1, what)
       def length = Projection.this.length
       def apply(idx : Int) = Projection.this.apply(length - idx - 1)
       override def stringPrefix = Projection.this.stringPrefix + "R"
     }
   }
   trait Array0[A] extends RandomAccessSeq.Mutable[A] {
     override def projection : Projection[A] = throw new Error
     override def slice(from : Int, until : Int) : Projection[A] = projection.slice(from, until)
     override def take(until : Int) : Projection[A] = projection.take(until)
     override def drop(from : Int) : Projection[A] = projection.drop(from)
     override def reverse = projection.reverse
   }
}

/** This class represents polymorphic arrays. <code>Array[T]</code> is Scala's representation
 *  for Java's <code>T[]</code>.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
final class Array[A](_length: Int) extends Array.Array0[A] {

   /** Multidimensional array creation */
   def this(dim1: Int, dim2: Int) = {
     this(dim1);
     throw new Error()
   }

   /** Multidimensional array creation */
   def this(dim1: Int, dim2: Int, dim3: Int) = {
     this(dim1);
     throw new Error()
   }

   /** Multidimensional array creation */
   def this(dim1: Int, dim2: Int, dim3: Int, dim4: Int) = {
     this(dim1);
     throw new Error()
   }

   /** Multidimensional array creation */
   def this(dim1: Int, dim2: Int, dim3: Int, dim4: Int, dim5: Int) = {
     this(dim1);
     throw new Error()
   }

   /** Multidimensional array creation */
   def this(dim1: Int, dim2: Int, dim3: Int, dim4: Int, dim5: Int, dim6: Int) = {
     this(dim1);
     throw new Error()
   }

   /** Multidimensional array creation */
   def this(dim1: Int, dim2: Int, dim3: Int, dim4: Int, dim5: Int, dim6: Int, dim7: Int) = {
     this(dim1);
     throw new Error()
   }

   /** Multidimensional array creation */
   def this(dim1: Int, dim2: Int, dim3: Int, dim4: Int, dim5: Int, dim6: Int, dim7: Int, dim8: Int) = {
     this(dim1);
     throw new Error()
   }

   /** Multidimensional array creation */
   def this(dim1: Int, dim2: Int, dim3: Int, dim4: Int, dim5: Int, dim6: Int, dim7: Int, dim8: Int, dim9: Int) = {
     this(dim1);
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
  override def update(i: Int, x: A): Unit = throw new Error()

  /** An iterator returning the elements of this array, starting from 0.
   */
  override def iterator: Iterator[A] = throw new Error()

  /** @deprecated  use <code>slice(from,end).force</code> instead */
  def subArray(from: Int, end: Int): Array[A] = throw new Error()

  /** Returns an array consisting of all elements of this array that satisfy the
   *  predicate <code>p</code>. The order of the elements is preserved.
   *
   *  @param p the predicate used to filter the array.
   *  @return the elements of this array satisfying <code>p</code>.
   */
  override def filter(p: A => Boolean): Array[A] = throw new Error()

  /** Returns the longest prefix of this array whose elements satisfy
   *  the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @return  the longest prefix of this array whose elements satisfy
   *           the predicate <code>p</code>.
   */
  override def takeWhile(p: A => Boolean): Array[A] = throw new Error()

  /** Returns the longest suffix of this array whose first element
   *  does not satisfy the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @return  the longest suffix of the array whose first element
   *           does not satisfy the predicate <code>p</code>.
   */
  override def dropWhile(p: A => Boolean): Array[A] = throw new Error()

  /** Returns an array consisting of all elements of this array followed
   *  by all elements of the argument iterable.
   */
  override def ++[B >: A](that: Iterable[B]): Array[B] = throw new Error()

  /** Returns the array resulting from applying the given function <code>f</code> to each
   *  element of this array.
   *
   *  @param f function to apply to each element.
   *  @return <code>[f(a0), ..., f(an)]</code> if this array is <code>[a0, ..., an]</code>.
   */
  override def map[B](f: A => B): Array[B] = throw new Error()

  /** Applies the given function <code>f</code> to each element of
   *  this array, then concatenates the results.
   *
   *  @param f the function to apply on each element.
   *  @return  <code>f(a<sub>0</sub>) ::: ... ::: f(a<sub>n</sub>)</code> if
   *           this array is <code>[a<sub>0</sub>, ..., a<sub>n</sub>]</code>.
   */
  override def flatMap[B](f: A => Iterable[B]): Array[B] = throw new Error()

  /** Returns an array formed from this array and the specified array
   *  <code>that</code> by associating each element of the former with
   *  the element at the same position in the latter.
   *  If one of the two arrays is longer than the other, its remaining elements are ignored.
   *
   *  @return     <code>Array({a<sub>0</sub>,b<sub>0</sub>}, ...,
   *              {a<sub>min(m,n)</sub>,b<sub>min(m,n)</sub>})</code> when
   *              <code>Array(a<sub>0</sub>, ..., a<sub>m</sub>)
   *              zip Array(b<sub>0</sub>, ..., b<sub>n</sub>)</code> is invoked.
   */
  def zip[B](that: Array[B]): Array[Tuple2[A,B]] = throw new Error()

  /** Returns an array that pairs each element of this array
   *  with its index, counting from 0.
   *
   *  @return      the array <code>Array({a<sub>0</sub>,0}, {a<sub>1</sub>,1},...)</code>
   *               where <code>a<sub>i</sub></code> are the elements of this stream.
   */
  def zipWithIndex: Array[Tuple2[A,Int]] = throw new Error()

  /** Returns an array that contains all indices of this array */
  def indices: Array[Int] = throw new Error()

  /**
   *  @return a deep string representation of this array.
   */
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
  def deepMkString(start: String, sep: String, end: String): String =
    throw new Error()

  /** Returns a string representation of this array object. The string
   *  representations of elements (w.r.t. the method <code>deepToString()</code>)
   *  are separated by the string <code>sep</code>.
   *
   *  @param sep separator string.
   *  @return a string representation of this array object.
   */
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
  def deepEquals(that: Any): Boolean = throw new Error()

}
