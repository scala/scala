/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import compat.Platform.arraycopy

/** This object ...
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
    for (val x <- xs) {
      len = len + x.length
    }
    val result = new Array[T](len)
    var start = 0
    for (val x <- xs) {
      copy(x, 0, result, start, x.length)
      start = start + x.length
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
    for (val i <- Iterator.range(start, end)) result(i - start) = i
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
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
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
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array
  }
*/

  def apply(xs: Boolean*): Array[Boolean] = {
    val array = new Array[Boolean](xs.length)
    var i = 0
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array
  }
  def apply(xs: Byte*): Array[Byte] = {
    val array = new Array[Byte](xs.length)
    var i = 0
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array
  }
  def apply(xs: Short*): Array[Short] = {
    val array = new Array[Short](xs.length)
    var i = 0
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array
  }
  def apply(xs: Char*): Array[Char] = {
    val array = new Array[Char](xs.length)
    var i = 0
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array
  }
  def apply(xs: Int*): Array[Int] = {
    val array = new Array[Int](xs.length)
    var i = 0
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array
  }
  def apply(xs: Long*): Array[Long] = {
    val array = new Array[Long](xs.length)
    var i = 0
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array
  }
  def apply(xs: Float*): Array[Float] = {
    val array = new Array[Float](xs.length)
    var i = 0
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array
  }
  def apply(xs: Double*): Array[Double] = {
    val array = new Array[Double](xs.length)
    var i = 0
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array
  }
  def apply(xs: Unit*): Array[Unit] = {
    val array = new Array[Unit](xs.length)
    var i = 0
    for (val x <- xs.elements) { array(i) = x; i = i + 1; }
    array
  }

  /** Create an array containing several copies of an element.
   *
   *  @param n    the length of the resulting array
   *  @param elem the element composing the resulting array
   *  @return     an array composed of n elements all equal to elem
   */
  def make[a](n: Int, elem: a): Array[a] = {
    val a = new Array[a](n)
    var i = 0
    while (i < n) {
      a(i) = elem
      i = i + 1
    }
    a
  }

 /** This method is called in a pattern match { case Array(...) => }.
   *
   *  @param x the selector value
   *  @return  array wrapped in an option, if this is a Seq, otherwise none
   */
  def unapplySeq[A](x: Any): Option[Seq[A]] =
    if (x.isInstanceOf[Array[A]]) Some(x.asInstanceOf[Array[A]]) else None
}

/** This class represents polymorphic arrays. It is never instantiated.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
final class Array[A](_length: Int) extends Seq[A] {
  import Predef.Error

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

  /** Update the element at given index.
   *  Indices start a <code>0</code>; <code>xs.apply(0)</code> is the first
   *  element of array <code>xs</code>.
   *  Note the indexing syntax <code>xs(i) = x</code> is a shorthand
   *  for <code>xs.update(i, x)</code>.
   *
   *  @param i   the index
   *  @param x   the value to be written at index <code>i</code>
   *  @throws ArrayIndexOutOfBoundsException if <code>i < 0</code> or
   *          <code>length <= i</code>
   */
  def update(i: Int, x: A): Unit = throw new Error()

  /** An iterator returning the elements of this array, starting from 0.
   */
  def elements: Iterator[A] = throw new Error()

  /** @deprecated  use slice instead */
  def subArray(from: Int, end: Int): Array[A] = throw new Error()

  /** A sub-array of <code>len</code> elements
   *  starting at index <code>from</code>
   *
   *  @param from   The index of the first element of the slice
   *  @param end    The index of the element following the slice
   *  @throws IndexOutOfBoundsException if <code>from &lt; 0</code>
   *          or <code>length &lt; from + len<code>
   */
  override def slice(from: Int, end: Int): Array[A] = throw new Error()

  /** Returns an array consisting of all elements of this array that satisfy the
   *  predicate <code>p</code>. The order of the elements is preserved.
   *
   *  @param p the predicate used to filter the array.
   *  @return the elements of this array satisfying <code>p</code>.
   */
  override def filter(p: A => Boolean): Array[A] = throw new Error()

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

  /**
   *  @return a deep string representation of this sequence.
   */
  def deepToString(): String = throw new Error()

  /** Returns a string representation of this array object. The resulting string
   *  begins with the string <code>start</code> and is finished by the string
   *  <code>end</code>. Inside, the string representations of elements (w.r.t.
   *  the method <code>deepToString()</code>) are separated by the string
   *  <code>sep</code>.
   *  <p/>
   *  Ex: <br/>
   *  <code>Array(Array(1, 2), Array(3)).deepMkString("[", "; ", "]") = "[[1; 2]; 3]"</code>
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

}
