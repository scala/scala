/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
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

}

/** This class represents polymorphic arrays. It is never instantiated.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
final class Array[A](_length: Int) extends Seq[A] {
  import Predef.Error
  def length: Int = throw new Error()
  def apply(i: Int): A = throw new Error()
  def update(i: Int, x: A): Unit = throw new Error()
  def elements: Iterator[A] = throw new Error()
  def subArray(from: Int, end: Int): Array[A] = throw new Error()
  def filter(p: A => Boolean): Array[A] = throw new Error()
  def map[B](f: A => B): Array[B] = throw new Error()
  def flatMap[B](f: A => Array[B]): Array[B] = throw new Error()
  def zip[B](that: Array[B]): Array[Tuple2[A,B]] = throw new Error()
  def zipWithIndex: Array[Tuple2[A,Int]] = throw new Error()
}
