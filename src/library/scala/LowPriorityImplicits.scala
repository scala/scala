/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import collection.mutable._
import collection.immutable.WrappedString
import collection.generic.CanBuildFrom

/** The `LowPriorityImplicits` class provides implicit values that
 *  are valid in all Scala compilation units without explicit qualification,
 *  but that are partially overridden by higher-priority conversions in object
 *  `Predef`.
 *
 *  @author  Martin Odersky
 *  @since 2.8
 */
class LowPriorityImplicits {
  /** We prefer the java.lang.* boxed types to these wrappers in
   *  any potential conflicts.  Conflicts do exist because the wrappers
   *  need to implement ScalaNumber in order to have a symmetric equals
   *  method, but that implies implementing java.lang.Number as well.
   */
  implicit def byteWrapper(x: Byte)       = new runtime.RichByte(x)
  implicit def shortWrapper(x: Short)     = new runtime.RichShort(x)
  implicit def intWrapper(x: Int)         = new runtime.RichInt(x)
  implicit def charWrapper(c: Char)       = new runtime.RichChar(c)
  implicit def longWrapper(x: Long)       = new runtime.RichLong(x)
  implicit def floatWrapper(x: Float)     = new runtime.RichFloat(x)
  implicit def doubleWrapper(x: Double)   = new runtime.RichDouble(x)
  implicit def booleanWrapper(x: Boolean) = new runtime.RichBoolean(x)

  implicit def genericWrapArray[T](xs: Array[T]): WrappedArray[T] =
    if (xs ne null) WrappedArray.make(xs) else null

  implicit def wrapRefArray[T <: AnyRef](xs: Array[T]): WrappedArray[T] = if (xs ne null) new WrappedArray.ofRef[T](xs) else null
  implicit def wrapIntArray(xs: Array[Int]): WrappedArray[Int] = if (xs ne null) new WrappedArray.ofInt(xs) else null
  implicit def wrapDoubleArray(xs: Array[Double]): WrappedArray[Double] = if (xs ne null) new WrappedArray.ofDouble(xs) else null
  implicit def wrapLongArray(xs: Array[Long]): WrappedArray[Long] = if (xs ne null) new WrappedArray.ofLong(xs) else null
  implicit def wrapFloatArray(xs: Array[Float]): WrappedArray[Float] = if (xs ne null) new WrappedArray.ofFloat(xs) else null
  implicit def wrapCharArray(xs: Array[Char]): WrappedArray[Char] = if (xs ne null) new WrappedArray.ofChar(xs) else null
  implicit def wrapByteArray(xs: Array[Byte]): WrappedArray[Byte] = if (xs ne null) new WrappedArray.ofByte(xs) else null
  implicit def wrapShortArray(xs: Array[Short]): WrappedArray[Short] = if (xs ne null) new WrappedArray.ofShort(xs) else null
  implicit def wrapBooleanArray(xs: Array[Boolean]): WrappedArray[Boolean] = if (xs ne null) new WrappedArray.ofBoolean(xs) else null
  implicit def wrapUnitArray(xs: Array[Unit]): WrappedArray[Unit] = if (xs ne null) new WrappedArray.ofUnit(xs) else null

  implicit def wrapString(s: String): WrappedString = if (s ne null) new WrappedString(s) else null
  implicit def unwrapString(ws: WrappedString): String = if (ws ne null) ws.self else null

  implicit def fallbackStringCanBuildFrom[T]: CanBuildFrom[String, T, collection.immutable.IndexedSeq[T]] =
    new CanBuildFrom[String, T, collection.immutable.IndexedSeq[T]] {
      def apply(from: String) = scala.collection.immutable.IndexedSeq.newBuilder[T]
      def apply() = scala.collection.immutable.IndexedSeq.newBuilder[T]
    }

  /** Can go away after next newstarr */
  /** Caution - not yet.  pos/t1459, pos/t2569, jvm/t1342 all fail without the next line. */
  def wrapArray[T <: AnyRef](xs: Array[T]): WrappedArray[T] = new WrappedArray.ofRef[T](xs)
  def wrapArray(xs: Array[Int]): WrappedArray[Int] = new WrappedArray.ofInt(xs)
  def wrapArray(xs: Array[Double]): WrappedArray[Double] = new WrappedArray.ofDouble(xs)
  def wrapArray(xs: Array[Long]): WrappedArray[Long] = new WrappedArray.ofLong(xs)
  def wrapArray(xs: Array[Float]): WrappedArray[Float] = new WrappedArray.ofFloat(xs)
  def wrapArray(xs: Array[Char]): WrappedArray[Char] = new WrappedArray.ofChar(xs)
  def wrapArray(xs: Array[Byte]): WrappedArray[Byte] = new WrappedArray.ofByte(xs)
  def wrapArray(xs: Array[Short]): WrappedArray[Short] = new WrappedArray.ofShort(xs)
  def wrapArray(xs: Array[Boolean]): WrappedArray[Boolean] = new WrappedArray.ofBoolean(xs)
  def wrapArray(xs: Array[Unit]): WrappedArray[Unit] = new WrappedArray.ofUnit(xs)
}
