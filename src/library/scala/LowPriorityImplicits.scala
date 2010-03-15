/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


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

  implicit def genericWrapArray[T](xs: Array[T]): WrappedArray[T] =
    if (xs != null) WrappedArray.make(xs) else null

  implicit def wrapRefArray[T <: AnyRef](xs: Array[T]): WrappedArray[T] = if (xs != null) new WrappedArray.ofRef[T](xs) else null
  implicit def wrapIntArray(xs: Array[Int]): WrappedArray[Int] = if (xs != null) new WrappedArray.ofInt(xs) else null
  implicit def wrapDoubleArray(xs: Array[Double]): WrappedArray[Double] = if (xs != null) new WrappedArray.ofDouble(xs) else null
  implicit def wrapLongArray(xs: Array[Long]): WrappedArray[Long] = if (xs != null) new WrappedArray.ofLong(xs) else null
  implicit def wrapFloatArray(xs: Array[Float]): WrappedArray[Float] = if (xs != null) new WrappedArray.ofFloat(xs) else null
  implicit def wrapCharArray(xs: Array[Char]): WrappedArray[Char] = if (xs != null) new WrappedArray.ofChar(xs) else null
  implicit def wrapByteArray(xs: Array[Byte]): WrappedArray[Byte] = if (xs != null) new WrappedArray.ofByte(xs) else null
  implicit def wrapShortArray(xs: Array[Short]): WrappedArray[Short] = if (xs != null) new WrappedArray.ofShort(xs) else null
  implicit def wrapBooleanArray(xs: Array[Boolean]): WrappedArray[Boolean] = if (xs != null) new WrappedArray.ofBoolean(xs) else null
  implicit def wrapUnitArray(xs: Array[Unit]): WrappedArray[Unit] = if (xs != null) new WrappedArray.ofUnit(xs) else null

  implicit def wrapString(s: String): WrappedString = if (s != null) new WrappedString(s) else null
  implicit def unwrapString(ws: WrappedString): String = if (ws != null) ws.self else null

  implicit def fallbackStringCanBuildFrom[T]: CanBuildFrom[String, T, collection.immutable.IndexedSeq[T]] =
    new CanBuildFrom[String, T, collection.immutable.IndexedSeq[T]] {
      def apply(from: String) = scala.collection.immutable.IndexedSeq.newBuilder[T]
      def apply() = scala.collection.immutable.IndexedSeq.newBuilder[T]
    }

  /** Can go away after next newstarr */
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
