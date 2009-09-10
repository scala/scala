/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Predef.scala 18558 2009-08-24 14:03:30Z moors $


package scala

import collection.mutable._
import collection.immutable.WrappedString

/** The `LowPriorityImplicits` class provides implicit values that are
 *  valid in all Scala compilation units without explicit qualification, but that
 *  are partially overridden by higher-priority conmversions in Predef
 */
class LowPriorityImplicits {

  implicit def genericArrayWrapper[T](xs: Array[T]): WrappedArray[T] = (xs: AnyRef) match { // !!! drop the AnyRef and get unreachable code errors!
    case x: Array[AnyRef] => arrayWrapper[AnyRef](x).asInstanceOf[WrappedArray[T]]
    case x: Array[Int] => arrayWrapper(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Double] => arrayWrapper(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Long] => arrayWrapper(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Float] => arrayWrapper(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Char] => arrayWrapper(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Byte] => arrayWrapper(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Short] => arrayWrapper(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Boolean] => arrayWrapper(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Unit] => arrayWrapper(x).asInstanceOf[WrappedArray[T]]
  }

  implicit def arrayWrapper[T <: AnyRef](xs: Array[T]): WrappedRefArray[T] = new WrappedRefArray[T](xs.asInstanceOf[Array[AnyRef]])
  implicit def arrayWrapper(xs: Array[Int]): WrappedIntArray = new WrappedIntArray(xs)
  implicit def arrayWrapper(xs: Array[Double]): WrappedDoubleArray = new WrappedDoubleArray(xs)
  implicit def arrayWrapper(xs: Array[Long]): WrappedLongArray = new WrappedLongArray(xs)
  implicit def arrayWrapper(xs: Array[Float]): WrappedFloatArray = new WrappedFloatArray(xs)
  implicit def arrayWrapper(xs: Array[Char]): WrappedCharArray = new WrappedCharArray(xs)
  implicit def arrayWrapper(xs: Array[Byte]): WrappedByteArray = new WrappedByteArray(xs)
  implicit def arrayWrapper(xs: Array[Short]): WrappedShortArray = new WrappedShortArray(xs)
  implicit def arrayWrapper(xs: Array[Boolean]): WrappedBooleanArray = new WrappedBooleanArray(xs)
  implicit def arrayWrapper(xs: Array[Unit]): WrappedUnitArray = new WrappedUnitArray(xs)

  implicit def wrapString(s: String): WrappedString = new WrappedString(s)
  implicit def unwrapString(ws: WrappedString): String = ws.self

}
