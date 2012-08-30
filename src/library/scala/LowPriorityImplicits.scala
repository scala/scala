/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import scala.collection.{ mutable, immutable, generic }
import mutable.WrappedArray
import immutable.WrappedString
import generic.CanBuildFrom
import language.implicitConversions

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
   *
   *  Note - these are inlined because they are value classes, but
   *  the call to xxxWrapper is not eliminated even though it does nothing.
   *  Even inlined, every call site does a no-op retrieval of Predef's MODULE$
   *  because maybe loading Predef has side effects!
   */
  @inline implicit def byteWrapper(x: Byte)       = new runtime.RichByte(x)
  @inline implicit def shortWrapper(x: Short)     = new runtime.RichShort(x)
  @inline implicit def intWrapper(x: Int)         = new runtime.RichInt(x)
  @inline implicit def charWrapper(c: Char)       = new runtime.RichChar(c)
  @inline implicit def longWrapper(x: Long)       = new runtime.RichLong(x)
  @inline implicit def floatWrapper(x: Float)     = new runtime.RichFloat(x)
  @inline implicit def doubleWrapper(x: Double)   = new runtime.RichDouble(x)
  @inline implicit def booleanWrapper(x: Boolean) = new runtime.RichBoolean(x)

  // These eight implicits exist solely to exclude Null from the domain of
  // the boxed types, so that e.g. "var x: Int = null" is a compile time
  // error rather than a delayed null pointer exception by way of the
  // conversion from java.lang.Integer.  If defined in the same file as
  // Integer2int, they would have higher priority because Null is a subtype
  // of Integer.  We balance that out and create conflict by moving the
  // definition into the superclass.
  //
  // Caution: do not adjust tightrope tension without safety goggles in place.
  implicit def Byte2byteNullConflict(x: Null): Byte          = sys.error("value error")
  implicit def Short2shortNullConflict(x: Null): Short       = sys.error("value error")
  implicit def Character2charNullConflict(x: Null): Char     = sys.error("value error")
  implicit def Integer2intNullConflict(x: Null): Int         = sys.error("value error")
  implicit def Long2longNullConflict(x: Null): Long          = sys.error("value error")
  implicit def Float2floatNullConflict(x: Null): Float       = sys.error("value error")
  implicit def Double2doubleNullConflict(x: Null): Double    = sys.error("value error")
  implicit def Boolean2booleanNullConflict(x: Null): Boolean = sys.error("value error")

  implicit def genericWrapArray[T](xs: Array[T]): WrappedArray[T] =
    if (xs eq null) null
    else WrappedArray.make(xs)

  // Since the JVM thinks arrays are covariant, one 0-length Array[AnyRef]
  // is as good as another for all T <: AnyRef.  Instead of creating 100,000,000
  // unique ones by way of this implicit, let's share one.
  implicit def wrapRefArray[T <: AnyRef](xs: Array[T]): WrappedArray[T] = {
    if (xs eq null) null
    else if (xs.length == 0) WrappedArray.empty[T]
    else new WrappedArray.ofRef[T](xs)
  }

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

  implicit def fallbackStringCanBuildFrom[T]: CanBuildFrom[String, T, immutable.IndexedSeq[T]] =
    new CanBuildFrom[String, T, immutable.IndexedSeq[T]] {
      def apply(from: String) = immutable.IndexedSeq.newBuilder[T]
      def apply() = immutable.IndexedSeq.newBuilder[T]
    }
}

