/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import Predef._
import scala.reflect.ClassManifest
import scala.collection.generic._

/**
 *  <p>A class representing <code>Array[T]</code></p>
 *
 *  @author  Martin Odersky, Stephane Micheloud
 *  @version 1.0
 *  @since 2.8
 */
abstract class WrappedArray[T] extends Vector[T] with ArrayLike[T, WrappedArray[T]] {

  override protected[this] def thisCollection: WrappedArray[T] = this
  override protected[this] def toCollection(repr: WrappedArray[T]): WrappedArray[T] = repr

  /** The manifest of the element type */
  def elemManifest: ClassManifest[T]

  /** The length of the array */
  def length: Int

  /** The element at given index */
  def apply(index: Int): T

  /** Update element at given index */
  def update(index: Int, elem: T): Unit

  /** The underlying array */
  def array: Array[T]
  override def stringPrefix = "WrappedArray"

  /** Creates new builder for this collection ==> move to subclasses
   */
  override protected[this] def newBuilder: Builder[T, WrappedArray[T]] =
    new WrappedArrayBuilder[T](elemManifest)
}

object WrappedArray {

  def make[T](x: AnyRef): WrappedArray[T] = x match {
    case x: Array[AnyRef] => wrapRefArray[AnyRef](x).asInstanceOf[WrappedArray[T]]
    case x: Array[Int] => wrapIntArray(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Double] => wrapDoubleArray(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Long] => wrapLongArray(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Float] => wrapFloatArray(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Char] => wrapCharArray(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Byte] => wrapByteArray(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Short] => wrapShortArray(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Boolean] => wrapBooleanArray(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Unit] => wrapUnitArray(x).asInstanceOf[WrappedArray[T]]
  }

  implicit def canBuildFrom[T](implicit m: ClassManifest[T]): CanBuildFrom[WrappedArray[_], T, WrappedArray[T]] =
    new CanBuildFrom[WrappedArray[_], T, WrappedArray[T]] {
      def apply(from: WrappedArray[_]): Builder[T, WrappedArray[T]] =
        ArrayBuilder.make[T]()(m) mapResult WrappedArray.make[T]
      def apply: Builder[T, WrappedArray[T]] =
        ArrayBuilder.make[T]()(m) mapResult WrappedArray.make[T]
  }

  def newBuilder[A]: Builder[A, Vector[A]] = new ArrayBuffer

  @serializable
  final class ofRef[T <: AnyRef](val array: Array[T]) extends WrappedArray[T] {
    lazy val elemManifest = ClassManifest.classType[T](array.getClass.getComponentType)
    def length: Int = array.length
    def apply(index: Int): T = array(index).asInstanceOf[T]
    def update(index: Int, elem: T) { array(index) = elem }
  }

  @serializable
  final class ofByte(val array: Array[Byte]) extends WrappedArray[Byte] {
    def elemManifest = ClassManifest.Byte
    def length: Int = array.length
    def apply(index: Int): Byte = array(index)
    def update(index: Int, elem: Byte) { array(index) = elem }
  }

  @serializable
  final class ofShort(val array: Array[Short]) extends WrappedArray[Short] {
    def elemManifest = ClassManifest.Short
    def length: Int = array.length
    def apply(index: Int): Short = array(index)
    def update(index: Int, elem: Short) { array(index) = elem }
  }

  @serializable
  final class ofChar(val array: Array[Char]) extends WrappedArray[Char] {
    def elemManifest = ClassManifest.Char
    def length: Int = array.length
    def apply(index: Int): Char = array(index)
    def update(index: Int, elem: Char) { array(index) = elem }
  }

  @serializable
  final class ofInt(val array: Array[Int]) extends WrappedArray[Int] {
    def elemManifest = ClassManifest.Int
    def length: Int = array.length
    def apply(index: Int): Int = array(index)
    def update(index: Int, elem: Int) { array(index) = elem }
  }

  @serializable
  final class ofLong(val array: Array[Long]) extends WrappedArray[Long] {
    def elemManifest = ClassManifest.Long
    def length: Int = array.length
    def apply(index: Int): Long = array(index)
    def update(index: Int, elem: Long) { array(index) = elem }
  }

  @serializable
  final class ofFloat(val array: Array[Float]) extends WrappedArray[Float] {
    def elemManifest = ClassManifest.Float
    def length: Int = array.length
    def apply(index: Int): Float = array(index)
    def update(index: Int, elem: Float) { array(index) = elem }
  }

  @serializable
  final class ofDouble(val array: Array[Double]) extends WrappedArray[Double] {
    def elemManifest = ClassManifest.Double
    def length: Int = array.length
    def apply(index: Int): Double = array(index)
    def update(index: Int, elem: Double) { array(index) = elem }
  }

  @serializable
  final class ofBoolean(val array: Array[Boolean]) extends WrappedArray[Boolean] {
    def elemManifest = ClassManifest.Boolean
    def length: Int = array.length
    def apply(index: Int): Boolean = array(index)
    def update(index: Int, elem: Boolean) { array(index) = elem }
  }

  @serializable
  final class ofUnit(val array: Array[Unit]) extends WrappedArray[Unit] {
    def elemManifest = ClassManifest.Unit
    def length: Int = array.length
    def apply(index: Int): Unit = array(index)
    def update(index: Int, elem: Unit) { array(index) = elem }
  }
}
