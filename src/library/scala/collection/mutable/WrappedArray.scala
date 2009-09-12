/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: WrappedArray.scala 18589 2009-08-27 14:45:35Z odersky $


package scala.collection.mutable

import Predef._
import scala.reflect.ClassManifest
import collection.generic._

/**
 *  <p>A class representing <code>Array[T]</code></p>
 *
 *  @author  Martin Odersky, Stephane Micheloud
 *  @version 1.0
 */
abstract class WrappedArray[A] extends Vector[A] with VectorLike[A, WrappedArray[A]] with Proxy { self =>

  override protected[this] def thisCollection: WrappedArray[A] = this
  override protected[this] def toCollection(repr: WrappedArray[A]): WrappedArray[A] = repr

  /** The manifest of the element type */
  def elemManifest: ClassManifest[A]

  /** The length of the array */
  def length: Int

  /** The element at given index */
  def apply(index: Int): A

  /** Update element at given index */
  def update(index: Int, elem: A): Unit

  /** The underlying array */
  def array: AnyRef

  /** The original of a proxy represented by a wrapped array */
  override def self = repr

  /** Creates new builder for this collection ==> move to subclasses
   */
  override protected[this] def newBuilder: Builder[A, WrappedArray[A]] =
    new WrappedArrayBuilder[A](elemManifest)
}

object WrappedArray {

  @serializable
  final class ofRef[T](val array: Array[AnyRef]) extends WrappedArray[T] {

    lazy val elemManifest = ClassManifest.classType[T](array.getClass.getComponentType)

    def length: Int = array.length

    def apply(index: Int): T = array(index).asInstanceOf[T]

    def update(index: Int, elem: T) {
      array(index) = elem.asInstanceOf[AnyRef]
    }

    def unbox(elemClass: Class[_]): AnyRef = array
  }

  @serializable
  final class ofByte(val array: Array[Byte]) extends WrappedArray[Byte] {

    def elemManifest = ClassManifest.Byte

    def length: Int = array.length

    def apply(index: Int): Byte = array(index)

    def update(index: Int, elem: Byte) {
      array(index) = elem
    }
    def unbox(elemClass: Class[_]): AnyRef = array
  }

  @serializable
  final class ofShort(val array: Array[Short]) extends WrappedArray[Short] {

    def elemManifest = ClassManifest.Short

    def length: Int = array.length

    def apply(index: Int): Short = array(index)

    def update(index: Int, elem: Short) {
      array(index) = elem
    }
    def unbox(elemClass: Class[_]): AnyRef = array
  }

  @serializable
  final class ofChar(val array: Array[Char]) extends WrappedArray[Char] {

    def elemManifest = ClassManifest.Char

    def length: Int = array.length

    def apply(index: Int): Char = array(index)

    def update(index: Int, elem: Char) {
      array(index) = elem
    }
    def unbox(elemClass: Class[_]): AnyRef = array
  }

  @serializable
  final class ofInt(val array: Array[Int]) extends WrappedArray[Int] {

    def elemManifest = ClassManifest.Int

    def length: Int = array.length

    def apply(index: Int): Int = array(index)

    def update(index: Int, elem: Int) {
      array(index) = elem
    }
    def unbox(elemClass: Class[_]): AnyRef = array
  }

  @serializable
  final class ofLong(val array: Array[Long]) extends WrappedArray[Long] {

    def elemManifest = ClassManifest.Long

    def length: Int = array.length

    def apply(index: Int): Long = array(index)

    def update(index: Int, elem: Long) {
      array(index) = elem
    }
    def unbox(elemClass: Class[_]): AnyRef = array
  }

  @serializable
  final class ofFloat(val array: Array[Float]) extends WrappedArray[Float] {

    def elemManifest = ClassManifest.Float

    def length: Int = array.length

    def apply(index: Int): Float = array(index)

    def update(index: Int, elem: Float) {
      array(index) = elem
    }
    def unbox(elemClass: Class[_]): AnyRef = array
  }

  @serializable
  final class ofDouble(val array: Array[Double]) extends WrappedArray[Double] {

    def elemManifest = ClassManifest.Double

    def length: Int = array.length

    def apply(index: Int): Double = array(index)

    def update(index: Int, elem: Double) {
      array(index) = elem
    }
    def unbox(elemClass: Class[_]): AnyRef = array
  }

  @serializable
  final class ofBoolean(val array: Array[Boolean]) extends WrappedArray[Boolean] {

    def elemManifest = ClassManifest.Boolean

    def length: Int = array.length

    def apply(index: Int): Boolean = array(index)

    def update(index: Int, elem: Boolean) {
      array(index) = elem
    }
    def unbox(elemClass: Class[_]): AnyRef = array
  }

  @serializable
  final class ofUnit(val array: Array[Unit]) extends WrappedArray[Unit] {

    def elemManifest = ClassManifest.Unit

    def length: Int = array.length

    def apply(index: Int): Unit = array(index)

    def update(index: Int, elem: Unit) {
      array(index) = elem
    }
    def unbox(elemClass: Class[_]): AnyRef = array
  }
}
