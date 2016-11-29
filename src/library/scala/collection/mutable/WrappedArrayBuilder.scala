/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package mutable

import scala.reflect.ClassTag

/** A builder class for arrays.
 *
 *  This builder can be reused.
 *
 *  @tparam A   type of elements that can be added to this builder.
 *  @param tag  class tag for objects of type `A`.
 *
 *  @since 2.8
 */
class WrappedArrayBuilder[A](tag: ClassTag[A]) extends ReusableBuilder[A, WrappedArray[A]] {

  @deprecated("use tag instead", "2.10.0")
  val manifest: ClassTag[A] = tag

  private var elems: WrappedArray[A] = _
  private var capacity: Int = 0
  private var size: Int = 0

  private def mkArray(size: Int): WrappedArray[A] = {
    val runtimeClass = tag.runtimeClass
    val newelems = runtimeClass match {
      case java.lang.Byte.TYPE      => new WrappedArray.ofByte(new Array[Byte](size)).asInstanceOf[WrappedArray[A]]
      case java.lang.Short.TYPE     => new WrappedArray.ofShort(new Array[Short](size)).asInstanceOf[WrappedArray[A]]
      case java.lang.Character.TYPE => new WrappedArray.ofChar(new Array[Char](size)).asInstanceOf[WrappedArray[A]]
      case java.lang.Integer.TYPE   => new WrappedArray.ofInt(new Array[Int](size)).asInstanceOf[WrappedArray[A]]
      case java.lang.Long.TYPE      => new WrappedArray.ofLong(new Array[Long](size)).asInstanceOf[WrappedArray[A]]
      case java.lang.Float.TYPE     => new WrappedArray.ofFloat(new Array[Float](size)).asInstanceOf[WrappedArray[A]]
      case java.lang.Double.TYPE    => new WrappedArray.ofDouble(new Array[Double](size)).asInstanceOf[WrappedArray[A]]
      case java.lang.Boolean.TYPE   => new WrappedArray.ofBoolean(new Array[Boolean](size)).asInstanceOf[WrappedArray[A]]
      case java.lang.Void.TYPE      => new WrappedArray.ofUnit(new Array[Unit](size)).asInstanceOf[WrappedArray[A]]
      case _                        => new WrappedArray.ofRef[A with AnyRef](tag.newArray(size).asInstanceOf[Array[A with AnyRef]]).asInstanceOf[WrappedArray[A]]
    }
    if (this.size > 0) Array.copy(elems.array, 0, newelems.array, 0, this.size)
    newelems
  }

  private def resize(size: Int) {
    elems = mkArray(size)
    capacity = size
  }

  override def sizeHint(size: Int) {
    if (capacity < size) resize(size)
  }

  private def ensureSize(size: Int) {
    if (capacity < size) {
      var newsize = if (capacity == 0) 16 else capacity * 2
      while (newsize < size) newsize *= 2
      resize(newsize)
    }
  }

  def +=(elem: A): this.type = {
    ensureSize(size + 1)
    elems(size) = elem
    size += 1
    this
  }

  def clear() { size = 0 }

  def result() = {
    if (capacity != 0 && capacity == size) {
      capacity = 0
      elems
    }
    else mkArray(size)
  }

  // todo: add ++=
}
