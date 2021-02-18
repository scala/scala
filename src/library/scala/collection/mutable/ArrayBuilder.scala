/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package mutable

import scala.reflect.ClassTag

/** A builder class for arrays.
 *
 *  @since 2.8
 *
 *  @tparam T    the type of the elements for the builder.
 */
@SerialVersionUID(-4721309866680431208L)
abstract class ArrayBuilder[T] extends ReusableBuilder[T, Array[T]] with Serializable

/** A companion object for array builders.
 *
 *  @since 2.8
 */
object ArrayBuilder {

  /** Creates a new arraybuilder of type `T`.
   *
   *  @tparam T     type of the elements for the array builder, with a `ClassTag` context bound.
   *  @return       a new empty array builder.
   */
  def make[T: ClassTag](): ArrayBuilder[T] = {
    val tag = implicitly[ClassTag[T]]
    val cls = tag.runtimeClass
    if (cls.isPrimitive) {
      cls match {
        case java.lang.Integer.TYPE   => new ArrayBuilder.ofInt().asInstanceOf[ArrayBuilder[T]]
        case java.lang.Double.TYPE    => new ArrayBuilder.ofDouble().asInstanceOf[ArrayBuilder[T]]
        case java.lang.Long.TYPE      => new ArrayBuilder.ofLong().asInstanceOf[ArrayBuilder[T]]
        case java.lang.Float.TYPE     => new ArrayBuilder.ofFloat().asInstanceOf[ArrayBuilder[T]]
        case java.lang.Character.TYPE => new ArrayBuilder.ofChar().asInstanceOf[ArrayBuilder[T]]
        case java.lang.Byte.TYPE      => new ArrayBuilder.ofByte().asInstanceOf[ArrayBuilder[T]]
        case java.lang.Short.TYPE     => new ArrayBuilder.ofShort().asInstanceOf[ArrayBuilder[T]]
        case java.lang.Boolean.TYPE   => new ArrayBuilder.ofBoolean().asInstanceOf[ArrayBuilder[T]]
        case java.lang.Void.TYPE      => new ArrayBuilder.ofUnit().asInstanceOf[ArrayBuilder[T]]
      }
    } else {
      new ArrayBuilder.ofRef[T with AnyRef]()(tag.asInstanceOf[ClassTag[T with AnyRef]]).asInstanceOf[ArrayBuilder[T]]
    }
  }

  /** A class for array builders for arrays of reference types.
   *
   *  This builder can be reused.
   *
   *  @tparam T     type of elements for the array builder, subtype of `AnyRef` with a `ClassTag` context bound.
   */
  @SerialVersionUID(-8376727444766075941L)
  final class ofRef[T <: AnyRef : ClassTag] extends ArrayBuilder[T] {

    private var elems: Array[T] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[T] = {
      if (capacity == size && capacity > 0) elems
      else if (elems eq null) new Array[T](size)
      else java.util.Arrays.copyOf[T](elems, size)
    }

    private def resize(size: Int) {
      elems = mkArray(size)
      capacity = size
    }

    override def sizeHint(size: Int) {
      if (capacity < size) resize(size)
    }

    private def ensureSize(size: Int) {
      if (capacity < size || capacity == 0) {
        var newsize = if (capacity == 0) 16 else capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: T): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: TraversableOnce[T]): this.type = (xs.asInstanceOf[AnyRef]) match {
      case xs: WrappedArray.ofRef[_] =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() { size = 0 }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        elems
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofRef[_] => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofRef"
  }

  /** A class for array builders for arrays of `byte`s. It can be reused. */
  @SerialVersionUID(-3484148043254823366L)
  final class ofByte extends ArrayBuilder[Byte] {

    private var elems: Array[Byte] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Byte] = {
      if (size == 0) Array.emptyByteArray
      else if (elems eq null) new Array(size)
      else java.util.Arrays.copyOf(elems, size)
    }

    private def resize(size: Int) {
      elems = mkArray(size)
      capacity = size
    }

    override def sizeHint(size: Int) {
      if (capacity < size) resize(size)
    }

    private def ensureSize(size: Int) {
      if (capacity < size || capacity == 0) {
        var newsize = if (capacity == 0) 16 else capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Byte): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: TraversableOnce[Byte]): this.type = xs match {
      case xs: WrappedArray.ofByte =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() { size = 0 }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        elems
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofByte => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofByte"
  }

  /** A class for array builders for arrays of `short`s. It can be reused. */
  @SerialVersionUID(3295904306819377609L)
  final class ofShort extends ArrayBuilder[Short] {

    private var elems: Array[Short] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Short] = {
      if (size == 0) Array.emptyShortArray
      else if (elems eq null) new Array(size)
      else java.util.Arrays.copyOf(elems, size)
    }

    private def resize(size: Int) {
      elems = mkArray(size)
      capacity = size
    }

    override def sizeHint(size: Int) {
      if (capacity < size) resize(size)
    }

    private def ensureSize(size: Int) {
      if (capacity < size || capacity == 0) {
        var newsize = if (capacity == 0) 16 else capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Short): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: TraversableOnce[Short]): this.type = xs match {
      case xs: WrappedArray.ofShort =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() { size = 0 }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        elems
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofShort => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofShort"
  }

  /** A class for array builders for arrays of `char`s. It can be reused. */
  @SerialVersionUID(-8284807600792805165L)
  final class ofChar extends ArrayBuilder[Char] {

    private var elems: Array[Char] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Char] = {
      if (size == 0) Array.emptyCharArray
      else if (elems eq null) new Array(size)
      else java.util.Arrays.copyOf(elems, size)
    }

    private def resize(size: Int) {
      elems = mkArray(size)
      capacity = size
    }

    override def sizeHint(size: Int) {
      if (capacity < size) resize(size)
    }

    private def ensureSize(size: Int) {
      if (capacity < size || capacity == 0) {
        var newsize = if (capacity == 0) 16 else capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Char): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: TraversableOnce[Char]): this.type = xs match {
      case xs: WrappedArray.ofChar =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() { size = 0 }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        elems
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofChar => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofChar"
  }

  /** A class for array builders for arrays of `int`s. It can be reused. */
  @SerialVersionUID(-3033902589330485711L)
  final class ofInt extends ArrayBuilder[Int] {

    private var elems: Array[Int] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Int] = {
      if (size == 0) Array.emptyIntArray
      else if (elems eq null) new Array(size)
      else java.util.Arrays.copyOf(elems, size)
    }

    private def resize(size: Int) {
      elems = mkArray(size)
      capacity = size
    }

    override def sizeHint(size: Int) {
      if (capacity < size) resize(size)
    }

    private def ensureSize(size: Int) {
      if (capacity < size || capacity == 0) {
        var newsize = if (capacity == 0) 16 else capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Int): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: TraversableOnce[Int]): this.type = xs match {
      case xs: WrappedArray.ofInt =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() { size = 0 }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        elems
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofInt => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofInt"
  }

  /** A class for array builders for arrays of `long`s. It can be reused. */
  @SerialVersionUID(-4278005356053656861L)
  final class ofLong extends ArrayBuilder[Long] {

    private var elems: Array[Long] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Long] = {
      if (size == 0) Array.emptyLongArray
      else if (elems eq null) new Array(size)
      else java.util.Arrays.copyOf(elems, size)
    }

    private def resize(size: Int) {
      elems = mkArray(size)
      capacity = size
    }

    override def sizeHint(size: Int) {
      if (capacity < size) resize(size)
    }

    private def ensureSize(size: Int) {
      if (capacity < size || capacity == 0) {
        var newsize = if (capacity == 0) 16 else capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Long): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: TraversableOnce[Long]): this.type = xs match {
      case xs: WrappedArray.ofLong =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() { size = 0 }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        elems
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofLong => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofLong"
  }

  /** A class for array builders for arrays of `float`s. It can be reused. */
  @SerialVersionUID(-740775369715282824L)
  final class ofFloat extends ArrayBuilder[Float] {

    private var elems: Array[Float] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Float] = {
      if (size == 0) Array.emptyFloatArray
      else if (elems eq null) new Array(size)
      else java.util.Arrays.copyOf(elems, size)
    }

    private def resize(size: Int) {
      elems = mkArray(size)
      capacity = size
    }

    override def sizeHint(size: Int) {
      if (capacity < size) resize(size)
    }

    private def ensureSize(size: Int) {
      if (capacity < size || capacity == 0) {
        var newsize = if (capacity == 0) 16 else capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Float): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: TraversableOnce[Float]): this.type = xs match {
      case xs: WrappedArray.ofFloat =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() { size = 0 }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        elems
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofFloat => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofFloat"
  }

  /** A class for array builders for arrays of `double`s. It can be reused. */
  @SerialVersionUID(2549152794429074790L)
  final class ofDouble extends ArrayBuilder[Double] {

    private var elems: Array[Double] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Double] = {
      if (size == 0) Array.emptyDoubleArray
      else if (elems eq null) new Array(size)
      else java.util.Arrays.copyOf(elems, size)
    }

    private def resize(size: Int) {
      elems = mkArray(size)
      capacity = size
    }

    override def sizeHint(size: Int) {
      if (capacity < size) resize(size)
    }

    private def ensureSize(size: Int) {
      if (capacity < size || capacity == 0) {
        var newsize = if (capacity == 0) 16 else capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Double): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: TraversableOnce[Double]): this.type = xs match {
      case xs: WrappedArray.ofDouble =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() { size = 0 }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        elems
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofDouble => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofDouble"
  }

  /** A class for array builders for arrays of `boolean`s. It can be reused. */
  @SerialVersionUID(-3574834070591819420L)
  class ofBoolean extends ArrayBuilder[Boolean] {

    private var elems: Array[Boolean] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Boolean] = {
      if (size == 0) Array.emptyBooleanArray
      else if (elems eq null) new Array(size)
      else java.util.Arrays.copyOf(elems, size)
    }

    private def resize(size: Int) {
      elems = mkArray(size)
      capacity = size
    }

    override def sizeHint(size: Int) {
      if (capacity < size) resize(size)
    }

    private def ensureSize(size: Int) {
      if (capacity < size || capacity == 0) {
        var newsize = if (capacity == 0) 16 else capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Boolean): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: TraversableOnce[Boolean]): this.type = xs match {
      case xs: WrappedArray.ofBoolean =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() { size = 0 }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        elems
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofBoolean => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofBoolean"
  }

  /** A class for array builders for arrays of `Unit` type. It can be reused. */
  @SerialVersionUID(1995804197797796249L)
  final class ofUnit extends ArrayBuilder[Unit] {

    private var size: Int = 0

    def +=(elem: Unit): this.type = {
      size += 1
      this
    }

    override def ++=(xs: TraversableOnce[Unit]): this.type = {
      size += xs.size
      this
    }

    def clear() { size = 0 }

    def result() = {
      if (size == 0) Array.emptyUnitArray
      else {
        val ans = new Array[Unit](size)
        java.util.Arrays.fill(ans.asInstanceOf[Array[AnyRef]], ())
        ans
      }
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofUnit => (size == x.size)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofUnit"
  }
}
