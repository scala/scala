package scala.collection
package mutable

import scala.reflect.ClassTag
import scala.collection.immutable.ImmutableArray

/** A builder class for arrays.
 *
 *  @since 2.8
 *
 *  @tparam T    the type of the elements for the builder.
 */
@SerialVersionUID(3L)
sealed abstract class ArrayBuilder[T]
  extends ReusableBuilder[T, Array[T]]
    with Serializable {
  protected[this] var capacity: Int = 0
  protected[this] def elems: Array[T]
  protected var size: Int = 0

  def length: Int = size

  protected[this] final def ensureSize(size: Int): Unit = {
    if (capacity < size || capacity == 0) {
      var newsize = if (capacity == 0) 16 else capacity * 2
      while (newsize < size) newsize *= 2
      resize(newsize)
    }
  }

  override final def sizeHint(size: Int): Unit =
    if (capacity < size) resize(size)

  final def clear(): Unit = size = 0

  protected[this] def resize(size: Int): Unit

  /** Add all elements of an array */
  def addAll(xs: Array[_ <: T]): this.type = addAll(xs, 0, xs.length)

  /** Add a slice of an array */
  def addAll(xs: Array[_ <: T], offset: Int, length: Int): this.type = {
    ensureSize(this.size + length)
    Array.copy(xs, offset, elems, this.size, length)
    size += length
    this
  }

  override def addAll(xs: IterableOnce[T]): this.type = {
    val k = xs.knownSize
    if(k > 0) {
      ensureSize(this.size + k)
      xs match {
        case xs: Iterable[T] => xs.copyToArray(elems, this.size)
        case _ => xs.iterator().copyToArray(elems, this.size)
      }
      size += k
    } else if(k < 0) super.addAll(xs)
    this
  }
}

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
    tag.runtimeClass match {
      case java.lang.Byte.TYPE      => new ArrayBuilder.ofByte().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Short.TYPE     => new ArrayBuilder.ofShort().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Character.TYPE => new ArrayBuilder.ofChar().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Integer.TYPE   => new ArrayBuilder.ofInt().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Long.TYPE      => new ArrayBuilder.ofLong().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Float.TYPE     => new ArrayBuilder.ofFloat().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Double.TYPE    => new ArrayBuilder.ofDouble().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Boolean.TYPE   => new ArrayBuilder.ofBoolean().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Void.TYPE      => new ArrayBuilder.ofUnit().asInstanceOf[ArrayBuilder[T]]
      case _                        => new ArrayBuilder.ofRef[T with AnyRef]()(tag.asInstanceOf[ClassTag[T with AnyRef]]).asInstanceOf[ArrayBuilder[T]]
    }
  }

  /** A class for array builders for arrays of reference types.
   *
   *  This builder can be reused.
   *
   *  @tparam T     type of elements for the array builder, subtype of `AnyRef` with a `ClassTag` context bound.
   */
  @SerialVersionUID(3L)
  final class ofRef[T <: AnyRef](implicit ct: ClassTag[T]) extends ArrayBuilder[T] {

    protected var elems: Array[T] = _

    private def mkArray(size: Int): Array[T] = {
      val newelems = new Array[T](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
      newelems
    }

    protected[this] def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    def addOne(elem: T): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

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
  @SerialVersionUID(3L)
  final class ofByte extends ArrayBuilder[Byte] {

    protected var elems: Array[Byte] = _

    private def mkArray(size: Int): Array[Byte] = {
      val newelems = new Array[Byte](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
      newelems
    }

    protected[this] def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    def addOne(elem: Byte): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

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
  @SerialVersionUID(3L)
  final class ofShort extends ArrayBuilder[Short] {

    protected var elems: Array[Short] = _

    private def mkArray(size: Int): Array[Short] = {
      val newelems = new Array[Short](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
      newelems
    }

    protected[this] def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    def addOne(elem: Short): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

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
  @SerialVersionUID(3L)
  final class ofChar extends ArrayBuilder[Char] {

    protected var elems: Array[Char] = _

    private def mkArray(size: Int): Array[Char] = {
      val newelems = new Array[Char](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
      newelems
    }

    protected[this] def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    def addOne(elem: Char): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

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
  @SerialVersionUID(3L)
  final class ofInt extends ArrayBuilder[Int] {

    protected var elems: Array[Int] = _

    private def mkArray(size: Int): Array[Int] = {
      val newelems = new Array[Int](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
      newelems
    }

    protected[this] def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    def addOne(elem: Int): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

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
  @SerialVersionUID(3L)
  final class ofLong extends ArrayBuilder[Long] {

    protected var elems: Array[Long] = _

    private def mkArray(size: Int): Array[Long] = {
      val newelems = new Array[Long](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
      newelems
    }

    protected[this] def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    def addOne(elem: Long): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

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
  @SerialVersionUID(3L)
  final class ofFloat extends ArrayBuilder[Float] {

    protected var elems: Array[Float] = _

    private def mkArray(size: Int): Array[Float] = {
      val newelems = new Array[Float](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
      newelems
    }

    protected[this] def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    def addOne(elem: Float): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

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
  @SerialVersionUID(3L)
  final class ofDouble extends ArrayBuilder[Double] {

    protected var elems: Array[Double] = _

    private def mkArray(size: Int): Array[Double] = {
      val newelems = new Array[Double](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
      newelems
    }

    protected[this] def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    def addOne(elem: Double): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

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
  @SerialVersionUID(3L)
  class ofBoolean extends ArrayBuilder[Boolean] {

    protected var elems: Array[Boolean] = _

    private def mkArray(size: Int): Array[Boolean] = {
      val newelems = new Array[Boolean](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
      newelems
    }

    protected[this] def resize(size: Int): Unit = {
      elems = mkArray(size)
      capacity = size
    }

    def addOne(elem: Boolean): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

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
  @SerialVersionUID(3L)
  final class ofUnit extends ArrayBuilder[Unit] {

    protected def elems: Array[Unit] = throw new UnsupportedOperationException()

    def addOne(elem: Unit): this.type = {
      size += 1
      this
    }

    override def addAll(xs: IterableOnce[Unit]): this.type = {
      size += xs.iterator().size
      this
    }

    override def addAll(xs: Array[_ <: Unit], offset: Int, length: Int): this.type = {
      size += length
      this
    }

    def result() = {
      val ans = new Array[Unit](size)
      var i = 0
      while (i < size) { ans(i) = (); i += 1 }
      ans
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofUnit => (size == x.size)
      case _ => false
    }

    protected[this] def resize(size: Int): Unit = ()

    override def toString = "ArrayBuilder.ofUnit"
  }
}
