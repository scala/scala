/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package mutable

import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime._
import scala.collection.generic._
import scala.collection.parallel.mutable.ParArray

/**
 *  A class representing `Array[T]`.
 *
 *  @tparam T    type of the elements in this wrapped array.
 *
 *  @author  Martin Odersky, Stephane Micheloud
 *  @version 1.0
 *  @since 2.8
 *  @define Coll `WrappedArray`
 *  @define coll wrapped array
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
abstract class WrappedArray[T]
extends AbstractSeq[T]
    with IndexedSeq[T]
    with ArrayLike[T, WrappedArray[T]]
    with CustomParallelizable[T, ParArray[T]]
{

  override protected[this] def thisCollection: WrappedArray[T] = this
  override protected[this] def toCollection(repr: WrappedArray[T]): WrappedArray[T] = repr

  /** The tag of the element type */
  def elemTag: ClassTag[T]

  @deprecated("use elemTag instead", "2.10.0")
  def elemManifest: ClassManifest[T] = ClassManifest.fromClass[T](arrayElementClass(elemTag).asInstanceOf[Class[T]])

  /** The length of the array */
  def length: Int

  /** The element at given index */
  def apply(index: Int): T

  /** Update element at given index */
  def update(index: Int, elem: T): Unit

  /** The underlying array */
  def array: Array[T]

  override def par = ParArray.handoff(array)

  private def elementClass: Class[_] =
    arrayElementClass(array.getClass)

  override def toArray[U >: T : ClassTag]: Array[U] = {
    val thatElementClass = arrayElementClass(implicitly[ClassTag[U]])
    if (elementClass eq thatElementClass)
      array.asInstanceOf[Array[U]]
    else
      super.toArray[U]
  }

  override def stringPrefix = "WrappedArray"

  /** Clones this object, including the underlying Array. */
  override def clone(): WrappedArray[T] = WrappedArray make array.clone()

  /** Creates new builder for this collection ==> move to subclasses
   */
  override protected[this] def newBuilder: Builder[T, WrappedArray[T]] =
    new WrappedArrayBuilder[T](elemTag)
}

private[mutable] abstract class WrappedArrayImpl[T] extends WrappedArray[T] {
  override def slice(from: Int, until: Int): WrappedArray[T] = {
    val start = if (from < 0) 0 else from
    if (until <= start || start >= repr.length)
      return emptyImpl
    val end = if (until > length) length else until
    sliceImpl(start, end)
  }

  protected def emptyImpl: WrappedArray[T]

  protected def sliceImpl(from: Int, until: Int): WrappedArray[T]
}

/** A companion object used to create instances of `WrappedArray`.
 */
object WrappedArray {
  import java.util
  // This is reused for all calls to empty.
  private val EmptyWrappedArray  = new ofRef[AnyRef](new Array[AnyRef](0))
  def empty[T <: AnyRef]: WrappedArray[T] = EmptyWrappedArray.asInstanceOf[WrappedArray[T]]

  // If make is called explicitly we use whatever we're given, even if it's
  // empty.  This may be unnecessary (if WrappedArray is to honor the collections
  // contract all empty ones must be equal, so discriminating based on the reference
  // equality of an empty array should not come up) but we may as well be
  // conservative since wrapRefArray contributes most of the unnecessary allocations.
  def make[T](x: AnyRef): WrappedArray[T] = (x match {
    case null              => null
    case x: Array[AnyRef]  => new ofRef[AnyRef](x)
    case x: Array[Int]     => new ofInt(x)
    case x: Array[Double]  => new ofDouble(x)
    case x: Array[Long]    => new ofLong(x)
    case x: Array[Float]   => new ofFloat(x)
    case x: Array[Char]    => new ofChar(x)
    case x: Array[Byte]    => new ofByte(x)
    case x: Array[Short]   => new ofShort(x)
    case x: Array[Boolean] => new ofBoolean(x)
    case x: Array[Unit]    => new ofUnit(x)
  }).asInstanceOf[WrappedArray[T]]

  implicit def canBuildFrom[T](implicit m: ClassTag[T]): CanBuildFrom[WrappedArray[_], T, WrappedArray[T]] =
    new CanBuildFrom[WrappedArray[_], T, WrappedArray[T]] {
      def apply(from: WrappedArray[_]): Builder[T, WrappedArray[T]] =
        ArrayBuilder.make[T]()(m) mapResult WrappedArray.make[T]
      def apply: Builder[T, WrappedArray[T]] =
        ArrayBuilder.make[T]()(m) mapResult WrappedArray.make[T]
  }

  def newBuilder[A]: Builder[A, IndexedSeq[A]] = new ArrayBuffer

  private val emptyWrappedByte = new ofByte(new Array[Byte](0))
  private val emptyWrappedShort = new ofShort(new Array[Short](0))
  private val emptyWrappedInt = new ofInt(new Array[Int](0))
  private val emptyWrappedLong = new ofLong(new Array[Long](0))
  private val emptyWrappedFloat = new ofFloat(new Array[Float](0))
  private val emptyWrappedDouble = new ofDouble(new Array[Double](0))
  private val emptyWrappedUnit = new ofUnit(new Array[Unit](0))
  private val emptyWrappedChar = new ofChar(new Array[Char](0))
  private val emptyWrappedBoolean = new ofBoolean(new Array[Boolean](0))

  final class ofRef[T <: AnyRef](val array: Array[T]) extends WrappedArrayImpl[T] with Serializable {
    lazy val elemTag = ClassTag[T](arrayElementClass(array.getClass))
    def length: Int = array.length
    def apply(index: Int): T = array(index).asInstanceOf[T]
    def update(index: Int, elem: T) { array(index) = elem }
    protected override def emptyImpl = new ofRef(util.Arrays.copyOf[T](array,0))
    protected override def sliceImpl(from: Int, until: Int) = new ofRef[T](util.Arrays.copyOfRange[T](array, from, until))
  }

  final class ofByte(val array: Array[Byte]) extends WrappedArrayImpl[Byte] with Serializable {
    def elemTag = ClassTag.Byte
    def length: Int = array.length
    def apply(index: Int): Byte = array(index)
    def update(index: Int, elem: Byte) { array(index) = elem }
    protected override def emptyImpl = emptyWrappedByte
    protected override def sliceImpl(from: Int, until: Int) = new ofByte(util.Arrays.copyOfRange(array, from, until))
  }

  final class ofShort(val array: Array[Short]) extends WrappedArrayImpl[Short] with Serializable {
    def elemTag = ClassTag.Short
    def length: Int = array.length
    def apply(index: Int): Short = array(index)
    def update(index: Int, elem: Short) { array(index) = elem }
    protected override def emptyImpl = emptyWrappedShort
    protected override def sliceImpl(from: Int, until: Int) = new ofShort(util.Arrays.copyOfRange(array, from, until))
  }

  final class ofChar(val array: Array[Char]) extends WrappedArrayImpl[Char] with Serializable {
    def elemTag = ClassTag.Char
    def length: Int = array.length
    def apply(index: Int): Char = array(index)
    def update(index: Int, elem: Char) { array(index) = elem }
    protected override def emptyImpl = emptyWrappedChar
    protected override def sliceImpl(from: Int, until: Int) = new ofChar(util.Arrays.copyOfRange(array, from, until))
  }

  final class ofInt(val array: Array[Int]) extends WrappedArrayImpl[Int] with Serializable {
    def elemTag = ClassTag.Int
    def length: Int = array.length
    def apply(index: Int): Int = array(index)
    def update(index: Int, elem: Int) { array(index) = elem }
    protected override def emptyImpl = emptyWrappedInt
    protected override def sliceImpl(from: Int, until: Int) = new ofInt(util.Arrays.copyOfRange(array, from, until))
  }

  final class ofLong(val array: Array[Long]) extends WrappedArrayImpl[Long] with Serializable {
    def elemTag = ClassTag.Long
    def length: Int = array.length
    def apply(index: Int): Long = array(index)
    def update(index: Int, elem: Long) { array(index) = elem }
    protected override def emptyImpl = emptyWrappedLong
    protected override def sliceImpl(from: Int, until: Int) = new ofLong(util.Arrays.copyOfRange(array, from, until))
  }

  final class ofFloat(val array: Array[Float]) extends WrappedArrayImpl[Float] with Serializable {
    def elemTag = ClassTag.Float
    def length: Int = array.length
    def apply(index: Int): Float = array(index)
    def update(index: Int, elem: Float) { array(index) = elem }
    protected override def emptyImpl = emptyWrappedFloat
    protected override def sliceImpl(from: Int, until: Int) = new ofFloat(util.Arrays.copyOfRange(array, from, until))
  }

  final class ofDouble(val array: Array[Double]) extends WrappedArrayImpl[Double] with Serializable {
    def elemTag = ClassTag.Double
    def length: Int = array.length
    def apply(index: Int): Double = array(index)
    def update(index: Int, elem: Double) { array(index) = elem }
    protected override def emptyImpl = emptyWrappedDouble
    protected override def sliceImpl(from: Int, until: Int) = new ofDouble(util.Arrays.copyOfRange(array, from, until))
  }

  final class ofBoolean(val array: Array[Boolean]) extends WrappedArrayImpl[Boolean] with Serializable {
    def elemTag = ClassTag.Boolean
    def length: Int = array.length
    def apply(index: Int): Boolean = array(index)
    def update(index: Int, elem: Boolean) { array(index) = elem }
    protected override def emptyImpl = emptyWrappedBoolean
    protected override def sliceImpl(from: Int, until: Int) = new ofBoolean(util.Arrays.copyOfRange(array, from, until))
  }

  final class ofUnit(val array: Array[Unit]) extends WrappedArrayImpl[Unit] with Serializable {
    def elemTag = ClassTag.Unit
    def length: Int = array.length
    def apply(index: Int): Unit = array(index)
    def update(index: Int, elem: Unit) { array(index) = elem }
    protected override def emptyImpl = emptyWrappedUnit
    protected override def sliceImpl(from: Int, until: Int) = {
      // cant use
      // new ofUnit(util.Arrays.copyOfRange[Unit](array, from, until)) - Unit is special and doesnt compile
      // cant use util.Arrays.copyOfRange[Unit](repr, from, until) - Unit is special and doesnt compile
      val res = new Array[Unit](until-from)
      System.arraycopy(repr, from, res, 0, until-from)
      new ofUnit(res)
    }
  }
}
