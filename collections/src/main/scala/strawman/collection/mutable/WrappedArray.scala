package strawman.collection
package mutable

import scala.{Unit, Int, Array, Boolean, Any, Byte, Short, Char, Long, Float, Double, AnyRef, Serializable, specialized, `inline`}
import scala.Predef.{Class, implicitly}
import scala.runtime.ScalaRunTime
import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3

import java.util.Arrays

/**
  *  A collection representing `Array[T]`. Unlike `ArrayBuffer` it is always backed by the same
  *  underlying `Array`, therefore it is not growable or shrinkable.
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
    with IndexedSeqOps[T, WrappedArray, WrappedArray[T]]
    with ArrayLike[T]
    with IndexedOptimizedSeq[T]
    with StrictOptimizedSeqOps[T, WrappedArray, WrappedArray[T]]
    with Serializable {

  def iterableFactory: strawman.collection.SeqFactory[WrappedArray] = WrappedArray

  protected[this] def fromSpecificIterable(coll: strawman.collection.Iterable[T]): WrappedArray[T] = {
    val b = ArrayBuilder.make()(elemTag)
    val s = coll.knownSize
    if(s > 0) b.sizeHint(s)
    b ++= coll
    WrappedArray.make(b.result())
  }
  protected[this] def newSpecificBuilder(): Builder[T, WrappedArray[T]] = WrappedArray.newBuilder(elemTag)

  /** The tag of the element type */
  def elemTag: ClassTag[T]

  /** Update element at given index */
  def update(index: Int, elem: T): Unit

  /** The underlying array */
  def array: Array[T]

  override def toArray[U >: T : ClassTag]: Array[U] = {
    val thatElementClass = implicitly[ClassTag[U]].runtimeClass
    if (array.getClass.getComponentType eq thatElementClass)
      array.asInstanceOf[Array[U]]
    else
      super.toArray[U]
  }

  override def className = "WrappedArray"

  /** Clones this object, including the underlying Array. */
  override def clone(): WrappedArray[T] = WrappedArray.make(array.clone())
}

/** A companion object used to create instances of `WrappedArray`.
  */
object WrappedArray extends StrictOptimizedSeqFactory[WrappedArray] {
  // This is reused for all calls to empty.
  private val EmptyWrappedArray  = new ofRef[AnyRef](new Array[AnyRef](0))
  def empty[T]: WrappedArray[T] = EmptyWrappedArray.asInstanceOf[WrappedArray[T]]

  def from[A](it: strawman.collection.IterableOnce[A]): WrappedArray[A] = {
    val n = it.knownSize
    if (n > -1) {
      val elements = scala.Array.ofDim[Any](n)
      val iterator = it.iterator()
      var i = 0
      while (i < n) {
        ScalaRunTime.array_update(elements, i, iterator.next())
        i = i + 1
      }
      make(elements)
    } else fromArrayBuffer(ArrayBuffer.from(it))
  }

  @`inline` private def fromArrayBuffer[A](arr: ArrayBuffer[A]): WrappedArray[A] =
    make[A](arr.asInstanceOf[ArrayBuffer[Any]].toArray)

  def newBuilder[A](): Builder[A, WrappedArray[A]] =
    ArrayBuffer.newBuilder[A]().mapResult(fromArrayBuffer)

  def newBuilder[A](implicit t: ClassTag[A]): Builder[A, WrappedArray[A]] = ArrayBuilder.make[A]()(t).mapResult(make)

  // If make is called explicitly we use whatever we're given, even if it's
  // empty.  This may be unnecessary (if WrappedArray is to honor the collections
  // contract all empty ones must be equal, so discriminating based on the reference
  // equality of an empty array should not come up) but we may as well be
  // conservative since wrapRefArray contributes most of the unnecessary allocations.
  def make[T](x: Array[_]): WrappedArray[T] = (x match {
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

  final class ofRef[T <: AnyRef](val array: Array[T]) extends WrappedArray[T] with Serializable {
    lazy val elemTag = ClassTag[T](array.getClass.getComponentType)
    def length: Int = array.length
    def apply(index: Int): T = array(index).asInstanceOf[T]
    def update(index: Int, elem: T): Unit = { array(index) = elem }
    override def hashCode = wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofRef[_] => Arrays.equals(array.asInstanceOf[Array[AnyRef]], that.array.asInstanceOf[Array[AnyRef]])
      case _ => super.equals(that)
    }
  }

  final class ofByte(val array: Array[Byte]) extends WrappedArray[Byte] with Serializable {
    def elemTag = ClassTag.Byte
    def length: Int = array.length
    def apply(index: Int): Byte = array(index)
    def update(index: Int, elem: Byte): Unit = { array(index) = elem }
    override def hashCode = wrappedBytesHash(array)
    override def equals(that: Any) = that match {
      case that: ofByte => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
  }

  final class ofShort(val array: Array[Short]) extends WrappedArray[Short] with Serializable {
    def elemTag = ClassTag.Short
    def length: Int = array.length
    def apply(index: Int): Short = array(index)
    def update(index: Int, elem: Short): Unit = { array(index) = elem }
    override def hashCode = wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofShort => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
  }

  final class ofChar(val array: Array[Char]) extends WrappedArray[Char] with Serializable {
    def elemTag = ClassTag.Char
    def length: Int = array.length
    def apply(index: Int): Char = array(index)
    def update(index: Int, elem: Char): Unit = { array(index) = elem }
    override def hashCode = wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofChar => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
  }

  final class ofInt(val array: Array[Int]) extends WrappedArray[Int] with Serializable {
    def elemTag = ClassTag.Int
    def length: Int = array.length
    def apply(index: Int): Int = array(index)
    def update(index: Int, elem: Int): Unit = { array(index) = elem }
    override def hashCode = wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofInt => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
  }

  final class ofLong(val array: Array[Long]) extends WrappedArray[Long] with Serializable {
    def elemTag = ClassTag.Long
    def length: Int = array.length
    def apply(index: Int): Long = array(index)
    def update(index: Int, elem: Long): Unit = { array(index) = elem }
    override def hashCode = wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofLong => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
  }

  final class ofFloat(val array: Array[Float]) extends WrappedArray[Float] with Serializable {
    def elemTag = ClassTag.Float
    def length: Int = array.length
    def apply(index: Int): Float = array(index)
    def update(index: Int, elem: Float): Unit = { array(index) = elem }
    override def hashCode = wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofFloat => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
  }

  final class ofDouble(val array: Array[Double]) extends WrappedArray[Double] with Serializable {
    def elemTag = ClassTag.Double
    def length: Int = array.length
    def apply(index: Int): Double = array(index)
    def update(index: Int, elem: Double): Unit = { array(index) = elem }
    override def hashCode = wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofDouble => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
  }

  final class ofBoolean(val array: Array[Boolean]) extends WrappedArray[Boolean] with Serializable {
    def elemTag = ClassTag.Boolean
    def length: Int = array.length
    def apply(index: Int): Boolean = array(index)
    def update(index: Int, elem: Boolean): Unit = { array(index) = elem }
    override def hashCode = wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofBoolean => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
  }

  final class ofUnit(val array: Array[Unit]) extends WrappedArray[Unit] with Serializable {
    def elemTag = ClassTag.Unit
    def length: Int = array.length
    def apply(index: Int): Unit = array(index)
    def update(index: Int, elem: Unit): Unit = { array(index) = elem }
    override def hashCode = wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofUnit => array.length == that.array.length
      case _ => super.equals(that)
    }
  }

  //TODO Use MurmurHash3.wrappedArrayHash/wrappedBytesHash which are private[scala]
  private def wrappedArrayHash[@specialized T](a: Array[T]): Int  = MurmurHash3.arrayHash(a, MurmurHash3.seqSeed)
  private def wrappedBytesHash(data: Array[Byte]): Int            = MurmurHash3.bytesHash(data, MurmurHash3.seqSeed)
}
