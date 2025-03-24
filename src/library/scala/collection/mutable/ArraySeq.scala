/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection
package mutable
import java.util.Arrays
import scala.collection.Stepper.EfficientSplit
import scala.collection.convert.impl._
import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3

/**
  *  A collection representing `Array[T]`. Unlike `ArrayBuffer` it is always backed by the same
  *  underlying `Array`, therefore it is not growable or shrinkable.
  *
  *  @tparam T    type of the elements in this wrapped array.
  *
  *  @define Coll `ArraySeq`
  *  @define coll wrapped array
  *  @define orderDependent
  *  @define orderDependentFold
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */
@SerialVersionUID(3L)
sealed abstract class ArraySeq[T]
  extends AbstractSeq[T]
    with IndexedSeq[T]
    with IndexedSeqOps[T, ArraySeq, ArraySeq[T]]
    with StrictOptimizedSeqOps[T, ArraySeq, ArraySeq[T]]
    with Serializable {

  override def iterableFactory: scala.collection.SeqFactory[ArraySeq] = ArraySeq.untagged

  override protected def fromSpecific(coll: scala.collection.IterableOnce[T]): ArraySeq[T] = {
    val b = ArrayBuilder.make(using elemTag).asInstanceOf[ArrayBuilder[T]]
    b.sizeHint(coll, delta = 0)
    b ++= coll
    ArraySeq.make(b.result())
  }
  override protected def newSpecificBuilder: Builder[T, ArraySeq[T]] = ArraySeq.newBuilder(using elemTag).asInstanceOf[Builder[T, ArraySeq[T]]]
  override def empty: ArraySeq[T] = ArraySeq.empty(using elemTag.asInstanceOf[ClassTag[T]])

  /** The tag of the element type. This does not have to be equal to the element type of this ArraySeq. A primitive
    * ArraySeq can be backed by an array of boxed values and a reference ArraySeq can be backed by an array of a supertype
    * or subtype of the element type. */
  def elemTag: ClassTag[_]

  /** Update element at given index */
  def update(@deprecatedName("idx", "2.13.0") index: Int, elem: T): Unit

  /** The underlying array. Its element type does not have to be equal to the element type of this ArraySeq. A primitive
    * ArraySeq can be backed by an array of boxed values and a reference ArraySeq can be backed by an array of a supertype
    * or subtype of the element type. */
  def array: Array[_]

  override def stepper[S <: Stepper[_]](implicit shape: StepperShape[T, S]): S with EfficientSplit

  override protected[this] def className = "ArraySeq"

  /** Clones this object, including the underlying Array. */
  override def clone(): ArraySeq[T] = ArraySeq.make(array.clone()).asInstanceOf[ArraySeq[T]]

  override def copyToArray[B >: T](xs: Array[B], start: Int, len: Int): Int = {
    val copied = IterableOnce.elemsToCopyToArray(length, xs.length, start, len)
    if(copied > 0) {
      Array.copy(array, 0, xs, start, copied)
    }
    copied
  }

  override def equals(other: Any): Boolean = other match {
    case that: ArraySeq[_] if this.array.length != that.array.length =>
      false
    case _ =>
      super.equals(other)
  }

  override def sorted[B >: T](implicit ord: Ordering[B]): ArraySeq[T] =
    ArraySeq.make(array.sorted(ord.asInstanceOf[Ordering[Any]])).asInstanceOf[ArraySeq[T]]

  override def sortInPlace[B >: T]()(implicit ord: Ordering[B]): this.type = {
    if (length > 1) scala.util.Sorting.stableSort(array.asInstanceOf[Array[B]])
    this
  }
}

/** A companion object used to create instances of `ArraySeq`.
  */
@SerialVersionUID(3L)
object ArraySeq extends StrictOptimizedClassTagSeqFactory[ArraySeq] { self =>
  val untagged: SeqFactory[ArraySeq] = new ClassTagSeqFactory.AnySeqDelegate(self)

  // This is reused for all calls to empty.
  private[this] val EmptyArraySeq  = new ofRef[AnyRef](new Array[AnyRef](0))
  def empty[T : ClassTag]: ArraySeq[T] = EmptyArraySeq.asInstanceOf[ArraySeq[T]]

  def from[A : ClassTag](it: scala.collection.IterableOnce[A]): ArraySeq[A] = make(Array.from[A](it))

  def newBuilder[A : ClassTag]: Builder[A, ArraySeq[A]] = ArrayBuilder.make[A].mapResult(make)

  /**
   * Wrap an existing `Array` into a `ArraySeq` of the proper primitive specialization type
   * without copying.
   *
   * Note that an array containing boxed primitives can be converted to a `ArraySeq` without
   * copying. For example, `val a: Array[Any] = Array(1)` is an array of `Object` at runtime,
   * containing `Integer`s. An `ArraySeq[Int]` can be obtained with a cast:
   * `ArraySeq.make(a).asInstanceOf[ArraySeq[Int]]`. The values are still
   * boxed, the resulting instance is an [[ArraySeq.ofRef]]. Writing
   * `ArraySeq.make(a.asInstanceOf[Array[Int]])` does not work, it throws a `ClassCastException`
   * at runtime.
   */
  def make[T](x: Array[T]): ArraySeq[T] = ((x: @unchecked) match {
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
  }).asInstanceOf[ArraySeq[T]]

  @SerialVersionUID(3L)
  final class ofRef[T <: AnyRef](val array: Array[T]) extends ArraySeq[T] {
    def elemTag: ClassTag[T] = ClassTag[T](array.getClass.getComponentType)
    def length: Int = array.length
    def apply(index: Int): T = array(index)
    def update(index: Int, elem: T): Unit = { array(index) = elem }
    override def hashCode = MurmurHash3.arraySeqHash(array)
    override def equals(that: Any) = that match {
      case that: ofRef[_] =>
        Array.equals(
          this.array.asInstanceOf[Array[AnyRef]],
          that.array.asInstanceOf[Array[AnyRef]])
      case _ => super.equals(that)
    }
    override def iterator: Iterator[T] = new ArrayOps.ArrayIterator[T](array)
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[T, S]): S with EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        new ObjectArrayStepper(array, 0, array.length)
      else shape.parUnbox(new ObjectArrayStepper(array, 0, array.length).asInstanceOf[AnyStepper[T] with EfficientSplit])
      ).asInstanceOf[S with EfficientSplit]
  }

  @SerialVersionUID(3L)
  final class ofByte(val array: Array[Byte]) extends ArraySeq[Byte] {
    // Type erases to `ManifestFactory.ByteManifest`, but can't annotate that because it's not accessible
    def elemTag: ClassTag.Byte.type = ClassTag.Byte
    def length: Int = array.length
    def apply(index: Int): Byte = array(index)
    def update(index: Int, elem: Byte): Unit = { array(index) = elem }
    override def hashCode = MurmurHash3.arraySeqHash(array)
    override def equals(that: Any) = that match {
      case that: ofByte => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
    override def iterator: Iterator[Byte] = new ArrayOps.ArrayIterator[Byte](array)
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Byte, S]): S with EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParIntStepper(new WidenedByteArrayStepper(array, 0, array.length))
      else new WidenedByteArrayStepper(array, 0, array.length)
      ).asInstanceOf[S with EfficientSplit]
  }

  @SerialVersionUID(3L)
  final class ofShort(val array: Array[Short]) extends ArraySeq[Short] {
    // Type erases to `ManifestFactory.ShortManifest`, but can't annotate that because it's not accessible
    def elemTag: ClassTag.Short.type = ClassTag.Short
    def length: Int = array.length
    def apply(index: Int): Short = array(index)
    def update(index: Int, elem: Short): Unit = { array(index) = elem }
    override def hashCode = MurmurHash3.arraySeqHash(array)
    override def equals(that: Any) = that match {
      case that: ofShort => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
    override def iterator: Iterator[Short] = new ArrayOps.ArrayIterator[Short](array)
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Short, S]): S with EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParIntStepper(new WidenedShortArrayStepper(array, 0, array.length))
      else new WidenedShortArrayStepper(array, 0, array.length)
      ).asInstanceOf[S with EfficientSplit]
  }

  @SerialVersionUID(3L)
  final class ofChar(val array: Array[Char]) extends ArraySeq[Char] {
    // Type erases to `ManifestFactory.CharManifest`, but can't annotate that because it's not accessible
    def elemTag: ClassTag.Char.type = ClassTag.Char
    def length: Int = array.length
    def apply(index: Int): Char = array(index)
    def update(index: Int, elem: Char): Unit = { array(index) = elem }
    override def hashCode = MurmurHash3.arraySeqHash(array)
    override def equals(that: Any) = that match {
      case that: ofChar => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
    override def iterator: Iterator[Char] = new ArrayOps.ArrayIterator[Char](array)
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Char, S]): S with EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParIntStepper(new WidenedCharArrayStepper(array, 0, array.length))
      else new WidenedCharArrayStepper(array, 0, array.length)
      ).asInstanceOf[S with EfficientSplit]

    override def addString(sb: StringBuilder, start: String, sep: String, end: String): sb.type = {
      val jsb = sb.underlying
      if (start.length != 0) jsb.append(start)
      val len = array.length
      if (len != 0) {
        if (sep.isEmpty) jsb.append(array)
        else {
          jsb.ensureCapacity(jsb.length + len + end.length + (len - 1) * sep.length)
          jsb.append(array(0))
          var i = 1
          while (i < len) {
            jsb.append(sep)
            jsb.append(array(i))
            i += 1
          }
        }
      }
      if (end.length != 0) jsb.append(end)
      sb
    }
  }

  @SerialVersionUID(3L)
  final class ofInt(val array: Array[Int]) extends ArraySeq[Int] {
    // Type erases to `ManifestFactory.IntManifest`, but can't annotate that because it's not accessible
    def elemTag: ClassTag.Int.type = ClassTag.Int
    def length: Int = array.length
    def apply(index: Int): Int = array(index)
    def update(index: Int, elem: Int): Unit = { array(index) = elem }
    override def hashCode = MurmurHash3.arraySeqHash(array)
    override def equals(that: Any) = that match {
      case that: ofInt => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
    override def iterator: Iterator[Int] = new ArrayOps.ArrayIterator[Int](array)
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Int, S]): S with EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParIntStepper(new IntArrayStepper(array, 0, array.length))
      else new IntArrayStepper(array, 0, array.length)
      ).asInstanceOf[S with EfficientSplit]
  }

  @SerialVersionUID(3L)
  final class ofLong(val array: Array[Long]) extends ArraySeq[Long] {
    // Type erases to `ManifestFactory.LongManifest`, but can't annotate that because it's not accessible
    def elemTag: ClassTag.Long.type = ClassTag.Long
    def length: Int = array.length
    def apply(index: Int): Long = array(index)
    def update(index: Int, elem: Long): Unit = { array(index) = elem }
    override def hashCode = MurmurHash3.arraySeqHash(array)
    override def equals(that: Any) = that match {
      case that: ofLong => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
    override def iterator: Iterator[Long] = new ArrayOps.ArrayIterator[Long](array)
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Long, S]): S with EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParLongStepper(new LongArrayStepper(array, 0, array.length))
      else new LongArrayStepper(array, 0, array.length)
      ).asInstanceOf[S with EfficientSplit]
  }

  @SerialVersionUID(3L)
  final class ofFloat(val array: Array[Float]) extends ArraySeq[Float] {
    // Type erases to `ManifestFactory.FloatManifest`, but can't annotate that because it's not accessible
    def elemTag: ClassTag.Float.type = ClassTag.Float
    def length: Int = array.length
    def apply(index: Int): Float = array(index)
    def update(index: Int, elem: Float): Unit = { array(index) = elem }
    override def hashCode = MurmurHash3.arraySeqHash(array)
    override def equals(that: Any) = that match {
      case that: ofFloat => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
    override def iterator: Iterator[Float] = new ArrayOps.ArrayIterator[Float](array)
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Float, S]): S with EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParDoubleStepper(new WidenedFloatArrayStepper(array, 0, array.length))
      else new WidenedFloatArrayStepper(array, 0, array.length)
      ).asInstanceOf[S with EfficientSplit]
  }

  @SerialVersionUID(3L)
  final class ofDouble(val array: Array[Double]) extends ArraySeq[Double] {
    // Type erases to `ManifestFactory.DoubleManifest`, but can't annotate that because it's not accessible
    def elemTag: ClassTag.Double.type = ClassTag.Double
    def length: Int = array.length
    def apply(index: Int): Double = array(index)
    def update(index: Int, elem: Double): Unit = { array(index) = elem }
    override def hashCode = MurmurHash3.arraySeqHash(array)
    override def equals(that: Any) = that match {
      case that: ofDouble => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
    override def iterator: Iterator[Double] = new ArrayOps.ArrayIterator[Double](array)
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Double, S]): S with EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParDoubleStepper(new DoubleArrayStepper(array, 0, array.length))
      else new DoubleArrayStepper(array, 0, array.length)
      ).asInstanceOf[S with EfficientSplit]
  }

  @SerialVersionUID(3L)
  final class ofBoolean(val array: Array[Boolean]) extends ArraySeq[Boolean] {
    // Type erases to `ManifestFactory.BooleanManifest`, but can't annotate that because it's not accessible
    def elemTag: ClassTag.Boolean.type = ClassTag.Boolean
    def length: Int = array.length
    def apply(index: Int): Boolean = array(index)
    def update(index: Int, elem: Boolean): Unit = { array(index) = elem }
    override def hashCode = MurmurHash3.arraySeqHash(array)
    override def equals(that: Any) = that match {
      case that: ofBoolean => Arrays.equals(array, that.array)
      case _ => super.equals(that)
    }
    override def iterator: Iterator[Boolean] = new ArrayOps.ArrayIterator[Boolean](array)
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Boolean, S]): S with EfficientSplit =
      new BoxedBooleanArrayStepper(array, 0, array.length).asInstanceOf[S with EfficientSplit]
  }

  @SerialVersionUID(3L)
  final class ofUnit(val array: Array[Unit]) extends ArraySeq[Unit] {
    // Type erases to `ManifestFactory.UnitManifest`, but can't annotate that because it's not accessible
    def elemTag: ClassTag.Unit.type = ClassTag.Unit
    def length: Int = array.length
    def apply(index: Int): Unit = array(index)
    def update(index: Int, elem: Unit): Unit = { array(index) = elem }
    override def hashCode = MurmurHash3.arraySeqHash(array)
    override def equals(that: Any) = that match {
      case that: ofUnit => array.length == that.array.length
      case _ => super.equals(that)
    }
    override def iterator: Iterator[Unit] = new ArrayOps.ArrayIterator[Unit](array)
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Unit, S]): S with EfficientSplit =
      new ObjectArrayStepper[AnyRef](array.asInstanceOf[Array[AnyRef]], 0, array.length).asInstanceOf[S with EfficientSplit]
  }
}
