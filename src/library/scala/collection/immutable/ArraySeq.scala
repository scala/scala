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

package scala.collection
package immutable

import java.util.Arrays

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.Stepper.EfficientSplit
import scala.collection.mutable.{ArrayBuffer, ArrayBuilder, Builder, ArraySeq => MutableArraySeq}
import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime
import scala.util.Sorting
import scala.util.hashing.MurmurHash3

/**
  * An immutable array.
  *
  * Supports efficient indexed access and has a small memory footprint.
  *
  * @define coll immutable array
  * @define Coll `ArraySeq`
  */
sealed abstract class ArraySeq[+A]
  extends AbstractSeq[A]
    with IndexedSeq[A]
    with IndexedSeqOps[A, ArraySeq, ArraySeq[A]]
    with StrictOptimizedSeqOps[A, ArraySeq, ArraySeq[A]]
    with EvidenceIterableFactoryDefaults[A, ArraySeq, ClassTag]
    with Serializable {

  /** The tag of the element type. This does not have to be equal to the element type of this ArraySeq. A primitive
    * ArraySeq can be backed by an array of boxed values and a reference ArraySeq can be backed by an array of a supertype
    * or subtype of the element type. */
  protected def elemTag: ClassTag[_]

  override def iterableFactory: SeqFactory[ArraySeq] = ArraySeq.untagged

  /** The wrapped mutable `Array` that backs this `ArraySeq`. Any changes to this array will break
    * the expected immutability. Its element type does not have to be equal to the element type of this ArraySeq.
    * A primitive ArraySeq can be backed by an array of boxed values and a reference ArraySeq can be backed by an
    * array of a supertype or subtype of the element type. */
  def unsafeArray: Array[_]

  protected def evidenceIterableFactory: ArraySeq.type = ArraySeq
  protected def iterableEvidence: ClassTag[A @uncheckedVariance] = elemTag.asInstanceOf[ClassTag[A]]

  override def stepper[S <: Stepper[_]](implicit shape: StepperShape[A, S]): S with EfficientSplit = {
    import scala.collection.convert.impl._
    val isRefShape = shape.shape == StepperShape.ReferenceShape
    val s = if (isRefShape) unsafeArray match {
      case a: Array[Int]     => AnyStepper.ofParIntStepper   (new IntArrayStepper(a, 0, a.length))
      case a: Array[Long]    => AnyStepper.ofParLongStepper  (new LongArrayStepper(a, 0, a.length))
      case a: Array[Double]  => AnyStepper.ofParDoubleStepper(new DoubleArrayStepper(a, 0, a.length))
      case a: Array[Byte]    => AnyStepper.ofParIntStepper   (new WidenedByteArrayStepper(a, 0, a.length))
      case a: Array[Short]   => AnyStepper.ofParIntStepper   (new WidenedShortArrayStepper(a, 0, a.length))
      case a: Array[Char]    => AnyStepper.ofParIntStepper   (new WidenedCharArrayStepper(a, 0, a.length))
      case a: Array[Float]   => AnyStepper.ofParDoubleStepper(new WidenedFloatArrayStepper(a, 0, a.length))
      case a: Array[Boolean] => new BoxedBooleanArrayStepper(a, 0, a.length)
      case a: Array[AnyRef]  => new ObjectArrayStepper(a, 0, a.length)
    } else {
      unsafeArray match {
        case a: Array[AnyRef] => shape.parUnbox(new ObjectArrayStepper(a, 0, a.length).asInstanceOf[AnyStepper[A] with EfficientSplit])
        case a: Array[Int]    => new IntArrayStepper(a, 0, a.length)
        case a: Array[Long]   => new LongArrayStepper(a, 0, a.length)
        case a: Array[Double] => new DoubleArrayStepper(a, 0, a.length)
        case a: Array[Byte]   => new WidenedByteArrayStepper(a, 0, a.length)
        case a: Array[Short]  => new WidenedShortArrayStepper(a, 0, a.length)
        case a: Array[Char]   => new WidenedCharArrayStepper(a, 0, a.length)
        case a: Array[Float]  => new WidenedFloatArrayStepper(a, 0, a.length)
      }
    }
    s.asInstanceOf[S with EfficientSplit]
  }

  @throws[ArrayIndexOutOfBoundsException]
  def apply(i: Int): A

  override def updated[B >: A](index: Int, elem: B): ArraySeq[B] = {
    val dest = new Array[Any](length)
    Array.copy(unsafeArray, 0, dest, 0, length)
    dest(index) = elem
    ArraySeq.unsafeWrapArray(dest).asInstanceOf[ArraySeq[B]]
  }

  override def map[B](f: A => B): ArraySeq[B] = iterableFactory.tabulate(length)(i => f(apply(i)))

  override def prepended[B >: A](elem: B): ArraySeq[B] = {
    val dest = new Array[Any](length + 1)
    dest(0) = elem
    Array.copy(unsafeArray, 0, dest, 1, length)
    ArraySeq.unsafeWrapArray(dest).asInstanceOf[ArraySeq[B]]
  }

  override def appended[B >: A](elem: B): ArraySeq[B] = {
    val dest = new Array[Any](length + 1)
    Array.copy(unsafeArray, 0, dest, 0, length)
    dest(length) = elem
    ArraySeq.unsafeWrapArray(dest).asInstanceOf[ArraySeq[B]]
  }

  override def appendedAll[B >: A](suffix: collection.IterableOnce[B]): ArraySeq[B] = {
    val b = ArrayBuilder.make[Any]
    val k = suffix.knownSize
    if(k >= 0) b.sizeHint(k + unsafeArray.length)
    b.addAll(unsafeArray)
    b.addAll(suffix)
    ArraySeq.unsafeWrapArray(b.result()).asInstanceOf[ArraySeq[B]]
  }

  override def prependedAll[B >: A](prefix: collection.IterableOnce[B]): ArraySeq[B] = {
    val b = ArrayBuilder.make[Any]
    val k = prefix.knownSize
    if(k >= 0) b.sizeHint(k + unsafeArray.length)
    b.addAll(prefix)
    if(k < 0) b.sizeHint(b.length + unsafeArray.length)
    b.addAll(unsafeArray)
    ArraySeq.unsafeWrapArray(b.result()).asInstanceOf[ArraySeq[B]]
  }

  override def zip[B](that: collection.IterableOnce[B]): ArraySeq[(A, B)] =
    that match {
      case bs: ArraySeq[B] =>
        ArraySeq.tabulate(length min bs.length) { i =>
          (apply(i), bs(i))
        }
      case _ =>
        strictOptimizedZip[B, ArraySeq[(A, B)]](that, iterableFactory.newBuilder)
    }

  override def take(n: Int): ArraySeq[A] =
    if (unsafeArray.length <= n)
      this
    else
      ArraySeq.unsafeWrapArray(new ArrayOps(unsafeArray).take(n)).asInstanceOf[ArraySeq[A]]

  override def takeRight(n: Int): ArraySeq[A] =
    if (unsafeArray.length <= n)
      this
    else
      ArraySeq.unsafeWrapArray(new ArrayOps(unsafeArray).takeRight(n)).asInstanceOf[ArraySeq[A]]

  override def drop(n: Int): ArraySeq[A] =
    if (n <= 0)
      this
    else
      ArraySeq.unsafeWrapArray(new ArrayOps(unsafeArray).drop(n)).asInstanceOf[ArraySeq[A]]

  override def dropRight(n: Int): ArraySeq[A] =
    if (n <= 0)
      this
    else
      ArraySeq.unsafeWrapArray(new ArrayOps(unsafeArray).dropRight(n)).asInstanceOf[ArraySeq[A]]

  override def slice(from: Int, until: Int): ArraySeq[A] =
    if (from <= 0 && unsafeArray.length <= until)
      this
    else
      ArraySeq.unsafeWrapArray(new ArrayOps(unsafeArray).slice(from, until)).asInstanceOf[ArraySeq[A]]

  override def tail: ArraySeq[A] = ArraySeq.unsafeWrapArray(new ArrayOps(unsafeArray).tail).asInstanceOf[ArraySeq[A]]

  override def reverse: ArraySeq[A] = ArraySeq.unsafeWrapArray(new ArrayOps(unsafeArray).reverse).asInstanceOf[ArraySeq[A]]

  override protected[this] def className = "ArraySeq"

  override def copyToArray[B >: A](xs: Array[B], start: Int = 0): Int = copyToArray[B](xs, start, length)

  override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Int = {
    val copied = IterableOnce.elemsToCopyToArray(length, xs.length, start, len)
    if(copied > 0) {
      Array.copy(unsafeArray, 0, xs, start, copied)
    }
    copied
  }

  override protected final def applyPreferredMaxLength: Int = Int.MaxValue

  override def sorted[B >: A](implicit ord: Ordering[B]): ArraySeq[A] =
    if(unsafeArray.length <= 1) this
    else {
      val a = Array.copyAs[AnyRef](unsafeArray, length)(ClassTag.AnyRef)
      Arrays.sort(a, ord.asInstanceOf[Ordering[AnyRef]])
      new ArraySeq.ofRef[AnyRef](a).asInstanceOf[ArraySeq[A]]
    }
}

/**
  * $factoryInfo
  * @define coll immutable array
  * @define Coll `ArraySeq`
  */
@SerialVersionUID(3L)
object ArraySeq extends StrictOptimizedClassTagSeqFactory[ArraySeq] { self =>
  val untagged: SeqFactory[ArraySeq] = new ClassTagSeqFactory.AnySeqDelegate(self)

  private[this] lazy val emptyImpl = new ArraySeq.ofRef[Nothing](new Array[Nothing](0))

  def empty[A : ClassTag]: ArraySeq[A] = emptyImpl

  def from[A : ClassTag](it: scala.collection.IterableOnce[A]): ArraySeq[A] = unsafeWrapArray {
    val n = it.knownSize
    if (n > -1) {
      val elements = Array.ofDim[A](n)
      val iterator = it.iterator
      var i = 0
      while (i < n) {
        ScalaRunTime.array_update(elements, i, iterator.next())
        i = i + 1
      }
      elements
    } else ArrayBuffer.from(it).toArray
  }

  def newBuilder[A : ClassTag]: Builder[A, ArraySeq[A]] =
    ArrayBuffer.newBuilder[A].mapResult(b => unsafeWrapArray[A](b.toArray))

  override def fill[A : ClassTag](n: Int)(elem: => A): ArraySeq[A] = tabulate(n)(_ => elem)

  override def tabulate[A : ClassTag](n: Int)(f: Int => A): ArraySeq[A] = {
    val elements = Array.ofDim[A](scala.math.max(n, 0))
    var i = 0
    while (i < n) {
      ScalaRunTime.array_update(elements, i, f(i))
      i = i + 1
    }
    ArraySeq.unsafeWrapArray(elements)
  }

  /**
   * Wrap an existing `Array` into an `ArraySeq` of the proper primitive specialization type
   * without copying.
   *
   * Note that an array containing boxed primitives can be wrapped in an `ArraySeq` without
   * copying. For example, `val a: Array[Any] = Array(1)` is an array of `Object` at runtime,
   * containing `Integer`s. An `ArraySeq[Int]` can be obtained with a cast:
   * `ArraySeq.unsafeWrapArray(a).asInstanceOf[ArraySeq[Int]]`. The values are still
   * boxed, the resulting instance is an [[ArraySeq.ofRef]]. Writing
   * `ArraySeq.unsafeWrapArray(a.asInstanceOf[Array[Int]])` does not work, it throws a
   * `ClassCastException` at runtime.
   */
  def unsafeWrapArray[T](x: Array[T]): ArraySeq[T] = ((x: Array[_]) match {
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
  final class ofRef[T <: AnyRef](val unsafeArray: Array[T]) extends ArraySeq[T] {
    lazy val elemTag = ClassTag[T](unsafeArray.getClass.getComponentType)
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): T = unsafeArray(i)
    override def hashCode = MurmurHash3.arraySeqHash(unsafeArray)
    override def equals(that: Any): Boolean = that match {
      case that: ofRef[_] =>
        Array.equals(
          this.unsafeArray.asInstanceOf[Array[AnyRef]],
          that.unsafeArray.asInstanceOf[Array[AnyRef]])
      case _ => super.equals(that)
    }
    override def sorted[B >: T](implicit ord: Ordering[B]): ArraySeq.ofRef[T] = {
      if(unsafeArray.length <= 1) this
      else {
        val a = unsafeArray.clone()
        Arrays.sort(a, ord.asInstanceOf[Ordering[T]])
        new ArraySeq.ofRef(a)
      }
    }
  }

  @SerialVersionUID(3L)
  final class ofByte(val unsafeArray: Array[Byte]) extends ArraySeq[Byte] {
    protected def elemTag = ClassTag.Byte
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Byte = unsafeArray(i)
    override def hashCode = MurmurHash3.arraySeqHash(unsafeArray)
    override def equals(that: Any) = that match {
      case that: ofByte => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
    override def sorted[B >: Byte](implicit ord: Ordering[B]): ArraySeq[Byte] =
      if(length <= 1) this
      else if(ord eq Ordering.Byte) {
        val a = unsafeArray.clone()
        Arrays.sort(a)
        new ArraySeq.ofByte(a)
      } else super.sorted[B]
  }

  @SerialVersionUID(3L)
  final class ofShort(val unsafeArray: Array[Short]) extends ArraySeq[Short] {
    protected def elemTag = ClassTag.Short
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Short = unsafeArray(i)
    override def hashCode = MurmurHash3.arraySeqHash(unsafeArray)
    override def equals(that: Any) = that match {
      case that: ofShort => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
    override def sorted[B >: Short](implicit ord: Ordering[B]): ArraySeq[Short] =
      if(length <= 1) this
      else if(ord eq Ordering.Short) {
        val a = unsafeArray.clone()
        Arrays.sort(a)
        new ArraySeq.ofShort(a)
      } else super.sorted[B]
  }

  @SerialVersionUID(3L)
  final class ofChar(val unsafeArray: Array[Char]) extends ArraySeq[Char] {
    protected def elemTag = ClassTag.Char
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Char = unsafeArray(i)
    override def hashCode = MurmurHash3.arraySeqHash(unsafeArray)
    override def equals(that: Any) = that match {
      case that: ofChar => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
    override def sorted[B >: Char](implicit ord: Ordering[B]): ArraySeq[Char] =
      if(length <= 1) this
      else if(ord eq Ordering.Char) {
        val a = unsafeArray.clone()
        Arrays.sort(a)
        new ArraySeq.ofChar(a)
      } else super.sorted[B]

    override def addString(sb: StringBuilder, start: String, sep: String, end: String): StringBuilder =
      (new MutableArraySeq.ofChar(unsafeArray)).addString(sb, start, sep, end)
  }

  @SerialVersionUID(3L)
  final class ofInt(val unsafeArray: Array[Int]) extends ArraySeq[Int] {
    protected def elemTag = ClassTag.Int
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Int = unsafeArray(i)
    override def hashCode = MurmurHash3.arraySeqHash(unsafeArray)
    override def equals(that: Any) = that match {
      case that: ofInt => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
    override def sorted[B >: Int](implicit ord: Ordering[B]): ArraySeq[Int] =
      if(length <= 1) this
      else if(ord eq Ordering.Int) {
        val a = unsafeArray.clone()
        Arrays.sort(a)
        new ArraySeq.ofInt(a)
      } else super.sorted[B]
  }

  @SerialVersionUID(3L)
  final class ofLong(val unsafeArray: Array[Long]) extends ArraySeq[Long] {
    protected def elemTag = ClassTag.Long
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Long = unsafeArray(i)
    override def hashCode = MurmurHash3.arraySeqHash(unsafeArray)
    override def equals(that: Any) = that match {
      case that: ofLong => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
    override def sorted[B >: Long](implicit ord: Ordering[B]): ArraySeq[Long] =
      if(length <= 1) this
      else if(ord eq Ordering.Long) {
        val a = unsafeArray.clone()
        Arrays.sort(a)
        new ArraySeq.ofLong(a)
      } else super.sorted[B]
  }

  @SerialVersionUID(3L)
  final class ofFloat(val unsafeArray: Array[Float]) extends ArraySeq[Float] {
    protected def elemTag = ClassTag.Float
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Float = unsafeArray(i)
    override def hashCode = MurmurHash3.arraySeqHash(unsafeArray)
    override def equals(that: Any) = that match {
      case that: ofFloat => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
  }

  @SerialVersionUID(3L)
  final class ofDouble(val unsafeArray: Array[Double]) extends ArraySeq[Double] {
    protected def elemTag = ClassTag.Double
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Double = unsafeArray(i)
    override def hashCode = MurmurHash3.arraySeqHash(unsafeArray)
    override def equals(that: Any) = that match {
      case that: ofDouble => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
  }

  @SerialVersionUID(3L)
  final class ofBoolean(val unsafeArray: Array[Boolean]) extends ArraySeq[Boolean] {
    protected def elemTag = ClassTag.Boolean
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Boolean = unsafeArray(i)
    override def hashCode = MurmurHash3.arraySeqHash(unsafeArray)
    override def equals(that: Any) = that match {
      case that: ofBoolean => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
    override def sorted[B >: Boolean](implicit ord: Ordering[B]): ArraySeq[Boolean] =
      if(length <= 1) this
      else if(ord eq Ordering.Boolean) {
        val a = unsafeArray.clone()
        Sorting.stableSort(a)
        new ArraySeq.ofBoolean(a)
      } else super.sorted[B]
  }

  @SerialVersionUID(3L)
  final class ofUnit(val unsafeArray: Array[Unit]) extends ArraySeq[Unit] {
    protected def elemTag = ClassTag.Unit
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Unit = unsafeArray(i)
    override def hashCode = MurmurHash3.arraySeqHash(unsafeArray)
    override def equals(that: Any) = that match {
      case that: ofUnit => unsafeArray.length == that.unsafeArray.length
      case _ => super.equals(that)
    }
  }
}
