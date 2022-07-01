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
import scala.collection.convert.impl._
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

  def stepper[S <: Stepper[_]](implicit shape: StepperShape[A, S]): S with EfficientSplit

  @throws[ArrayIndexOutOfBoundsException]
  def apply(i: Int): A

  override def updated[B >: A](index: Int, elem: B): ArraySeq[B] = {
    val dest = new Array[Any](length)
    Array.copy(unsafeArray, 0, dest, 0, length)
    dest(index) = elem
    ArraySeq.unsafeWrapArray(dest).asInstanceOf[ArraySeq[B]]
  }

  override def map[B](f: A => B): ArraySeq[B] = {
    val a = new Array[Any](size)
    var i = 0
    while (i < a.length){
      a(i) = f(apply(i))
      i += 1
    }
    ArraySeq.unsafeWrapArray(a).asInstanceOf[ArraySeq[B]]
  }

  override def prepended[B >: A](elem: B): ArraySeq[B] =
    ArraySeq.unsafeWrapArray(unsafeArray.prepended[Any](elem)).asInstanceOf[ArraySeq[B]]

  override def appended[B >: A](elem: B): ArraySeq[B] =
    ArraySeq.unsafeWrapArray(unsafeArray.appended[Any](elem)).asInstanceOf[ArraySeq[B]]

  /** Fast concatenation of two [[ArraySeq]]s.
    *
    * @return null if optimisation not possible.
    */
  private def appendedAllArraySeq[B >: A](that: ArraySeq[B]): ArraySeq[B] = {
    // Optimise concatenation of two ArraySeqs
    // For ArraySeqs with sizes of [100, 1000, 10000] this is [3.5, 4.1, 5.2]x as fast
    if (isEmpty)
      that
    else if (that.isEmpty)
      this
    else {
      val thisIsObj = this.unsafeArray.isInstanceOf[Array[AnyRef]]
      val thatIsObj = that.unsafeArray.isInstanceOf[Array[AnyRef]]
      val mismatch = thisIsObj != thatIsObj
      if (mismatch)
        // Combining primatives and objects: abort
        null
      else if (thisIsObj) {
        // A and B are objects
        val ax = this.unsafeArray.asInstanceOf[Array[A]]
        val ay = that.unsafeArray.asInstanceOf[Array[B]]
        val len = ax.length + ay.length
        val a = new Array[AnyRef](len)
        System.arraycopy(ax, 0, a, 0, ax.length)
        System.arraycopy(ay, 0, a, ax.length, ay.length)
        ArraySeq.unsafeWrapArray(a).asInstanceOf[ArraySeq[B]]
      } else {
        // A is a primative and B = A. Use this instance's protected ClassTag.
        val ax = this.unsafeArray.asInstanceOf[Array[A]]
        val ay = that.unsafeArray.asInstanceOf[Array[A]]
        val len = ax.length + ay.length
        val a = iterableEvidence.newArray(len)
        System.arraycopy(ax, 0, a, 0, ax.length)
        System.arraycopy(ay, 0, a, ax.length, ay.length)
        ArraySeq.unsafeWrapArray(a).asInstanceOf[ArraySeq[B]]
      }
    }
  }

  override def appendedAll[B >: A](suffix: collection.IterableOnce[B]): ArraySeq[B] = {
    def genericResult = {
      val k = suffix.knownSize
      if (k == 0) this
      else {
        val b = ArrayBuilder.make[Any]
        if(k >= 0) b.sizeHint(k + unsafeArray.length)
        b.addAll(unsafeArray)
        b.addAll(suffix)
        ArraySeq.unsafeWrapArray(b.result()).asInstanceOf[ArraySeq[B]]
      }
    }

    suffix match {
      case that: ArraySeq[_] =>
        val result = appendedAllArraySeq(that.asInstanceOf[ArraySeq[B]])
        if (result == null) genericResult
        else result
      case _ =>
        genericResult
    }
  }

  override def prependedAll[B >: A](prefix: collection.IterableOnce[B]): ArraySeq[B] = {
    def genericResult = {
      val k = prefix.knownSize
      if (k == 0) this
      else {
        val b = ArrayBuilder.make[Any]
        if(k >= 0) b.sizeHint(k + unsafeArray.length)
        b.addAll(prefix)
        if(k < 0) b.sizeHint(b.length + unsafeArray.length)
        b.addAll(unsafeArray)
        ArraySeq.unsafeWrapArray(b.result()).asInstanceOf[ArraySeq[B]]
      }
    }

    prefix match {
      case that: ArraySeq[_] =>
        val result = that.asInstanceOf[ArraySeq[B]].appendedAllArraySeq(this)
        if (result == null) genericResult
        else result
      case _ =>
        genericResult
    }
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

  override def foldLeft[B](z: B)(f: (B, A) => B): B = {
    // For ArraySeqs with sizes of [100, 1000, 10000] this is [1.3, 1.8, 1.8]x as fast
    // as the same while-loop over this instead of unsafeArray.
    val array = unsafeArray
    var b = z
    var i = 0
    while (i < array.length) {
      val a = array(i).asInstanceOf[A]
      b = f(b, a)
      i += 1
    }
    b
  }

  override def foldRight[B](z: B)(f: (A, B) => B): B = {
    // For ArraySeqs with sizes of [100, 1000, 10000] this is [1.6, 1.8, 2.7]x as fast
    // as the same while-loop over this instead of unsafeArray.
    val array = unsafeArray
    var b = z
    var i = array.length
    while (i > 0) {
      i -= 1
      val a = array(i).asInstanceOf[A]
      b = f(a, b)
    }
    b
  }

  override def tail: ArraySeq[A] = ArraySeq.unsafeWrapArray(new ArrayOps(unsafeArray).tail).asInstanceOf[ArraySeq[A]]

  override def reverse: ArraySeq[A] = ArraySeq.unsafeWrapArray(new ArrayOps(unsafeArray).reverse).asInstanceOf[ArraySeq[A]]

  override protected[this] def className = "ArraySeq"

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

  def from[A](it: scala.collection.IterableOnce[A])(implicit tag: ClassTag[A]): ArraySeq[A] = it match {
    case as: ArraySeq[A] => as
    case _ => unsafeWrapArray(Array.from[A](it))
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
   * without copying. Any changes to wrapped array will break the expected immutability.
   *
   * Note that an array containing boxed primitives can be wrapped in an `ArraySeq` without
   * copying. For example, `val a: Array[Any] = Array(1)` is an array of `Object` at runtime,
   * containing `Integer`s. An `ArraySeq[Int]` can be obtained with a cast:
   * `ArraySeq.unsafeWrapArray(a).asInstanceOf[ArraySeq[Int]]`. The values are still
   * boxed, the resulting instance is an [[ArraySeq.ofRef]]. Writing
   * `ArraySeq.unsafeWrapArray(a.asInstanceOf[Array[Int]])` does not work, it throws a
   * `ClassCastException` at runtime.
   */
  def unsafeWrapArray[T](x: Array[T]): ArraySeq[T] = ((x: @unchecked) match {
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
    def elemTag = ClassTag[T](unsafeArray.getClass.getComponentType)
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
    override def iterator: Iterator[T] = new ArrayOps.ArrayIterator[T](unsafeArray)
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[T, S]): S with EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        new ObjectArrayStepper(unsafeArray, 0, unsafeArray.length)
      else shape.parUnbox(new ObjectArrayStepper(unsafeArray, 0, unsafeArray.length).asInstanceOf[AnyStepper[T] with EfficientSplit])
    ).asInstanceOf[S with EfficientSplit]
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
    override def iterator: Iterator[Byte] = new ArrayOps.ArrayIterator[Byte](unsafeArray)
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Byte, S]): S with EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParIntStepper(new WidenedByteArrayStepper(unsafeArray, 0, unsafeArray.length))
      else new WidenedByteArrayStepper(unsafeArray, 0, unsafeArray.length)
    ).asInstanceOf[S with EfficientSplit]
    override def updated[B >: Byte](index: Int, elem: B): ArraySeq[B] =
      elem match {
        case b: Byte => new ArraySeq.ofByte(unsafeArray.updated(index, b))
        case _ => super.updated(index, elem)
      }
    override def appended[B >: Byte](elem: B): ArraySeq[B] =
      elem match {
        case b: Byte => new ArraySeq.ofByte(unsafeArray.appended(b))
        case _ => super.appended(elem)
      }
    override def prepended[B >: Byte](elem: B): ArraySeq[B] =
      elem match {
        case b: Byte => new ArraySeq.ofByte(unsafeArray.prepended(b))
        case _ => super.prepended(elem)
      }
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
    override def iterator: Iterator[Short] = new ArrayOps.ArrayIterator[Short](unsafeArray)
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Short, S]): S with EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParIntStepper(new WidenedShortArrayStepper(unsafeArray, 0, unsafeArray.length))
      else new WidenedShortArrayStepper(unsafeArray, 0, unsafeArray.length)
    ).asInstanceOf[S with EfficientSplit]
    override def updated[B >: Short](index: Int, elem: B): ArraySeq[B] =
      elem match {
        case b: Short => new ArraySeq.ofShort(unsafeArray.updated(index, b))
        case _ => super.updated(index, elem)
      }
    override def appended[B >: Short](elem: B): ArraySeq[B] =
      elem match {
        case b: Short => new ArraySeq.ofShort(unsafeArray.appended(b))
        case _ => super.appended(elem)
      }
    override def prepended[B >: Short](elem: B): ArraySeq[B] =
      elem match {
        case b: Short => new ArraySeq.ofShort(unsafeArray.prepended(b))
        case _ => super.prepended(elem)
      }
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
    override def iterator: Iterator[Char] = new ArrayOps.ArrayIterator[Char](unsafeArray)
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Char, S]): S with EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParIntStepper(new WidenedCharArrayStepper(unsafeArray, 0, unsafeArray.length))
      else new WidenedCharArrayStepper(unsafeArray, 0, unsafeArray.length)
    ).asInstanceOf[S with EfficientSplit]
    override def updated[B >: Char](index: Int, elem: B): ArraySeq[B] =
      elem match {
        case b: Char => new ArraySeq.ofChar(unsafeArray.updated(index, b))
        case _ => super.updated(index, elem)
      }
    override def appended[B >: Char](elem: B): ArraySeq[B] =
      elem match {
        case b: Char => new ArraySeq.ofChar(unsafeArray.appended(b))
        case _ => super.appended(elem)
      }
    override def prepended[B >: Char](elem: B): ArraySeq[B] =
      elem match {
        case b: Char => new ArraySeq.ofChar(unsafeArray.prepended(b))
        case _ => super.prepended(elem)
      }

    override def addString(sb: StringBuilder, start: String, sep: String, end: String): sb.type =
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
    override def iterator: Iterator[Int] = new ArrayOps.ArrayIterator[Int](unsafeArray)
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Int, S]): S with EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParIntStepper(new IntArrayStepper(unsafeArray, 0, unsafeArray.length))
      else new IntArrayStepper(unsafeArray, 0, unsafeArray.length)
    ).asInstanceOf[S with EfficientSplit]
    override def updated[B >: Int](index: Int, elem: B): ArraySeq[B] =
      elem match {
        case b: Int => new ArraySeq.ofInt(unsafeArray.updated(index, b))
        case _ => super.updated(index, elem)
      }
    override def appended[B >: Int](elem: B): ArraySeq[B] =
      elem match {
        case b: Int => new ArraySeq.ofInt(unsafeArray.appended(b))
        case _ => super.appended(elem)
      }
    override def prepended[B >: Int](elem: B): ArraySeq[B] =
      elem match {
        case b: Int => new ArraySeq.ofInt(unsafeArray.prepended(b))
        case _ => super.prepended(elem)
      }
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
    override def iterator: Iterator[Long] = new ArrayOps.ArrayIterator[Long](unsafeArray)
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Long, S]): S with EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParLongStepper(new LongArrayStepper(unsafeArray, 0, unsafeArray.length))
      else new LongArrayStepper(unsafeArray, 0, unsafeArray.length)
    ).asInstanceOf[S with EfficientSplit]
    override def updated[B >: Long](index: Int, elem: B): ArraySeq[B] =
      elem match {
        case b: Long => new ArraySeq.ofLong(unsafeArray.updated(index, b))
        case _ => super.updated(index, elem)
      }
    override def appended[B >: Long](elem: B): ArraySeq[B] =
      elem match {
        case b: Long => new ArraySeq.ofLong(unsafeArray.appended(b))
        case _ => super.appended(elem)
      }
    override def prepended[B >: Long](elem: B): ArraySeq[B] =
      elem match {
        case b: Long => new ArraySeq.ofLong(unsafeArray.prepended(b))
        case _ => super.prepended(elem)
      }
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
    override def iterator: Iterator[Float] = new ArrayOps.ArrayIterator[Float](unsafeArray)
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Float, S]): S with EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParDoubleStepper(new WidenedFloatArrayStepper(unsafeArray, 0, unsafeArray.length))
      else new WidenedFloatArrayStepper(unsafeArray, 0, unsafeArray.length)
    ).asInstanceOf[S with EfficientSplit]
    override def updated[B >: Float](index: Int, elem: B): ArraySeq[B] =
      elem match {
        case b: Float => new ArraySeq.ofFloat(unsafeArray.updated(index, b))
        case _ => super.updated(index, elem)
      }
    override def appended[B >: Float](elem: B): ArraySeq[B] =
      elem match {
        case b: Float => new ArraySeq.ofFloat(unsafeArray.appended(b))
        case _ => super.appended(elem)
      }
    override def prepended[B >: Float](elem: B): ArraySeq[B] =
      elem match {
        case b: Float => new ArraySeq.ofFloat(unsafeArray.prepended(b))
        case _ => super.prepended(elem)
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
    override def iterator: Iterator[Double] = new ArrayOps.ArrayIterator[Double](unsafeArray)
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Double, S]): S with EfficientSplit = (
      if(shape.shape == StepperShape.ReferenceShape)
        AnyStepper.ofParDoubleStepper(new DoubleArrayStepper(unsafeArray, 0, unsafeArray.length))
      else new DoubleArrayStepper(unsafeArray, 0, unsafeArray.length)
    ).asInstanceOf[S with EfficientSplit]
    override def updated[B >: Double](index: Int, elem: B): ArraySeq[B] =
      elem match {
        case b: Double => new ArraySeq.ofDouble(unsafeArray.updated(index, b))
        case _ => super.updated(index, elem)
      }
    override def appended[B >: Double](elem: B): ArraySeq[B] =
      elem match {
        case b: Double => new ArraySeq.ofDouble(unsafeArray.appended(b))
        case _ => super.appended(elem)
      }
    override def prepended[B >: Double](elem: B): ArraySeq[B] =
      elem match {
        case b: Double => new ArraySeq.ofDouble(unsafeArray.prepended(b))
        case _ => super.prepended(elem)
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
    override def iterator: Iterator[Boolean] = new ArrayOps.ArrayIterator[Boolean](unsafeArray)
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Boolean, S]): S with EfficientSplit =
      new BoxedBooleanArrayStepper(unsafeArray, 0, unsafeArray.length).asInstanceOf[S with EfficientSplit]
    override def updated[B >: Boolean](index: Int, elem: B): ArraySeq[B] =
      elem match {
        case b: Boolean => new ArraySeq.ofBoolean(unsafeArray.updated(index, b))
        case _ => super.updated(index, elem)
      }
    override def appended[B >: Boolean](elem: B): ArraySeq[B] =
      elem match {
        case b: Boolean => new ArraySeq.ofBoolean(unsafeArray.appended(b))
        case _ => super.appended(elem)
      }
    override def prepended[B >: Boolean](elem: B): ArraySeq[B] =
      elem match {
        case b: Boolean => new ArraySeq.ofBoolean(unsafeArray.prepended(b))
        case _ => super.prepended(elem)
      }
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
    override def iterator: Iterator[Unit] = new ArrayOps.ArrayIterator[Unit](unsafeArray)
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Unit, S]): S with EfficientSplit =
      new ObjectArrayStepper[AnyRef](unsafeArray.asInstanceOf[Array[AnyRef]], 0, unsafeArray.length).asInstanceOf[S with EfficientSplit]
  }
}
