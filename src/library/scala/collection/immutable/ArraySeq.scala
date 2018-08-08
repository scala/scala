package scala
package collection.immutable

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.collection.mutable.{ArrayBuffer, ArrayBuilder, Builder, ArraySeq => MutableArraySeq}
import scala.collection.{ArrayOps, ClassTagSeqFactory, SeqFactory, StrictOptimizedClassTagSeqFactory}
import scala.collection.IterableOnce
import scala.annotation.unchecked.uncheckedVariance
import scala.util.hashing.MurmurHash3
import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime
import java.util.Arrays

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
    with StrictOptimizedSeqOps[A, ArraySeq, ArraySeq[A]] {

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

  override protected def fromSpecific(coll: scala.collection.IterableOnce[A] @uncheckedVariance): ArraySeq[A] = ArraySeq.from(coll)(elemTag.asInstanceOf[ClassTag[A]])

  override protected def newSpecificBuilder: Builder[A, ArraySeq[A]] @uncheckedVariance = ArraySeq.newBuilder[A](elemTag.asInstanceOf[ClassTag[A]])

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

  override protected[this] def writeReplace(): AnyRef = this

  override def equals(other: Any): Boolean = other match {
    case that: ArraySeq[_] if this.unsafeArray.length != that.unsafeArray.length =>
      false
    case _ =>
      super.equals(other)
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

  // scalac generates a `readReplace` method to discard the deserialized state (see https://github.com/scala/bug/issues/10412).
  // This prevents it from serializing it in the first place:
  private[this] def writeObject(out: ObjectOutputStream): Unit = ()
  private[this] def readObject(in: ObjectInputStream): Unit = ()
}
