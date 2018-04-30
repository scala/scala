package scala
package collection.immutable

import scala.collection.mutable.{ArrayBuffer, Builder, ArrayBuilder}
import scala.collection.{IterableOnce, Iterator, SeqFactory, ClassTagSeqFactory, StrictOptimizedClassTagSeqFactory, View, ArrayOps}

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
  * @define Coll `ImmutableArray`
  */
sealed abstract class ImmutableArray[+A]
  extends AbstractSeq[A]
    with IndexedSeq[A]
    with IndexedSeqOps[A, ImmutableArray, ImmutableArray[A]]
    with StrictOptimizedSeqOps[A, ImmutableArray, ImmutableArray[A]] {

  /** The tag of the element type */
  protected def elemTag: ClassTag[A] @uncheckedVariance

  override def iterableFactory: SeqFactory[ImmutableArray] = ImmutableArray.untagged

  /** The wrapped mutable `Array` that backs this `ImmutableArray`. Any changes to this array will break
    * the expected immutability. */
  def unsafeArray: Array[A @uncheckedVariance]
  // uncheckedVariance should be safe: Array[A] for reference types A is covariant at the JVM level. Array[A] for
  // primitive types A can only be widened to Array[Any] which erases to Object.

  override protected def fromSpecificIterable(coll: scala.collection.Iterable[A] @uncheckedVariance): ImmutableArray[A] = ImmutableArray.from[A](coll)(elemTag)

  override protected def newSpecificBuilder(): Builder[A, ImmutableArray[A]] @uncheckedVariance = ImmutableArray.newBuilder[A]()(elemTag)

  @throws[ArrayIndexOutOfBoundsException]
  def apply(i: Int): A

  override def updated[B >: A](index: Int, elem: B): ImmutableArray[B] = {
    val dest = new Array[Any](length)
    Array.copy(unsafeArray, 0, dest, 0, length)
    dest(index) = elem
    ImmutableArray.unsafeWrapArray(dest).asInstanceOf[ImmutableArray[B]]
  }

  override def map[B](f: A => B): ImmutableArray[B] = iterableFactory.tabulate(length)(i => f(apply(i)))

  override def prepended[B >: A](elem: B): ImmutableArray[B] = {
    val dest = new Array[Any](length + 1)
    dest(0) = elem
    Array.copy(unsafeArray, 0, dest, 1, length)
    ImmutableArray.unsafeWrapArray(dest).asInstanceOf[ImmutableArray[B]]
  }

  override def appended[B >: A](elem: B): ImmutableArray[B] = {
    val dest = new Array[Any](length + 1)
    Array.copy(unsafeArray, 0, dest, 0, length)
    dest(length) = elem
    ImmutableArray.unsafeWrapArray(dest).asInstanceOf[ImmutableArray[B]]
  }

  override def appendedAll[B >: A](suffix: collection.Iterable[B]): ImmutableArray[B] = {
    val b = ArrayBuilder.make[Any]()
    val k = suffix.knownSize
    if(k >= 0) b.sizeHint(k + unsafeArray.length)
    b.addAll(unsafeArray)
    b.addAll(suffix)
    ImmutableArray.unsafeWrapArray(b.result()).asInstanceOf[ImmutableArray[B]]
  }

  override def prependedAll[B >: A](prefix: collection.Iterable[B]): ImmutableArray[B] = {
    val b = ArrayBuilder.make[Any]()
    val k = prefix.knownSize
    if(k >= 0) b.sizeHint(k + unsafeArray.length)
    b.addAll(prefix)
    if(k < 0) b.sizeHint(b.length + unsafeArray.length)
    b.addAll(unsafeArray)
    ImmutableArray.unsafeWrapArray(b.result()).asInstanceOf[ImmutableArray[B]]
  }

  override def zip[B](that: collection.Iterable[B]): ImmutableArray[(A, B)] =
    that match {
      case bs: ImmutableArray[B] =>
        ImmutableArray.tabulate(length min bs.length) { i =>
          (apply(i), bs(i))
        }
      case _ =>
        fromIterable(new View.Zip(toIterable, that))
    }

  override def take(n: Int): ImmutableArray[A] = ImmutableArray.unsafeWrapArray(new ArrayOps(unsafeArray).take(n))

  override def takeRight(n: Int): ImmutableArray[A] = ImmutableArray.unsafeWrapArray(new ArrayOps(unsafeArray).takeRight(n))

  override def drop(n: Int): ImmutableArray[A] = ImmutableArray.unsafeWrapArray(new ArrayOps(unsafeArray).drop(n))

  override def dropRight(n: Int): ImmutableArray[A] = ImmutableArray.unsafeWrapArray(new ArrayOps(unsafeArray).dropRight(n))

  override def slice(from: Int, until: Int): ImmutableArray[A] = ImmutableArray.unsafeWrapArray(new ArrayOps(unsafeArray).slice(from, until))

  override def tail: ImmutableArray[A] = ImmutableArray.unsafeWrapArray(new ArrayOps(unsafeArray).tail)

  override def reverse: ImmutableArray[A] = ImmutableArray.unsafeWrapArray(new ArrayOps(unsafeArray).reverse)

  override def className = "ImmutableArray"

  override def copyToArray[B >: A](xs: Array[B], start: Int = 0): xs.type = copyToArray[B](xs, start, length)

  override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): xs.type = {
    val l = scala.math.min(scala.math.min(len, length), xs.length-start)
    if(l > 0) Array.copy(unsafeArray, 0, xs, start, l)
    xs
  }
}

/**
  * $factoryInfo
  * @define coll immutable array
  * @define Coll `ImmutableArray`
  */
object ImmutableArray extends StrictOptimizedClassTagSeqFactory[ImmutableArray] { self =>
  val untagged: SeqFactory[ImmutableArray] = new ClassTagSeqFactory.AnySeqDelegate(self)

  private[this] lazy val emptyImpl = new ImmutableArray.ofRef[Nothing](new Array[Nothing](0))

  def empty[A : ClassTag]: ImmutableArray[A] = emptyImpl

  def from[A : ClassTag](it: scala.collection.IterableOnce[A]): ImmutableArray[A] = unsafeWrapArray {
    val n = it.knownSize
    if (n > -1) {
      val elements = Array.ofDim[A](n)
      val iterator = it.iterator()
      var i = 0
      while (i < n) {
        ScalaRunTime.array_update(elements, i, iterator.next())
        i = i + 1
      }
      elements
    } else ArrayBuffer.from(it).toArray
  }

  def newBuilder[A : ClassTag](): Builder[A, ImmutableArray[A]] =
    ArrayBuffer.newBuilder[A]().mapResult(b => unsafeWrapArray[A](b.toArray))

  override def fill[A : ClassTag](n: Int)(elem: => A): ImmutableArray[A] = tabulate(n)(_ => elem)

  override def tabulate[A : ClassTag](n: Int)(f: Int => A): ImmutableArray[A] = {
    val elements = Array.ofDim[A](scala.math.max(n, 0))
    var i = 0
    while (i < n) {
      ScalaRunTime.array_update(elements, i, f(i))
      i = i + 1
    }
    ImmutableArray.unsafeWrapArray(elements)
  }

  /**
   * Wrap an existing `Array` into an `ImmutableArray` of the proper primitive specialization type
   * without copying.
   *
   * Note that an array containing boxed primitives can be wrapped in an `ImmutableArray` without
   * copying. For example, `val a: Array[Any] = Array(1)` is an array of `Object` at runtime,
   * containing `Integer`s. An `ImmutableArray[Int]` can be obtained with a cast:
   * `ImmutableArray.unsafeWrapArray(a).asInstanceOf[ImmutableArray[Int]]`. The values are still
   * boxed, the resulting instance is an [[ImmutableArray.ofRef]]. Writing
   * `ImmutableArray.unsafeWrapArray(a.asInstanceOf[Array[Int]])` does not work, it throws a
   * `ClassCastException` at runtime.
   */
  def unsafeWrapArray[T](x: Array[T]): ImmutableArray[T] = (x.asInstanceOf[Array[_]] match {
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
  }).asInstanceOf[ImmutableArray[T]]

  @SerialVersionUID(3L)
  final class ofRef[T <: AnyRef](val unsafeArray: Array[T]) extends ImmutableArray[T] with Serializable {
    lazy val elemTag = ClassTag[T](unsafeArray.getClass.getComponentType)
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): T = unsafeArray(i)
    override def hashCode = MurmurHash3.arrayHash(unsafeArray, MurmurHash3.seqSeed)
    override def equals(that: Any) = that match {
      case that: ofRef[_] => Arrays.equals(unsafeArray.asInstanceOf[Array[AnyRef]], that.unsafeArray.asInstanceOf[Array[AnyRef]])
      case _ => super.equals(that)
    }
  }

  @SerialVersionUID(3L)
  final class ofByte(val unsafeArray: Array[Byte]) extends ImmutableArray[Byte] with Serializable {
    protected def elemTag = ClassTag.Byte
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Byte = unsafeArray(i)
    override def hashCode = MurmurHash3.arrayHash(unsafeArray, MurmurHash3.seqSeed)
    override def equals(that: Any) = that match {
      case that: ofByte => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
  }

  @SerialVersionUID(3L)
  final class ofShort(val unsafeArray: Array[Short]) extends ImmutableArray[Short] with Serializable {
    protected def elemTag = ClassTag.Short
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Short = unsafeArray(i)
    override def hashCode = MurmurHash3.arrayHash(unsafeArray, MurmurHash3.seqSeed)
    override def equals(that: Any) = that match {
      case that: ofShort => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
  }

  @SerialVersionUID(3L)
  final class ofChar(val unsafeArray: Array[Char]) extends ImmutableArray[Char] with Serializable {
    protected def elemTag = ClassTag.Char
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Char = unsafeArray(i)
    override def hashCode = MurmurHash3.arrayHash(unsafeArray, MurmurHash3.seqSeed)
    override def equals(that: Any) = that match {
      case that: ofChar => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
  }

  @SerialVersionUID(3L)
  final class ofInt(val unsafeArray: Array[Int]) extends ImmutableArray[Int] with Serializable {
    protected def elemTag = ClassTag.Int
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Int = unsafeArray(i)
    override def hashCode = MurmurHash3.arrayHash(unsafeArray, MurmurHash3.seqSeed)
    override def equals(that: Any) = that match {
      case that: ofInt => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
  }

  @SerialVersionUID(3L)
  final class ofLong(val unsafeArray: Array[Long]) extends ImmutableArray[Long] with Serializable {
    protected def elemTag = ClassTag.Long
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Long = unsafeArray(i)
    override def hashCode = MurmurHash3.arrayHash(unsafeArray, MurmurHash3.seqSeed)
    override def equals(that: Any) = that match {
      case that: ofLong => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
  }

  @SerialVersionUID(3L)
  final class ofFloat(val unsafeArray: Array[Float]) extends ImmutableArray[Float] with Serializable {
    protected def elemTag = ClassTag.Float
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Float = unsafeArray(i)
    override def hashCode = MurmurHash3.arrayHash(unsafeArray, MurmurHash3.seqSeed)
    override def equals(that: Any) = that match {
      case that: ofFloat => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
  }

  @SerialVersionUID(3L)
  final class ofDouble(val unsafeArray: Array[Double]) extends ImmutableArray[Double] with Serializable {
    protected def elemTag = ClassTag.Double
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Double = unsafeArray(i)
    override def hashCode = MurmurHash3.arrayHash(unsafeArray, MurmurHash3.seqSeed)
    override def equals(that: Any) = that match {
      case that: ofDouble => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
  }

  @SerialVersionUID(3L)
  final class ofBoolean(val unsafeArray: Array[Boolean]) extends ImmutableArray[Boolean] with Serializable {
    protected def elemTag = ClassTag.Boolean
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Boolean = unsafeArray(i)
    override def hashCode = MurmurHash3.arrayHash(unsafeArray, MurmurHash3.seqSeed)
    override def equals(that: Any) = that match {
      case that: ofBoolean => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
  }

  @SerialVersionUID(3L)
  final class ofUnit(val unsafeArray: Array[Unit]) extends ImmutableArray[Unit] with Serializable {
    protected def elemTag = ClassTag.Unit
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Unit = unsafeArray(i)
    override def hashCode = MurmurHash3.arrayHash(unsafeArray, MurmurHash3.seqSeed)
    override def equals(that: Any) = that match {
      case that: ofUnit => unsafeArray.length == that.unsafeArray.length
      case _ => super.equals(that)
    }
  }
}
