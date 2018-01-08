package strawman
package collection.immutable

import strawman.collection.mutable.{ArrayBuffer, Builder}
import strawman.collection.{IterableOnce, Iterator, SeqFactory, StrictOptimizedSeqFactory, View}

import scala.{Any, ArrayIndexOutOfBoundsException, Boolean, Int, Nothing, UnsupportedOperationException, throws, Array, AnyRef, `inline`, Serializable, Byte, Short, Long, Double, Unit, Float, Char}
import scala.annotation.unchecked.uncheckedVariance
import scala.util.hashing.MurmurHash3
import scala.runtime.ScalaRunTime
import scala.Predef.intWrapper
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
  extends IndexedSeq[A]
    with IndexedSeqOps[A, ImmutableArray, ImmutableArray[A]]
    with StrictOptimizedSeqOps[A, ImmutableArray, ImmutableArray[A]] {

  def iterableFactory: SeqFactory[ImmutableArray] = ImmutableArray

  /** The wrapped mutable `Array` that backs this `ImmutableArray`. Any changes to this array will break
    * the expected immutability. */
  def unsafeArray: Array[A @uncheckedVariance]
  // uncheckedVariance should be safe: Array[A] for reference types A is covariant at the JVM level. Array[A] for
  // primitive types A can only be widened to Array[Any] which erases to Object.

  protected[this] def fromSpecificIterable(coll: strawman.collection.Iterable[A]): ImmutableArray[A] = fromIterable(coll)

  protected[this] def newSpecificBuilder(): Builder[A, ImmutableArray[A]] = ImmutableArray.newBuilder[A]()

  @throws[ArrayIndexOutOfBoundsException]
  def apply(i: Int): A

  override def updated[B >: A](index: Int, elem: B): ImmutableArray[B] = {
    val dest = scala.Array.ofDim[Any](length)
    scala.Array.copy(unsafeArray, 0, dest, 0, length)
    dest(index) = elem
    ImmutableArray.unsafeWrapArray(dest)
  }

  override def map[B](f: A => B): ImmutableArray[B] = ImmutableArray.tabulate(length)(i => f(apply(i)))

  override def prepended[B >: A](elem: B): ImmutableArray[B] = {
    val dest = scala.Array.ofDim[Any](length + 1)
    dest(0) = elem
    scala.Array.copy(unsafeArray, 0, dest, 1, length)
    ImmutableArray.unsafeWrapArray(dest)
  }

  override def appended[B >: A](elem: B): ImmutableArray[B] = {
    val dest = scala.Array.ofDim[Any](length + 1)
    scala.Array.copy(unsafeArray, 0, dest, 0, length)
    dest(length) = elem
    ImmutableArray.unsafeWrapArray(dest)
  }

  override def appendedAll[B >: A](xs: collection.Iterable[B]): ImmutableArray[B] =
    xs match {
      case bs: ImmutableArray[B] =>
        val dest = scala.Array.ofDim[Any](length + bs.length)
        scala.Array.copy(unsafeArray, 0, dest, 0, length)
        scala.Array.copy(bs.unsafeArray, 0, dest, length, bs.length)
        ImmutableArray.unsafeWrapArray(dest)
      case _ =>
        fromIterable(View.Concat(toIterable, xs))
    }

  override def prependedAll[B >: A](xs: collection.Iterable[B]): ImmutableArray[B] =
    xs match {
      case bs: ImmutableArray[B] =>
        val dest = scala.Array.ofDim[Any](length + bs.length)
        java.lang.System.arraycopy(bs.unsafeArray, 0, dest, 0, bs.length)
        java.lang.System.arraycopy(unsafeArray, 0, dest, bs.length, length)
        ImmutableArray.unsafeWrapArray(dest)
      case _ =>
        fromIterable(View.Concat(xs, toIterable))
    }

  override def zip[B](that: collection.Iterable[B]): ImmutableArray[(A, B)] =
    that match {
      case bs: ImmutableArray[B] =>
        ImmutableArray.tabulate(length min bs.length) { i =>
          (apply(i), bs(i))
        }
      case _ =>
        fromIterable(View.Zip(toIterable, that))
    }

  override def take(n: Int): ImmutableArray[A] = ImmutableArray.tabulate(n)(apply)

  override def takeRight(n: Int): ImmutableArray[A] = ImmutableArray.tabulate(n min length)(i => apply(length - (n min length) + i))

  override def drop(n: Int): ImmutableArray[A] = ImmutableArray.tabulate((length - n) max 0)(i => apply(n + i))

  override def dropRight(n: Int): ImmutableArray[A] = ImmutableArray.tabulate((length - n) max 0)(apply)

  override def slice(from: Int, until: Int): ImmutableArray[A] = {
    val lo = scala.math.max(from, 0)
    ImmutableArray.tabulate(until - lo)(i => apply(i + lo))
  }

  override def tail: ImmutableArray[A] =
    if (length > 0) {
      val dest = scala.Array.ofDim[Any](length - 1)
      java.lang.System.arraycopy(unsafeArray, 1, dest, 0, length - 1)
      ImmutableArray.unsafeWrapArray(dest)
    } else throw new UnsupportedOperationException("tail of empty array")

  override def reverse: ImmutableArray[A] = ImmutableArray.tabulate(length)(i => apply(length - 1 - i))

  override def className = "ImmutableArray"
}

/**
  * $factoryInfo
  * @define coll immutable array
  * @define Coll `ImmutableArray`
  */
object ImmutableArray extends StrictOptimizedSeqFactory[ImmutableArray] {

  private[this] lazy val emptyImpl = new ImmutableArray.ofRef[Nothing](new scala.Array[Nothing](0))

  def empty[A]: ImmutableArray[A] = emptyImpl

  def fromArrayBuffer[A](arr: ArrayBuffer[A]): ImmutableArray[A] =
    unsafeWrapArray[A](arr.asInstanceOf[ArrayBuffer[Any]].toArray)

  def from[A](it: strawman.collection.IterableOnce[A]): ImmutableArray[A] = {
    val n = it.knownSize
    if (n > -1) {
      val elements = scala.Array.ofDim[Any](n)
      val iterator = it.iterator()
      var i = 0
      while (i < n) {
        ScalaRunTime.array_update(elements, i, iterator.next())
        i = i + 1
      }
      unsafeWrapArray(elements)
    } else fromArrayBuffer(ArrayBuffer.from(it))
  }

  def newBuilder[A](): Builder[A, ImmutableArray[A]] =
    ArrayBuffer.newBuilder[A]().mapResult(fromArrayBuffer)

  override def fill[A](n: Int)(elem: => A): ImmutableArray[A] = tabulate(n)(_ => elem)

  override def tabulate[A](n: Int)(f: Int => A): ImmutableArray[A] = {
    val elements = scala.Array.ofDim[Any](scala.math.max(n, 0))
    var i = 0
    while (i < n) {
      ScalaRunTime.array_update(elements, i, f(i))
      i = i + 1
    }
    ImmutableArray.unsafeWrapArray(elements)
  }

  /** Wrap an existing `Array` into an `ImmutableArray` of the proper primitive specialization type without copying. */
  def unsafeWrapArray[T](x: Array[_]): ImmutableArray[T] = (x match {
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

  final class ofRef[T <: AnyRef](val unsafeArray: Array[T]) extends ImmutableArray[T] with Serializable {
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): T = unsafeArray(i)
    override def hashCode = MurmurHash3.arrayHash(unsafeArray)
    override def equals(that: Any) = that match {
      case that: ofRef[_] => Arrays.equals(unsafeArray.asInstanceOf[Array[AnyRef]], that.unsafeArray.asInstanceOf[Array[AnyRef]])
      case _ => super.equals(that)
    }
  }

  final class ofByte(val unsafeArray: Array[Byte]) extends ImmutableArray[Byte] with Serializable {
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Byte = unsafeArray(i)
    override def hashCode = MurmurHash3.arrayHash(unsafeArray)
    override def equals(that: Any) = that match {
      case that: ofByte => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
  }

  final class ofShort(val unsafeArray: Array[Short]) extends ImmutableArray[Short] with Serializable {
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Short = unsafeArray(i)
    override def hashCode = MurmurHash3.arrayHash(unsafeArray)
    override def equals(that: Any) = that match {
      case that: ofShort => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
  }

  final class ofChar(val unsafeArray: Array[Char]) extends ImmutableArray[Char] with Serializable {
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Char = unsafeArray(i)
    override def hashCode = MurmurHash3.arrayHash(unsafeArray)
    override def equals(that: Any) = that match {
      case that: ofChar => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
  }

  final class ofInt(val unsafeArray: Array[Int]) extends ImmutableArray[Int] with Serializable {
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Int = unsafeArray(i)
    override def hashCode = MurmurHash3.arrayHash(unsafeArray)
    override def equals(that: Any) = that match {
      case that: ofInt => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
  }

  final class ofLong(val unsafeArray: Array[Long]) extends ImmutableArray[Long] with Serializable {
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Long = unsafeArray(i)
    override def hashCode = MurmurHash3.arrayHash(unsafeArray)
    override def equals(that: Any) = that match {
      case that: ofLong => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
  }

  final class ofFloat(val unsafeArray: Array[Float]) extends ImmutableArray[Float] with Serializable {
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Float = unsafeArray(i)
    override def hashCode = MurmurHash3.arrayHash(unsafeArray)
    override def equals(that: Any) = that match {
      case that: ofFloat => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
  }

  final class ofDouble(val unsafeArray: Array[Double]) extends ImmutableArray[Double] with Serializable {
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Double = unsafeArray(i)
    override def hashCode = MurmurHash3.arrayHash(unsafeArray)
    override def equals(that: Any) = that match {
      case that: ofDouble => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
  }

  final class ofBoolean(val unsafeArray: Array[Boolean]) extends ImmutableArray[Boolean] with Serializable {
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Boolean = unsafeArray(i)
    override def hashCode = MurmurHash3.arrayHash(unsafeArray)
    override def equals(that: Any) = that match {
      case that: ofBoolean => Arrays.equals(unsafeArray, that.unsafeArray)
      case _ => super.equals(that)
    }
  }

  final class ofUnit(val unsafeArray: Array[Unit]) extends ImmutableArray[Unit] with Serializable {
    def length: Int = unsafeArray.length
    @throws[ArrayIndexOutOfBoundsException]
    def apply(i: Int): Unit = unsafeArray(i)
    override def hashCode = MurmurHash3.arrayHash(unsafeArray)
    override def equals(that: Any) = that match {
      case that: ofUnit => unsafeArray.length == that.unsafeArray.length
      case _ => super.equals(that)
    }
  }
}
