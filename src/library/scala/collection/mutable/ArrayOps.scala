/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package mutable
import compat.Platform.arraycopy

import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime._

import parallel.mutable.ParArray


/** This class serves as a wrapper for `Array`s with all the operations found in
 *  indexed sequences. Where needed, instances of arrays are implicitly converted
 *  into this class.
 *
 *  The difference between this class and `WrappedArray` is that calling transformer
 *  methods such as `filter` and `map` will yield an array, whereas a `WrappedArray`
 *  will remain a `WrappedArray`.
 *
 *  @since 2.8
 *
 *  @tparam T   type of the elements contained in this array.
 *
 *  @define Coll `ArrayOps`
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
abstract class ArrayOps[T] extends ArrayLike[T, Array[T]] with CustomParallelizable[T, ParArray[T]] {

  private def elementClass: Class[_] =
    arrayElementClass(repr.getClass)

  override def copyToArray[U >: T](xs: Array[U], start: Int, len: Int) {
    var l = math.min(len, repr.length)
    if (xs.length - start < l) l = xs.length - start max 0
    Array.copy(repr, 0, xs, start, l)
  }

  override def toArray[U >: T : ClassTag]: Array[U] = {
    val thatElementClass = arrayElementClass(implicitly[ClassTag[U]])
    if (elementClass eq thatElementClass)
      repr.asInstanceOf[Array[U]]
    else
      super.toArray[U]
  }

  override def par = ParArray.handoff(repr)

  /** Flattens a two-dimensional array by concatenating all its rows
   *  into a single array.
   *
   *  @tparam U        Type of row elements.
   *  @param asTrav    A function that converts elements of this array to rows - arrays of type `U`.
   *  @return          An array obtained by concatenating rows of this array.
   */
  def flatten[U](implicit asTrav: T => collection.Traversable[U], m: ClassTag[U]): Array[U] = {
    val b = Array.newBuilder[U]
    b.sizeHint(map{case is: collection.IndexedSeq[_] => is.size case _ => 0}.sum)
    for (xs <- this)
      b ++= asTrav(xs)
    b.result
  }

  /** Transposes a two dimensional array.
   *
   *  @tparam U       Type of row elements.
   *  @param asArray  A function that converts elements of this array to rows - arrays of type `U`.
   *  @return         An array obtained by replacing elements of this arrays with rows the represent.
   */
  def transpose[U](implicit asArray: T => Array[U]): Array[Array[U]] = {
    def mkRowBuilder() = Array.newBuilder(ClassTag[U](arrayElementClass(elementClass)))
    val bs = asArray(head) map (_ => mkRowBuilder())
    for (xs <- this) {
      var i = 0
      for (x <- asArray(xs)) {
        bs(i) += x
        i += 1
      }
    }
    val bb: Builder[Array[U], Array[Array[U]]] = Array.newBuilder(ClassTag[Array[U]](elementClass))
    for (b <- bs) bb += b.result
    bb.result
  }

  def seq = thisCollection

}

/**
 * A companion object for `ArrayOps`.
 *
 * @since 2.8
 */
object ArrayOps {

  /** A class of `ArrayOps` for arrays containing reference types. */
  class ofRef[T <: AnyRef](override val repr: Array[T]) extends ArrayOps[T] with ArrayLike[T, Array[T]] {

    override protected[this] def thisCollection: WrappedArray[T] = new WrappedArray.ofRef[T](repr)
    override protected[this] def toCollection(repr: Array[T]): WrappedArray[T] = new WrappedArray.ofRef[T](repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofRef[T]()(ClassTag[T](arrayElementClass(repr.getClass)))

    def length: Int = repr.length
    def apply(index: Int): T = repr(index)
    def update(index: Int, elem: T) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `byte`s. */
  class ofByte(override val repr: Array[Byte]) extends ArrayOps[Byte] with ArrayLike[Byte, Array[Byte]] {

    override protected[this] def thisCollection: WrappedArray[Byte] = new WrappedArray.ofByte(repr)
    override protected[this] def toCollection(repr: Array[Byte]): WrappedArray[Byte] = new WrappedArray.ofByte(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofByte

    def length: Int = repr.length
    def apply(index: Int): Byte = repr(index)
    def update(index: Int, elem: Byte) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `short`s. */
  class ofShort(override val repr: Array[Short]) extends ArrayOps[Short] with ArrayLike[Short, Array[Short]] {

    override protected[this] def thisCollection: WrappedArray[Short] = new WrappedArray.ofShort(repr)
    override protected[this] def toCollection(repr: Array[Short]): WrappedArray[Short] = new WrappedArray.ofShort(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofShort

    def length: Int = repr.length
    def apply(index: Int): Short = repr(index)
    def update(index: Int, elem: Short) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `char`s. */
  class ofChar(override val repr: Array[Char]) extends ArrayOps[Char] with ArrayLike[Char, Array[Char]] {

    override protected[this] def thisCollection: WrappedArray[Char] = new WrappedArray.ofChar(repr)
    override protected[this] def toCollection(repr: Array[Char]): WrappedArray[Char] = new WrappedArray.ofChar(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofChar

    def length: Int = repr.length
    def apply(index: Int): Char = repr(index)
    def update(index: Int, elem: Char) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `int`s. */
  class ofInt(override val repr: Array[Int]) extends ArrayOps[Int] with ArrayLike[Int, Array[Int]] {

    override protected[this] def thisCollection: WrappedArray[Int] = new WrappedArray.ofInt(repr)
    override protected[this] def toCollection(repr: Array[Int]): WrappedArray[Int] = new WrappedArray.ofInt(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofInt

    def length: Int = repr.length
    def apply(index: Int): Int = repr(index)
    def update(index: Int, elem: Int) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `long`s. */
  class ofLong(override val repr: Array[Long]) extends ArrayOps[Long] with ArrayLike[Long, Array[Long]] {

    override protected[this] def thisCollection: WrappedArray[Long] = new WrappedArray.ofLong(repr)
    override protected[this] def toCollection(repr: Array[Long]): WrappedArray[Long] = new WrappedArray.ofLong(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofLong

    def length: Int = repr.length
    def apply(index: Int): Long = repr(index)
    def update(index: Int, elem: Long) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `float`s. */
  class ofFloat(override val repr: Array[Float]) extends ArrayOps[Float] with ArrayLike[Float, Array[Float]] {

    override protected[this] def thisCollection: WrappedArray[Float] = new WrappedArray.ofFloat(repr)
    override protected[this] def toCollection(repr: Array[Float]): WrappedArray[Float] = new WrappedArray.ofFloat(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofFloat

    def length: Int = repr.length
    def apply(index: Int): Float = repr(index)
    def update(index: Int, elem: Float) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `double`s. */
  class ofDouble(override val repr: Array[Double]) extends ArrayOps[Double] with ArrayLike[Double, Array[Double]] {

    override protected[this] def thisCollection: WrappedArray[Double] = new WrappedArray.ofDouble(repr)
    override protected[this] def toCollection(repr: Array[Double]): WrappedArray[Double] = new WrappedArray.ofDouble(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofDouble

    def length: Int = repr.length
    def apply(index: Int): Double = repr(index)
    def update(index: Int, elem: Double) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `boolean`s. */
  class ofBoolean(override val repr: Array[Boolean]) extends ArrayOps[Boolean] with ArrayLike[Boolean, Array[Boolean]] {

    override protected[this] def thisCollection: WrappedArray[Boolean] = new WrappedArray.ofBoolean(repr)
    override protected[this] def toCollection(repr: Array[Boolean]): WrappedArray[Boolean] = new WrappedArray.ofBoolean(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofBoolean

    def length: Int = repr.length
    def apply(index: Int): Boolean = repr(index)
    def update(index: Int, elem: Boolean) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays of `Unit` types. */
  class ofUnit(override val repr: Array[Unit]) extends ArrayOps[Unit] with ArrayLike[Unit, Array[Unit]] {

    override protected[this] def thisCollection: WrappedArray[Unit] = new WrappedArray.ofUnit(repr)
    override protected[this] def toCollection(repr: Array[Unit]): WrappedArray[Unit] = new WrappedArray.ofUnit(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofUnit

    def length: Int = repr.length
    def apply(index: Int): Unit = repr(index)
    def update(index: Int, elem: Unit) { repr(index) = elem }
  }
}
