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
 *  @define Coll `Array`
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
sealed trait ArrayOps[T] extends Any with ArrayLike[T, Array[T]] with CustomParallelizable[T, ParArray[T]] {

  private def elementClass: Class[_] =
    repr.getClass.getComponentType

  override def copyToArray[U >: T](xs: Array[U], start: Int, len: Int) {
    val l = len min repr.length min (xs.length - start)
    if (l > 0) Array.copy(repr, 0, xs, start, l)
  }

  override def slice(from: Int, until: Int): Array[T] = {
     val lo = math.max(from, 0)
     val hi = math.min(math.max(until, 0), repr.length)
     val size = math.max(hi - lo, 0)
     val result = java.lang.reflect.Array.newInstance(elementClass, size)
     if (size > 0) {
      Array.copy(repr, lo, result, 0, size)
     }
     result.asInstanceOf[Array[T]]
  }

  override def toArray[U >: T : ClassTag]: Array[U] = {
    val thatElementClass = implicitly[ClassTag[U]].runtimeClass
    if (elementClass eq thatElementClass)
      repr.asInstanceOf[Array[U]]
    else
      super.toArray[U]
  }

  def :+[B >: T: ClassTag](elem: B): Array[B] = {
    val result = Array.ofDim[B](repr.length + 1)
    Array.copy(repr, 0, result, 0, repr.length)
    result(repr.length) = elem
    result
  }

  def +:[B >: T: ClassTag](elem: B): Array[B] = {
    val result = Array.ofDim[B](repr.length + 1)
    result(0) = elem
    Array.copy(repr, 0, result, 1, repr.length)
    result
  }

  override def par = ParArray.handoff(repr)

  /** Flattens a two-dimensional array by concatenating all its rows
   *  into a single array.
   *
   *  @tparam U        Type of row elements.
   *  @param asTrav    A function that converts elements of this array to rows - arrays of type `U`.
   *  @return          An array obtained by concatenating rows of this array.
   */
  def flatten[U](implicit asTrav: T => scala.collection.Traversable[U], m: ClassTag[U]): Array[U] = {
    val b = Array.newBuilder[U]
    b.sizeHint(map{case is: scala.collection.IndexedSeq[_] => is.size case _ => 0}.sum)
    for (xs <- this)
      b ++= asTrav(xs)
    b.result()
  }

  /** Transposes a two dimensional array.
   *
   *  @tparam U       Type of row elements.
   *  @param asArray  A function that converts elements of this array to rows - arrays of type `U`.
   *  @return         An array obtained by replacing elements of this arrays with rows the represent.
   */
  def transpose[U](implicit asArray: T => Array[U]): Array[Array[U]] = {
    val bb: Builder[Array[U], Array[Array[U]]] = Array.newBuilder(ClassTag[Array[U]](elementClass))
    if (isEmpty) bb.result()
    else {
      def mkRowBuilder() = Array.newBuilder(ClassTag[U](elementClass.getComponentType))
      val bs = asArray(head) map (_ => mkRowBuilder())
      for (xs <- this) {
        var i = 0
        for (x <- asArray(xs)) {
          bs(i) += x
          i += 1
        }
      }
      for (b <- bs) bb += b.result()
      bb.result()
    }
  }

  /** Converts an array of pairs into an array of first elements and an array of second elements.
   *
   *  @tparam T1    the type of the first half of the element pairs
   *  @tparam T2    the type of the second half of the element pairs
   *  @param asPair an implicit conversion which asserts that the element type
   *                of this Array is a pair.
   *  @param ct1    a class tag for T1 type parameter that is required to create an instance
   *                of Array[T1]
   *  @param ct2    a class tag for T2 type parameter that is required to create an instance
   *                of Array[T2]
   *  @return       a pair of Arrays, containing, respectively, the first and second half
   *                of each element pair of this Array.
   */
  // implementation NOTE: ct1 and ct2 can't be written as context bounds because desugared
  // implicits are put in front of asPair parameter that is supposed to guide type inference
  def unzip[T1, T2](implicit asPair: T => (T1, T2), ct1: ClassTag[T1], ct2: ClassTag[T2]): (Array[T1], Array[T2]) = {
    val a1 = new Array[T1](length)
    val a2 = new Array[T2](length)
    var i = 0
    while (i < length) {
      val e = apply(i)
      a1(i) = e._1
      a2(i) = e._2
      i += 1
    }
    (a1, a2)
  }

  /** Converts an array of triples into three arrays, one containing the elements from each position of the triple.
   *
   *  @tparam T1      the type of the first of three elements in the triple
   *  @tparam T2      the type of the second of three elements in the triple
   *  @tparam T3      the type of the third of three elements in the triple
   *  @param asTriple an implicit conversion which asserts that the element type
   *                  of this Array is a triple.
   *  @param ct1    a class tag for T1 type parameter that is required to create an instance
   *                of Array[T1]
   *  @param ct2    a class tag for T2 type parameter that is required to create an instance
   *                of Array[T2]
   *  @param ct3    a class tag for T3 type parameter that is required to create an instance
   *                of Array[T3]
   *  @return         a triple of Arrays, containing, respectively, the first, second, and third
   *                  elements from each element triple of this Array.
   */
  // implementation NOTE: ct1, ct2, ct3 can't be written as context bounds because desugared
  // implicits are put in front of asPair parameter that is supposed to guide type inference
  def unzip3[T1, T2, T3](implicit asTriple: T => (T1, T2, T3), ct1: ClassTag[T1], ct2: ClassTag[T2],
    ct3: ClassTag[T3]): (Array[T1], Array[T2], Array[T3]) = {
    val a1 = new Array[T1](length)
    val a2 = new Array[T2](length)
    val a3 = new Array[T3](length)
    var i = 0
    while (i < length) {
      val e = apply(i)
      a1(i) = e._1
      a2(i) = e._2
      a3(i) = e._3
      i += 1
    }
    (a1, a2, a3)
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
  final class ofRef[T <: AnyRef](override val repr: Array[T]) extends AnyVal with ArrayOps[T] with ArrayLike[T, Array[T]] {

    override protected[this] def thisCollection: WrappedArray[T] = new WrappedArray.ofRef[T](repr)
    override protected[this] def toCollection(repr: Array[T]): WrappedArray[T] = new WrappedArray.ofRef[T](repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofRef[T]()(ClassTag[T](repr.getClass.getComponentType))

    def length: Int = repr.length
    def apply(index: Int): T = repr(index)
    def update(index: Int, elem: T) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `byte`s. */
final class ofByte(override val repr: Array[Byte]) extends AnyVal with ArrayOps[Byte] with ArrayLike[Byte, Array[Byte]] {

    override protected[this] def thisCollection: WrappedArray[Byte] = new WrappedArray.ofByte(repr)
    override protected[this] def toCollection(repr: Array[Byte]): WrappedArray[Byte] = new WrappedArray.ofByte(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofByte

    def length: Int = repr.length
    def apply(index: Int): Byte = repr(index)
    def update(index: Int, elem: Byte) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `short`s. */
final class ofShort(override val repr: Array[Short]) extends AnyVal with ArrayOps[Short] with ArrayLike[Short, Array[Short]] {

    override protected[this] def thisCollection: WrappedArray[Short] = new WrappedArray.ofShort(repr)
    override protected[this] def toCollection(repr: Array[Short]): WrappedArray[Short] = new WrappedArray.ofShort(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofShort

    def length: Int = repr.length
    def apply(index: Int): Short = repr(index)
    def update(index: Int, elem: Short) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `char`s. */
final class ofChar(override val repr: Array[Char]) extends AnyVal with ArrayOps[Char] with ArrayLike[Char, Array[Char]] {

    override protected[this] def thisCollection: WrappedArray[Char] = new WrappedArray.ofChar(repr)
    override protected[this] def toCollection(repr: Array[Char]): WrappedArray[Char] = new WrappedArray.ofChar(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofChar

    def length: Int = repr.length
    def apply(index: Int): Char = repr(index)
    def update(index: Int, elem: Char) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `int`s. */
final class ofInt(override val repr: Array[Int]) extends AnyVal with ArrayOps[Int] with ArrayLike[Int, Array[Int]] {

    override protected[this] def thisCollection: WrappedArray[Int] = new WrappedArray.ofInt(repr)
    override protected[this] def toCollection(repr: Array[Int]): WrappedArray[Int] = new WrappedArray.ofInt(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofInt

    def length: Int = repr.length
    def apply(index: Int): Int = repr(index)
    def update(index: Int, elem: Int) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `long`s. */
final class ofLong(override val repr: Array[Long]) extends AnyVal with ArrayOps[Long] with ArrayLike[Long, Array[Long]] {

    override protected[this] def thisCollection: WrappedArray[Long] = new WrappedArray.ofLong(repr)
    override protected[this] def toCollection(repr: Array[Long]): WrappedArray[Long] = new WrappedArray.ofLong(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofLong

    def length: Int = repr.length
    def apply(index: Int): Long = repr(index)
    def update(index: Int, elem: Long) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `float`s. */
final class ofFloat(override val repr: Array[Float]) extends AnyVal with ArrayOps[Float] with ArrayLike[Float, Array[Float]] {

    override protected[this] def thisCollection: WrappedArray[Float] = new WrappedArray.ofFloat(repr)
    override protected[this] def toCollection(repr: Array[Float]): WrappedArray[Float] = new WrappedArray.ofFloat(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofFloat

    def length: Int = repr.length
    def apply(index: Int): Float = repr(index)
    def update(index: Int, elem: Float) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `double`s. */
final class ofDouble(override val repr: Array[Double]) extends AnyVal with ArrayOps[Double] with ArrayLike[Double, Array[Double]] {

    override protected[this] def thisCollection: WrappedArray[Double] = new WrappedArray.ofDouble(repr)
    override protected[this] def toCollection(repr: Array[Double]): WrappedArray[Double] = new WrappedArray.ofDouble(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofDouble

    def length: Int = repr.length
    def apply(index: Int): Double = repr(index)
    def update(index: Int, elem: Double) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `boolean`s. */
final class ofBoolean(override val repr: Array[Boolean]) extends AnyVal with ArrayOps[Boolean] with ArrayLike[Boolean, Array[Boolean]] {

    override protected[this] def thisCollection: WrappedArray[Boolean] = new WrappedArray.ofBoolean(repr)
    override protected[this] def toCollection(repr: Array[Boolean]): WrappedArray[Boolean] = new WrappedArray.ofBoolean(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofBoolean

    def length: Int = repr.length
    def apply(index: Int): Boolean = repr(index)
    def update(index: Int, elem: Boolean) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays of `Unit` types. */
final class ofUnit(override val repr: Array[Unit]) extends AnyVal with ArrayOps[Unit] with ArrayLike[Unit, Array[Unit]] {

    override protected[this] def thisCollection: WrappedArray[Unit] = new WrappedArray.ofUnit(repr)
    override protected[this] def toCollection(repr: Array[Unit]): WrappedArray[Unit] = new WrappedArray.ofUnit(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofUnit

    def length: Int = repr.length
    def apply(index: Int): Unit = repr(index)
    def update(index: Int, elem: Unit) { repr(index) = elem }
  }
}
