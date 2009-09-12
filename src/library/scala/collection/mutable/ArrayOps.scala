/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: RichString.scala 18589 2009-08-27 14:45:35Z odersky $


package scala.collection
package mutable

abstract class ArrayOps[T] extends VectorLike[T, AnyRef /*!!!Array[T]!!!*/] {
}


object ArrayOps {

  class ofRef[T <: AnyRef](override val repr: Array[AnyRef]) extends ArrayOps[T] with VectorLike[T, Array[AnyRef]] {

    override protected[this] def thisCollection: WrappedArray[T] = new WrappedArray.ofRef[T](repr)
    override protected[this] def toCollection(repr: Array[AnyRef]): WrappedArray[T] = new WrappedArray.ofRef[T](repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofRef[T]

    def length: Int = repr.length
    def apply(index: Int): T = repr(index).asInstanceOf[T]
    def update(index: Int, elem: T) { repr(index) = elem.asInstanceOf[AnyRef] }
  }

  class ofByte(override val repr: Array[Byte]) extends ArrayOps[Byte] with VectorLike[Byte, Array[Byte]] {

    override protected[this] def thisCollection: WrappedArray[Byte] = new WrappedArray.ofByte(repr)
    override protected[this] def toCollection(repr: Array[Byte]): WrappedArray[Byte] = new WrappedArray.ofByte(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofByte

    def length: Int = repr.length
    def apply(index: Int): Byte = repr(index)
    def update(index: Int, elem: Byte) { repr(index) = elem }
  }

  class ofShort(override val repr: Array[Short]) extends ArrayOps[Short] with VectorLike[Short, Array[Short]] {

    override protected[this] def thisCollection: WrappedArray[Short] = new WrappedArray.ofShort(repr)
    override protected[this] def toCollection(repr: Array[Short]): WrappedArray[Short] = new WrappedArray.ofShort(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofShort

    def length: Int = repr.length
    def apply(index: Int): Short = repr(index)
    def update(index: Int, elem: Short) { repr(index) = elem }
  }

  class ofChar(override val repr: Array[Char]) extends ArrayOps[Char] with VectorLike[Char, Array[Char]] {

    override protected[this] def thisCollection: WrappedArray[Char] = new WrappedArray.ofChar(repr)
    override protected[this] def toCollection(repr: Array[Char]): WrappedArray[Char] = new WrappedArray.ofChar(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofChar

    def length: Int = repr.length
    def apply(index: Int): Char = repr(index)
    def update(index: Int, elem: Char) { repr(index) = elem }
  }

  class ofInt(override val repr: Array[Int]) extends ArrayOps[Int] with VectorLike[Int, Array[Int]] {

    override protected[this] def thisCollection: WrappedArray[Int] = new WrappedArray.ofInt(repr)
    override protected[this] def toCollection(repr: Array[Int]): WrappedArray[Int] = new WrappedArray.ofInt(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofInt

    def length: Int = repr.length
    def apply(index: Int): Int = repr(index)
    def update(index: Int, elem: Int) { repr(index) = elem }
  }

  class ofLong(override val repr: Array[Long]) extends ArrayOps[Long] with VectorLike[Long, Array[Long]] {

    override protected[this] def thisCollection: WrappedArray[Long] = new WrappedArray.ofLong(repr)
    override protected[this] def toCollection(repr: Array[Long]): WrappedArray[Long] = new WrappedArray.ofLong(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofLong

    def length: Int = repr.length
    def apply(index: Int): Long = repr(index)
    def update(index: Int, elem: Long) { repr(index) = elem }
  }

  class ofFloat(override val repr: Array[Float]) extends ArrayOps[Float] with VectorLike[Float, Array[Float]] {

    override protected[this] def thisCollection: WrappedArray[Float] = new WrappedArray.ofFloat(repr)
    override protected[this] def toCollection(repr: Array[Float]): WrappedArray[Float] = new WrappedArray.ofFloat(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofFloat

    def length: Int = repr.length
    def apply(index: Int): Float = repr(index)
    def update(index: Int, elem: Float) { repr(index) = elem }
  }

  class ofDouble(override val repr: Array[Double]) extends ArrayOps[Double] with VectorLike[Double, Array[Double]] {

    override protected[this] def thisCollection: WrappedArray[Double] = new WrappedArray.ofDouble(repr)
    override protected[this] def toCollection(repr: Array[Double]): WrappedArray[Double] = new WrappedArray.ofDouble(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofDouble

    def length: Int = repr.length
    def apply(index: Int): Double = repr(index)
    def update(index: Int, elem: Double) { repr(index) = elem }
  }

  class ofBoolean(override val repr: Array[Boolean]) extends ArrayOps[Boolean] with VectorLike[Boolean, Array[Boolean]] {

    override protected[this] def thisCollection: WrappedArray[Boolean] = new WrappedArray.ofBoolean(repr)
    override protected[this] def toCollection(repr: Array[Boolean]): WrappedArray[Boolean] = new WrappedArray.ofBoolean(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofBoolean

    def length: Int = repr.length
    def apply(index: Int): Boolean = repr(index)
    def update(index: Int, elem: Boolean) { repr(index) = elem }
  }

  class ofUnit(override val repr: Array[Unit]) extends ArrayOps[Unit] with VectorLike[Unit, Array[Unit]] {

    override protected[this] def thisCollection: WrappedArray[Unit] = new WrappedArray.ofUnit(repr)
    override protected[this] def toCollection(repr: Array[Unit]): WrappedArray[Unit] = new WrappedArray.ofUnit(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofUnit

    def length: Int = repr.length
    def apply(index: Int): Unit = repr(index)
    def update(index: Int, elem: Unit) { repr(index) = elem }
  }
}
