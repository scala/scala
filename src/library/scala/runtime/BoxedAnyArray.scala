/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime


import Predef._
import compat.Platform

/**
 * Arrays created by <code>new Array[T](length)</code> where <code>T</code>
 * is a type variable.
 */
@serializable
final class BoxedAnyArray(val length: Int) extends BoxedArray {

  private var boxed = new Array[AnyRef](length)
  private val hash = boxed.hashCode()
  private var unboxed: AnyRef = null
  private var elemClass: Class = null

  def apply(index: Int): Any = synchronized {
    if (unboxed eq null)
      boxed(index);
    else if (elemClass eq classOf[Int])
      Int.box(unboxed.asInstanceOf[Array[Int]](index))
    else if (elemClass eq classOf[Double])
      Double.box(unboxed.asInstanceOf[Array[Double]](index))
    else if (elemClass eq classOf[Float])
      Float.box(unboxed.asInstanceOf[Array[Float]](index))
    else if (elemClass eq classOf[Long])
      Long.box(unboxed.asInstanceOf[Array[Long]](index))
    else if (elemClass eq classOf[Char])
      Char.box(unboxed.asInstanceOf[Array[Char]](index))
    else if (elemClass eq classOf[Byte])
      Byte.box(unboxed.asInstanceOf[Array[Byte]](index))
    else if (elemClass eq classOf[Short])
      Short.box(unboxed.asInstanceOf[Array[Short]](index))
    else if (elemClass eq classOf[Boolean])
      Boolean.box(unboxed.asInstanceOf[Array[Boolean]](index))
    else
      unboxed.asInstanceOf[Array[AnyRef]](index)
  }

  def update(index: Int, _elem: Any): Unit = synchronized {
    val elem = _elem.asInstanceOf[AnyRef]
    if (unboxed eq null)
      boxed(index) = elem
    else if (elemClass eq classOf[Int])
      unboxed.asInstanceOf[Array[Int]](index) = Int.unbox(elem)
    else if (elemClass eq classOf[Double])
      unboxed.asInstanceOf[Array[Double]](index) = Double.unbox(elem)
    else if (elemClass eq classOf[Float])
      unboxed.asInstanceOf[Array[Float]](index) = Float.unbox(elem)
    else if (elemClass eq classOf[Long])
      unboxed.asInstanceOf[Array[Long]](index) = Long.unbox(elem)
    else if (elemClass eq classOf[Char])
      unboxed.asInstanceOf[Array[Char]](index) = Char.unbox(elem)
    else if (elemClass eq classOf[Byte])
      unboxed.asInstanceOf[Array[Byte]](index) = Byte.unbox(elem)
    else if (elemClass eq classOf[Short])
      unboxed.asInstanceOf[Array[Short]](index) = Short.unbox(elem)
    else if (elemClass eq classOf[Boolean])
      unboxed.asInstanceOf[Array[Boolean]](index) = Boolean.unbox(elem)
    else
      unboxed.asInstanceOf[Array[AnyRef]](index) = elem
  }

  def unbox(elemTag: String): AnyRef =
    if (elemTag eq ScalaRunTime.IntTag) unbox(classOf[Int])
    else if (elemTag eq ScalaRunTime.DoubleTag) unbox(classOf[Double])
    else if (elemTag eq ScalaRunTime.FloatTag) unbox(classOf[Float])
    else if (elemTag eq ScalaRunTime.LongTag) unbox(classOf[Long])
    else if (elemTag eq ScalaRunTime.CharTag) unbox(classOf[Char])
    else if (elemTag eq ScalaRunTime.ByteTag) unbox(classOf[Byte])
    else if (elemTag eq ScalaRunTime.ShortTag) unbox(classOf[Short])
    else if (elemTag eq ScalaRunTime.BooleanTag) unbox(classOf[Boolean])
    else unbox(Platform.getClassForName(elemTag))

  def unbox(elemClass: Class): AnyRef = synchronized {
    if (unboxed eq null) {
      this.elemClass = elemClass;
      if (elemClass eq classOf[Int]) {
	val newvalue = new Array[Int](length)
	var i = 0
	while (i < length) {
	  newvalue(i) = Int.unbox(boxed(i))
	  i = i + 1
	}
	unboxed = newvalue
      } else if (elemClass eq classOf[Double]) {
	val newvalue = new Array[Double](length)
	var i = 0
	while (i < length) {
	  newvalue(i) = Double.unbox(boxed(i))
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemClass eq classOf[Float]) {
	val newvalue = new Array[Float](length)
	var i = 0
	while (i < length) {
	  newvalue(i) = Float.unbox(boxed(i))
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemClass eq classOf[Long]) {
	val newvalue = new Array[Long](length)
	var i = 0
	while (i < length) {
	  newvalue(i) = Long.unbox(boxed(i))
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemClass eq classOf[Char]) {
	val newvalue = new Array[Char](length)
	var i = 0
	while (i < length) {
	  newvalue(i) = Char.unbox(boxed(i))
	  i = i + 1
	}
	unboxed = newvalue
      } else if (elemClass eq classOf[Byte]) {
	val newvalue = new Array[Byte](length)
	var i = 0
	while (i < length) {
	  newvalue(i) = Byte.unbox(boxed(i))
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemClass eq classOf[Short]) {
	val newvalue = new Array[Short](length)
	var i = 0
	while (i < length) {
	  newvalue(i) = Short.unbox(boxed(i))
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemClass eq classOf[Boolean]) {
	val newvalue = new Array[Boolean](length)
	var i = 0
	while (i < length) {
	  newvalue(i) = Boolean.unbox(boxed(i))
	  i = i + 1
	}
	unboxed = newvalue;
      } else if (elemClass == boxed.getClass().getComponentType()) {
        // todo: replace with ScalaRunTime.AnyRef.class
	unboxed = boxed
      } else {
	unboxed = Platform.createArray(elemClass, length);
	Platform.arraycopy(boxed, 0, unboxed, 0, length);
      }
      boxed = null
    }
    unboxed
  }

  override def equals(other: Any): Boolean = (
    other.isInstanceOf[BoxedAnyArray] && (this eq (other.asInstanceOf[BoxedAnyArray])) ||
    (if (unboxed eq null) boxed == other else unboxed == other)
  )

  override def hashCode(): Int = hash

  def value: AnyRef = {
    if (unboxed eq null) throw new NotDefinedError("BoxedAnyArray.value")
    unboxed
  }

  private def adapt(other: AnyRef): AnyRef =
    if (this.unboxed eq null)
      other match {
        case that: BoxedAnyArray =>
          if (that.unboxed eq null) {
            that.boxed
          } else {
            if (ScalaRunTime.isValueClass(that.elemClass)) unbox(that.elemClass);
            that.unboxed
          }
        case that: BoxedArray =>
          adapt(that.value)
        case that: Array[Int] =>
          unbox(ScalaRunTime.IntTag); that
        case that: Array[Double] =>
          unbox(ScalaRunTime.DoubleTag); that
        case that: Array[Float] =>
          unbox(ScalaRunTime.FloatTag); that
        case that: Array[Long] =>
          unbox(ScalaRunTime.LongTag); that
        case that: Array[Char] =>
          unbox(ScalaRunTime.CharTag); that
        case that: Array[Short] =>
          unbox(ScalaRunTime.ShortTag); that
        case that: Array[Byte] =>
          unbox(ScalaRunTime.ByteTag); that
        case that: Array[Boolean] =>
          unbox(ScalaRunTime.BooleanTag); that
        case _ =>
          other
      }
    else
      other match {
        case that: BoxedAnyArray =>
          if (that.unboxed ne null) that.unboxed
          else if (ScalaRunTime.isValueClass(this.elemClass)) that.unbox(this.elemClass)
          else that.boxed
        case that: BoxedArray =>
          adapt(that.value)
        case _ =>
          other
      }

  override def copyFrom(src: AnyRef, from: Int, to: Int, len: Int): Unit = {
    val src1 = adapt(src)
    Array.copy(src1, from, if (unboxed ne null) unboxed else boxed, to, len)
  }

  override def copyTo(from: Int, dest: AnyRef, to: Int, len: Int): Unit = {
    var dest1 = adapt(dest)
    Array.copy(if (unboxed ne null) unboxed else boxed, from, dest1, to, len)
  }

  override def subArray(start: Int, end: Int): AnyRef = {
    val result = new BoxedAnyArray(end - start);
    Array.copy(this, start, result, 0, end - start)
    result
  }

  final override def filter(p: Any => Boolean): BoxedArray = {
    val include = new Array[Boolean](length)
    var len = 0
    var i = 0
    while (i < length) {
      if (p(this(i))) { include(i) = true; len = len + 1 }
      i = i + 1
    }
    val result = new BoxedAnyArray(len)
    len = 0
    i = 0
    while (len < result.length) {
      if (include(i)) { result(len) = this(i); len = len + 1 }
      i = i + 1
    }
    result
  }

  final override def slice(start: Int, end: Int): BoxedArray = {
    val len = end - start
    val result = new BoxedAnyArray(len)
    Array.copy(this, start, result, 0, len)
    result
  }

}
