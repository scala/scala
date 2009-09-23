/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime
import scala.reflect.ClassManifest
import compat.Platform

/**
 * Arrays created by <code>new Array[T](length)</code> where <code>T</code>
 * is a type variable.
 *
 * @author Martin Odersky
 */
@serializable
final class BoxedAnyArray[A](val length: Int) extends BoxedArray[A] {

  def elemManifest: ClassManifest[A] = null

  private var boxed = new Array[AnyRef](length)
//  private val hash = boxed.hashCode()
  private var unboxed: AnyRef = null
  private var elemClass: Class[_] = null

  def apply(index: Int): A = synchronized {
    if (unboxed eq null)
      boxed(index)
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
  }.asInstanceOf[A]

  def update(index: Int, _elem: A): Unit = synchronized {
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

  def unbox(elemClass: Class[_]): AnyRef = synchronized {
    if (unboxed eq null) {
      this.elemClass = elemClass;
      if (elemClass eq classOf[Int]) {
        val newvalue = new Array[Int](length)
        var i = 0
        while (i < length) {
          newvalue(i) = Int.unbox(boxed(i))
          i += 1
        }
        unboxed = newvalue
      } else if (elemClass eq classOf[Double]) {
        val newvalue = new Array[Double](length)
        var i = 0
        while (i < length) {
          newvalue(i) = Double.unbox(boxed(i))
          i += 1
        }
        unboxed = newvalue;
      } else if (elemClass eq classOf[Float]) {
        val newvalue = new Array[Float](length)
        var i = 0
        while (i < length) {
          newvalue(i) = Float.unbox(boxed(i))
          i += 1
        }
        unboxed = newvalue;
      } else if (elemClass eq classOf[Long]) {
        val newvalue = new Array[Long](length)
        var i = 0
        while (i < length) {
          newvalue(i) = Long.unbox(boxed(i))
          i += 1
        }
        unboxed = newvalue;
      } else if (elemClass eq classOf[Char]) {
        val newvalue = new Array[Char](length)
        var i = 0
        while (i < length) {
          newvalue(i) = Char.unbox(boxed(i))
          i += 1
        }
        unboxed = newvalue
      } else if (elemClass eq classOf[Byte]) {
        val newvalue = new Array[Byte](length)
        var i = 0
        while (i < length) {
          newvalue(i) = Byte.unbox(boxed(i))
          i += 1
        }
        unboxed = newvalue;
      } else if (elemClass eq classOf[Short]) {
        val newvalue = new Array[Short](length)
        var i = 0
        while (i < length) {
          newvalue(i) = Short.unbox(boxed(i))
          i += 1
        }
        unboxed = newvalue;
      } else if (elemClass eq classOf[Boolean]) {
        val newvalue = new Array[Boolean](length)
        var i = 0
        while (i < length) {
          newvalue(i) = Boolean.unbox(boxed(i))
          i += 1
        }
        unboxed = newvalue;
      } else if (elemClass == classOf[AnyRef]) {
        unboxed = boxed
      } else {
        unboxed = Platform.createArray(elemClass, length)
        if (elemClass.isArray) {
          var i = 0
          while (i < length) {
            boxed(i) match {
              case ba: BoxedArray[_] => boxed(i) = ba.unbox(elemClass.getComponentType())
              case _ =>
            }
            i += 1
          }
        }
        Platform.arraycopy(boxed, 0, unboxed, 0, length)
      }
      boxed = null
    }
    unboxed
  }

  def value: AnyRef = {
    if (unboxed eq null) throw new NotDefinedError("BoxedAnyArray.value")
    unboxed
  }

  private def adapt(other: AnyRef): AnyRef =
    if (this.unboxed eq null)
      other match {
        case that: BoxedAnyArray[_] =>
          if (that.unboxed eq null) {
            that.boxed
          } else {
            if (ScalaRunTime.isValueClass(that.elemClass)) unbox(that.elemClass);
            that.unboxed
          }
        case that: BoxedArray[_] =>
          adapt(that.value)
        case that: Array[Int] =>
          unbox(classOf[Int]); that
        case that: Array[Double] =>
          unbox(classOf[Double]); that
        case that: Array[Float] =>
          unbox(classOf[Float]); that
        case that: Array[Long] =>
          unbox(classOf[Long]); that
        case that: Array[Char] =>
          unbox(classOf[Char]); that
        case that: Array[Short] =>
          unbox(classOf[Short]); that
        case that: Array[Byte] =>
          unbox(classOf[Byte]); that
        case that: Array[Boolean] =>
          unbox(classOf[Boolean]); that
        case _ =>
          other
      }
    else
      other match {
        case that: BoxedAnyArray[_] =>
          if (that.unboxed ne null) that.unboxed
          else if (ScalaRunTime.isValueClass(this.elemClass)) that.unbox(this.elemClass)
          else that.boxed
        case that: BoxedArray[_] =>
          adapt(that.value)
        case _ =>
          other
      }

  override def copyFrom(src: AnyRef, from: Int, to: Int, len: Int) {
    val src1 = adapt(src)
    Array.copy(src1, from, if (unboxed ne null) unboxed else boxed, to, len)
  }

  override def copyTo(from: Int, dest: AnyRef, to: Int, len: Int) {
    var dest1 = adapt(dest)
    Array.copy(if (unboxed ne null) unboxed else boxed, from, dest1, to, len)
  }
}
