/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime


import Predef.{Class, Throwable}
import java.lang.Runnable

/* The object <code>ScalaRunTime</code> provides ...
 */
object ScalaRunTime {

  /** Names for primitive types, used by array unboxing */
  val ByteTag = ".Byte"
  val ShortTag = ".Short"
  val CharTag = ".Char"
  val IntTag = ".Int"
  val LongTag = ".Long"
  val FloatTag = ".Float"
  val DoubleTag = ".Double"
  val BooleanTag = ".Boolean"

  def isArray(x:AnyRef): Boolean = x != null && x.getClass.isArray
  def isValueTag(tag: String) = tag.charAt(0) == '.'
  def isValueClass(clazz: Class) = clazz.isPrimitive()

  abstract class Try[a] {
    def Catch[b >: a](handler: PartialFunction[Throwable, b]): b
    def Finally(handler: Unit): a
  }

  def Try[a](block: => a): Try[a] = new Try[a] with Runnable {
    var result: a = _
    var exception: Throwable = ExceptionHandling.tryCatch(this)

    def run(): Unit = result = block

    def Catch[b >: a](handler: PartialFunction[Throwable, b]): b =
      if (exception eq null)
        result.asInstanceOf[b]
      // !!! else if (exception is LocalReturn)
      // !!!   // ...
      else if (handler isDefinedAt exception)
        handler(exception)
      else
        throw exception

    def Finally(handler: Unit): a =
      if (exception eq null)
        result.asInstanceOf[a]
      else
        throw exception
  }

  def caseFields(x: Product): List[Any] = {
    val arity = x.arity
    def fields(from: Int): List[Any] =
      if (from == arity) List()
      else x.element(from) :: fields(from + 1)
    fields(0)
  }

  def _toString(x: Product): String =
    caseFields(x).mkString(x.productPrefix + "(", ",", ")")

  def _hashCode(x: Product): Int = {
    var code = x.getClass().hashCode()
    val arr =  x.arity
    var i = 0
    while (i < arr) {
      code = code * 41 + x.element(i).hashCode()
      i = i + 1
    }
    code
  }

  def _equals(x: Product, y: Any): Boolean = y match {
    case y1: Product if x.arity == y1.arity =>
      val arity = x.arity
      var i = 0
      while (i < arity && x.element(i) == y1.element(i))
        i = i + 1
      i == arity
    case _ =>
      false
  }

  def _equalsWithVarArgs(x: Product, y: Any): Boolean = y match {
    case y1: Product if x.arity == y1.arity =>
      val arity = x.arity
      var i = 0
      while (i < arity - 1 && x.element(i) == y1.element(i))
        i = i + 1;
      i == arity - 1 && {
        x.element(i) match {
          case xs: Seq[_] =>
            y1.element(i) match {
              case ys: Seq[_] => xs sameElements ys
            }
        }
      }
    case _ =>
      false
  }

  //def checkDefined[T >: Null](x: T): T =
  //  if (x == null) throw new UndefinedException else x

  def Seq[a](xs: a*): Seq[a] = null // interpreted specially by new backend.

  def arrayValue(x: BoxedArray, elemTag: String): AnyRef =
    if (x eq null) null else x.unbox(elemTag)

  def arrayValue(x: BoxedArray, elemClass: Class): AnyRef =
    if (x eq null) null else x.unbox(elemClass)

  def boxArray(value: AnyRef): BoxedArray = value match {
    case x: Array[Byte] => new BoxedByteArray(x)
    case x: Array[Short] => new BoxedShortArray(x)
    case x: Array[Char] => new BoxedCharArray(x)
    case x: Array[Int] => new BoxedIntArray(x)
    case x: Array[Long] => new BoxedLongArray(x)
    case x: Array[Float] => new BoxedFloatArray(x)
    case x: Array[Double] => new BoxedDoubleArray(x)
    case x: Array[Boolean] => new BoxedBooleanArray(x)
    case x: Array[AnyRef] => new BoxedObjectArray(x)
    case x: BoxedArray => x
  }
}
