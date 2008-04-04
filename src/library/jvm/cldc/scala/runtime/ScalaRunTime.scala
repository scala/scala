/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime


import Predef._

/* The object <code>ScalaRunTime</code> provides ...
 */
object ScalaRunTime {

  /** Names for primitive types, used by array unboxing */
  val ByteTag = ".Byte"
  val ShortTag = ".Short"
  val CharTag = ".Char"
  val IntTag = ".Int"
  val LongTag = ".Long"
  val BooleanTag = ".Boolean"

  def isArray(x: AnyRef): Boolean =
    (x != null && x.getClass.isArray) || (x != null && x.isInstanceOf[BoxedArray])

  def isValueTag(tag: String) = tag.charAt(0) == '.'

  def isValueClass(clazz: Class[_]) =
    clazz == classOf[Boolean] || clazz == classOf[Byte] ||
    clazz == classOf[Short  ] || clazz == classOf[Char] ||
    clazz == classOf[Int    ] || clazz == classOf[Long]

  def checkInitialized[T <: AnyRef](x: T): T =
    if (x == null) throw new UninitializedError else x

  abstract class Try[A] {
    def Catch[B >: A](handler: PartialFunction[Throwable, B]): B
    def Finally(handler: Unit): A
  }

  def Try[A](block: => A): Try[A] = new Try[A] with Runnable {
    var result: A = _
    var exception: Throwable = ExceptionHandling.tryCatch(this)

    def run() { result = block }

    def Catch[B >: A](handler: PartialFunction[Throwable, B]): B =
      if (exception eq null)
        result.asInstanceOf[B]
      // !!! else if (exception is LocalReturn)
      // !!!   // ...
      else if (handler isDefinedAt exception)
        handler(exception)
      else
        throw exception

    def Finally(handler: Unit): A =
      if (exception eq null)
        result.asInstanceOf[A]
      else
        throw exception
  }

  def caseFields(x: Product): List[Any] = {
    val arity = x.productArity
    def fields(from: Int): List[Any] =
      if (from == arity) List()
      else x.productElement(from) :: fields(from + 1)
    fields(0)
  }

  def _toString(x: Product): String =
    caseFields(x).mkString(x.productPrefix + "(", ",", ")")

  def _hashCode(x: Product): Int = {
    var code = x.getClass().hashCode()
    val arr =  x.productArity
    var i = 0
    while (i < arr) {
      code = code * 41 + x.productElement(i).hashCode()
      i += 1
    }
    code
  }

  def _equals(x: Product, y: Any): Boolean = y match {
    case y1: Product if x.productArity == y1.productArity =>
      val arity = x.productArity
      var i = 0
      while (i < arity && x.productElement(i) == y1.productElement(i))
        i += 1
      i == arity
    case _ =>
      false
  }

  def _equalsWithVarArgs(x: Product, y: Any): Boolean = y match {
    case y1: Product if x.productArity == y1.productArity =>
      val arity = x.productArity
      var i = 0
      while (i < arity - 1 && x.productElement(i) == y1.productElement(i))
        i += 1
      i == arity - 1 && {
        x.productElement(i) match {
          case xs: Seq[_] =>
            y1.productElement(i) match {
              case ys: Seq[_] => xs sameElements ys
            }
        }
      }
    case _ =>
      false
  }

  //def checkDefined[T >: Null](x: T): T =
  //  if (x == null) throw new UndefinedException else x

  def Seq[A](xs: A*): Seq[A] = null // interpreted specially by new backend.

  def arrayValue(x: BoxedArray, elemTag: String): AnyRef =
    if (x eq null) null else x.unbox(elemTag)

  def arrayValue(x: BoxedArray, elemClass: Class[_]): AnyRef =
    if (x eq null) null else x.unbox(elemClass)

  def boxArray(value: AnyRef): BoxedArray = value match {
    case x: Array[Byte] => new BoxedByteArray(x)
    case x: Array[Short] => new BoxedShortArray(x)
    case x: Array[Char] => new BoxedCharArray(x)
    case x: Array[Int] => new BoxedIntArray(x)
    case x: Array[Long] => new BoxedLongArray(x)
    case x: Array[Boolean] => new BoxedBooleanArray(x)
    case x: Array[AnyRef] => new BoxedObjectArray(x)
    case x: BoxedArray => x
  }
}
