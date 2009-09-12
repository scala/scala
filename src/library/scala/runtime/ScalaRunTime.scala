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
import scala.collection.Sequence
import scala.collection.mutable._

/* The object <code>ScalaRunTime</code> provides ...
 */
object ScalaRunTime {

  def isArray(x: AnyRef): Boolean = x != null && (x.getClass.isArray || x.isInstanceOf[BoxedArray[_]])
  def isValueClass(clazz: Class[_]) = clazz.isPrimitive()

  // todo: [for Gilles] replace with boxArray
  def forceBoxedArray[A <: Any](xs: Seq[A]): Array[A] = {
    val array = new Array[A](xs.length)
    var i = 0
    for (x <- xs.iterator) { array(i) = x; i += 1 }
    array
  }

  /** Get generic array element */
  def getArrayElem(xs: AnyRef, idx: Int): Any = java.lang.reflect.Array.get(xs, idx)

  /** Get generic array element */
  def setArrayElem(xs: AnyRef, idx: Int, value: Any): Unit = java.lang.reflect.Array.set(xs, idx, value)

  def toArray[T](xs: scala.collection.Sequence[T]) = {
    val arr = new Array[AnyRef](xs.length)
    var i = 0
    for (x <- xs) arr(i) = x.asInstanceOf[AnyRef]
    arr
  }

  /** Convert arrays to sequences, leave sequences as they are
   *  !!! see duplication wrt
   */
  def toSequence[T](xs: AnyRef): Sequence[T] = xs match {
    case ts: Sequence[T] => ts.asInstanceOf[Sequence[T]]
    case null => null
    case x: Array[AnyRef] => new WrappedArray.ofRef(x).asInstanceOf[Array[T]]
    case x: Array[Int] => new WrappedArray.ofInt(x).asInstanceOf[Array[T]]
    case x: Array[Double] => new WrappedArray.ofDouble(x).asInstanceOf[Array[T]]
    case x: Array[Long] => new WrappedArray.ofLong(x).asInstanceOf[Array[T]]
    case x: Array[Float] => new WrappedArray.ofFloat(x).asInstanceOf[Array[T]]
    case x: Array[Char] => new WrappedArray.ofChar(x).asInstanceOf[Array[T]]
    case x: Array[Byte] => new WrappedArray.ofByte(x).asInstanceOf[Array[T]]
    case x: Array[Short] => new WrappedArray.ofShort(x).asInstanceOf[Array[T]]
    case x: Array[Boolean] => new WrappedArray.ofBoolean(x).asInstanceOf[Array[T]]
  }

  def checkInitialized[T <: AnyRef](x: T): T =
    if (x == null) throw new UninitializedError else x

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
    val arity = x.productArity
    def fields(from: Int): List[Any] =
      if (from == arity) List()
      else x.productElement(from) :: fields(from + 1)
    fields(0)
  }

  def _toString(x: Product): String =
    caseFields(x).mkString(x.productPrefix + "(", ",", ")")

  def _hashCode(x: Product): Int = {
    var code = x.productPrefix.hashCode()
    val arr =  x.productArity
    var i = 0
    while (i < arr) {
      val elem = x.productElement(i)
      code = code * 41 + (if (elem == null) 0 else elem.hashCode())
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

  def Seq[a](xs: a*): Seq[a] = null // interpreted specially by new backend.

  def arrayValue[A](x: BoxedArray[A], elemClass: Class[_]): AnyRef =
    if (x eq null) null else x.unbox(elemClass)

  /** Temporary method to go to new array representation
   *  !!! can be reomved once bootstrap is complete !!!
   */
  def unboxedArray[A](x: AnyRef): AnyRef = x match {
    case ba: BoxedArray[_] => ba.value
    case _ => x
  }

  def boxArray(value: AnyRef): BoxedArray[_] = value match {
    case x: Array[AnyRef] => new BoxedObjectArray(x, ClassManifest.classType(x.getClass.getComponentType))
    case x: Array[Int] => new BoxedIntArray(x)
    case x: Array[Double] => new BoxedDoubleArray(x)
    case x: Array[Long] => new BoxedLongArray(x)
    case x: Array[Float] => new BoxedFloatArray(x)
    case x: Array[Char] => new BoxedCharArray(x)
    case x: Array[Byte] => new BoxedByteArray(x)
    case x: Array[Short] => new BoxedShortArray(x)
    case x: Array[Boolean] => new BoxedBooleanArray(x)
    case x: BoxedArray[_] => x
    case null => null
  }

  /** Given any Scala value, convert it to a String.
   *
   * The primary motivation for this method is to provide a means for
   * correctly obtaining a String representation of a value, while
   * avoiding the pitfalls of naïvely calling toString on said value.
   * In particular, it addresses the fact that (a) toString cannot be
   * called on null and (b) depending on the apparent type of an
   * array, toString may or may not print it in a human-readable form.
   *
   * @param arg the value to stringify
   * @return a string representation of <code>arg</code>
   *
   */
  def stringOf(arg : Any): String = arg match {
    case null => "null"
    case (arg : AnyRef) if isArray(arg) => boxArray(arg).deepToString
    case arg => arg.toString
  }
}
