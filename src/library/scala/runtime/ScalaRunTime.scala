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
import scala.collection.Seq
import scala.collection.mutable._
import scala.collection.immutable.{ List, Stream, Nil, :: }

/* The object <code>ScalaRunTime</code> provides ...
 */
object ScalaRunTime {
  def isArray(x: AnyRef): Boolean = isArray(x, 1)
  def isArray(x: Any, atLevel: Int): Boolean =
    x != null && isArrayClass(x.asInstanceOf[AnyRef].getClass, atLevel)

  private def isArrayClass(clazz: Class[_], atLevel: Int): Boolean =
    clazz.isArray && (atLevel == 1 || isArrayClass(clazz.getComponentType, atLevel - 1))

  def isValueClass(clazz: Class[_]) = clazz.isPrimitive()

  /** Retrieve generic array element */
  def array_apply(xs: AnyRef, idx: Int): Any = java.lang.reflect.Array.get(xs, idx)

  /** update generic array element */
  def array_update(xs: AnyRef, idx: Int, value: Any): Unit = java.lang.reflect.Array.set(xs, idx, value)

  /** Get generic array length */
  def array_length(xs: AnyRef): Int = java.lang.reflect.Array.getLength(xs)

  /** Convert a numeric value array to an object array.
   *  Needed to deal with vararg arguments of primtive types that are passed
   *  to a generic Java vararg parameter T ...
   */
  def toObjectArray(src: AnyRef): Array[Object] = {
    val length = array_length(src)
    val dest = new Array[Object](length)
    for (i <- 0 until length)
      array_update(dest, i, array_apply(src, i))
    dest
  }

  def toArray[T](xs: scala.collection.Seq[T]) = {
    val arr = new Array[AnyRef](xs.length)
    var i = 0
    for (x <- xs) {
      arr(i) = x.asInstanceOf[AnyRef]
      i += 1
    }
    arr
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

  def _toString(x: Product): String =
    x.productIterator.mkString(x.productPrefix + "(", ",", ")")

  def _hashCodeJenkins(x: Product): Int =
    scala.util.JenkinsHash.hashSeq(x.productPrefix.toSeq ++ x.productIterator.toSeq)

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

  /** Fast path equality method for inlining; used when -optimise is set.
   */
  @inline def inlinedEquals(x: Object, y: Object): Boolean =
    if (x eq y) true
    else if (x eq null) false
    else if (x.isInstanceOf[java.lang.Number] || x.isInstanceOf[java.lang.Character]) BoxesRunTime.equals2(x, y)
    else x.equals(y)

  def _equals(x: Product, y: Any): Boolean = y match {
    case y: Product if x.productArity == y.productArity => x.productIterator sameElements y.productIterator
    case _                                              => false
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
    case arg: AnyRef if isArray(arg) =>
      val d: collection.IndexedSeq[Any] = WrappedArray.make(arg).deep
      d.toString
    case arg: WrappedArray[_] => arg.deep.toString
    case arg => arg.toString
  }
}
