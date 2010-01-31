/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime

import scala.reflect.ClassManifest
import scala.collection.Seq
import scala.collection.mutable.WrappedArray
import scala.collection.immutable.{ List, Stream, Nil, :: }
import scala.util.control.ControlException

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
  def array_apply(xs: AnyRef, idx: Int): Any = xs match {
    case x: Array[AnyRef]  => x(idx).asInstanceOf[Any]
    case x: Array[Int]     => x(idx).asInstanceOf[Any]
    case x: Array[Double]  => x(idx).asInstanceOf[Any]
    case x: Array[Long]    => x(idx).asInstanceOf[Any]
    case x: Array[Float]   => x(idx).asInstanceOf[Any]
    case x: Array[Char]    => x(idx).asInstanceOf[Any]
    case x: Array[Byte]    => x(idx).asInstanceOf[Any]
    case x: Array[Short]   => x(idx).asInstanceOf[Any]
    case x: Array[Boolean] => x(idx).asInstanceOf[Any]
    case x: Array[Unit]    => x(idx).asInstanceOf[Any]
    case null => throw new NullPointerException
  }

  /** update generic array element */
  def array_update(xs: AnyRef, idx: Int, value: Any): Unit = xs match {
    case x: Array[AnyRef]  => x(idx) = value.asInstanceOf[AnyRef]
    case x: Array[Int]     => x(idx) = value.asInstanceOf[Int]
    case x: Array[Double]  => x(idx) = value.asInstanceOf[Double]
    case x: Array[Long]    => x(idx) = value.asInstanceOf[Long]
    case x: Array[Float]   => x(idx) = value.asInstanceOf[Float]
    case x: Array[Char]    => x(idx) = value.asInstanceOf[Char]
    case x: Array[Byte]    => x(idx) = value.asInstanceOf[Byte]
    case x: Array[Short]   => x(idx) = value.asInstanceOf[Short]
    case x: Array[Boolean] => x(idx) = value.asInstanceOf[Boolean]
    case x: Array[Unit]    => x(idx) = value.asInstanceOf[Unit]
    case null => throw new NullPointerException
  }

  /** Get generic array length */
  def array_length(xs: AnyRef): Int = xs match {
    case x: Array[AnyRef]  => x.length
    case x: Array[Int]     => x.length
    case x: Array[Double]  => x.length
    case x: Array[Long]    => x.length
    case x: Array[Float]   => x.length
    case x: Array[Char]    => x.length
    case x: Array[Byte]    => x.length
    case x: Array[Short]   => x.length
    case x: Array[Boolean] => x.length
    case x: Array[Unit]    => x.length
    case null => throw new NullPointerException
  }

  def array_clone(xs: AnyRef): AnyRef = xs match {
    case x: Array[AnyRef]  => ArrayRuntime.cloneArray(x)
    case x: Array[Int]     => ArrayRuntime.cloneArray(x)
    case x: Array[Double]  => ArrayRuntime.cloneArray(x)
    case x: Array[Long]    => ArrayRuntime.cloneArray(x)
    case x: Array[Float]   => ArrayRuntime.cloneArray(x)
    case x: Array[Char]    => ArrayRuntime.cloneArray(x)
    case x: Array[Byte]    => ArrayRuntime.cloneArray(x)
    case x: Array[Short]   => ArrayRuntime.cloneArray(x)
    case x: Array[Boolean] => ArrayRuntime.cloneArray(x)
    case x: Array[Unit]    => x
    case null => throw new NullPointerException
  }

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

  abstract class Try[+A] {
    def Catch[B >: A](handler: PartialFunction[Throwable, B]): B
    def Finally(fin: => Unit): A
  }

  def Try[A](block: => A): Try[A] = new Try[A] with Runnable {
    private var result: A = _
    private var exception: Throwable =
      try   { run() ; null }
      catch {
        case e: ControlException  => throw e  // don't catch non-local returns etc
        case e: Throwable         => e
      }

    def run() { result = block }

    def Catch[B >: A](handler: PartialFunction[Throwable, B]): B =
      if (exception == null) result
      else if (handler isDefinedAt exception) handler(exception)
      else throw exception

    def Finally(fin: => Unit): A = {
      fin

      if (exception == null) result
      else throw exception
    }
  }

  def _toString(x: Product): String =
    x.productIterator.mkString(x.productPrefix + "(", ",", ")")

  def _hashCodeJenkins(x: Product): Int =
    scala.util.JenkinsHash.hashSeq(x.productPrefix.toSeq ++ x.productIterator.toSeq)

  def _hashCode(x: Product): Int = {
    val arr =  x.productArity
    var code = arr
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
  def stringOf(arg: Any): String = arg match {
    case null                     => "null"
    case x: AnyRef if isArray(x)  => WrappedArray make x map stringOf mkString ("Array(", ", ", ")")
    case x: Traversable[_]        => x map stringOf mkString (x.stringPrefix + "(", ", ", ")")
    case x                        => x toString
  }
}
