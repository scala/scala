/*                                                                      *\
**     ________ ___   __   ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ |_|                                         **
**                                                                      **
\*                                                                      */

// $Id$


package scala.runtime


import Predef._

object StringAdd {
  // Needed for the format hack. Can be removed once we drop 1.4
  lazy val formatMethod: java.lang.reflect.Method = {
    val paramTypes = Array[Class[T] forSome { type T }](classOf[String], classOf[Array[Object]])
    classOf[String].getDeclaredMethod("format", paramTypes)
  }
}
final class StringAdd(self: Any) {

  def +(other: String) = String.valueOf(self) + other

  /** Returns string formatted according to given <code>format</code> string.
   *  Format strings are as for <code>String.format</code>
   *  (@see java.lang.String.format).
   *  Only works on Java 1.5 or higher!
   */
  def formatted(format: String): String = {
    // This should be:
    // String.format(format, Array(self.asInstanceOf[Object]))
    // However, the line above does not compile on Java 1.4 because String.format exists only in 1.5
    // Therefore, we do the following hack:
    val args = Array(self.asInstanceOf[Object])
    StringAdd.formatMethod.invoke(null, Array[Object](format, args)).asInstanceOf[String]
  }
}
