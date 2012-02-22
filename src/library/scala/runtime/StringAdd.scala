/*                                                                      *\
**     ________ ___   __   ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ |_|                                         **
**                                                                      **
\*                                                                      */

package scala.runtime

/** A wrapper class that adds string concatenation `+` to any value */
final class StringAdd(val self: Any) {

  // Note: The implicit conversion from Any to StringAdd is one of two
  // implicit conversions from Any to AnyRef in Predef. It is important to have at least
  // two such conversions, so that silent conversions from value types to AnyRef
  // are avoided. If StringFormat should become a value class, another
  // implicit conversion from Any to AnyRef has to be introduced in Predef

  def +(other: String) = String.valueOf(self) + other

}
