/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

final class RichShort(val self: Short) extends ScalaWholeNumberProxy[Short] {
  override def isValidByte = self.toByte.toInt == self.toInt
  override def isValidShort = true
  override def isValidChar = self.toChar.toInt == self.toInt
  override def isValidInt = true
  override def isValidLong = true
  override def isValidFloat = true
  override def isValidDouble = true
}
