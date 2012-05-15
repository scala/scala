/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import scala.collection.{ mutable, immutable }
import scala.PartialFunction.cond
import scala.reflect.internal.Chars

trait ReplStrings {
  /** Convert a string into code that can recreate the string.
   *  This requires replacing all special characters by escape
   *  codes. It does not add the surrounding " marks.  */
  def string2code(str: String): String = {
    val res = new StringBuilder
    for (c <- str) c match {
      case '"' | '\'' | '\\'  => res += '\\' ; res += c
      case _ if c.isControl   => res ++= Chars.char2uescape(c)
      case _                  => res += c
    }
    res.toString
  }

  def string2codeQuoted(str: String) =
    "\"" + string2code(str) + "\""

  def any2stringOf(x: Any, maxlen: Int) =
    "scala.runtime.ScalaRunTime.replStringOf(%s, %s)".format(x, maxlen)

  def words(s: String) = s.trim split "\\s+" filterNot (_ == "") toList
  def isQuoted(s: String) = (s.length >= 2) && (s.head == s.last) && ("\"'" contains s.head)
}
