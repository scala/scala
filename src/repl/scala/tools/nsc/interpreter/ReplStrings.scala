/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package interpreter

import scala.reflect.internal.Chars

trait ReplStrings {
  /** Convert a string into code that can recreate the string.
   *  This requires replacing all special characters by escape
   *  codes. It does not add the surrounding " marks.
   */
  def string2code(str: String): String = {
    val res = new StringBuilder
    for (c <- str) c match {
      case '"'  => res ++= """\""""
      case '\'' => res ++= """\'"""
      case '\\' => res ++= """\\"""
      case '\b' => res ++= """\b"""
      case '\t' => res ++= """\t"""
      case '\n' => res ++= """\n"""
      case '\f' => res ++= """\f"""
      case '\r' => res ++= """\r"""
      case _ if c.isControl => res ++= Chars.char2uescape(c)
      case _    => res += c
    }
    res.toString
  }

  def string2codeQuoted(str: String) =
    "\"" + string2code(str) + "\""

  def any2stringOf(x: Any, maxlen: Int) =
    "_root_.scala.runtime.ScalaRunTime.replStringOf(%s, %s)".format(x, maxlen)

  // no escaped or nested quotes
  private[this] val inquotes = """(['"])(.*?)\1""".r
  def unquoted(s: String) = s match { case inquotes(_, w) => w ; case _ => s }
  def words(s: String) = (s.trim split "\\s+" filterNot (_ == "") map unquoted).toList
}
