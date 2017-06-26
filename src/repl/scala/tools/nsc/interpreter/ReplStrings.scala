/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import scala.reflect.internal.Chars

object ReplStrings {
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

  //  /* An s-interpolator that uses `stringOf(arg)` instead of `String.valueOf(arg)`. */
  //  private[nsc] implicit class `smart stringifier`(val sc: StringContext) extends AnyVal {
  //    import StringContext.treatEscapes, scala.runtime.ScalaRunTime.stringOf
  //    def ss(args: Any*): String = sc.standardInterpolator(treatEscapes, args map stringOf)
  //  }
}
