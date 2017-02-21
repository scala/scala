/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import util.stringFromWriter

class Formatting(indent: Int) {

  private val indentation = " " * indent

  private def indenting(code: String): Boolean = {
    /** Heuristic to avoid indenting and thereby corrupting """-strings and XML literals. */
    val tokens = List("\"\"\"", "</", "/>")
    val noIndent = (code contains "\n") && (tokens exists code.contains)

    !noIndent
  }
  /** Indent some code by the width of the scala> prompt.
   *  This way, compiler error messages read better.
   */
  def indentCode(code: String) = stringFromWriter(str =>
    for (line <- code.lines) {
      if (indenting(code)) str print indentation
      str println line
      str.flush()
    }
  )
}
object Formatting {
  def forPrompt(prompt: String) = new Formatting(prompt.lines.toList.last.length)
}
