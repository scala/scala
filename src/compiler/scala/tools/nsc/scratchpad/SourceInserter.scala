package scala.tools.nsc
package scratchpad

import java.io.Writer
import scala.reflect.internal.util.SourceFile
import scala.reflect.internal.Chars._

@deprecated("SI-6458: Instrumentation logic will be moved out of the compiler.","2.10.0")
object SourceInserter {
  /** Strip right hand side comments from sources
   *  @param cs   The original program source
   *  @param oldcaret The caret position in the original source
   *                  a value of -1 indicates caret is at end of source
   *  @return         A pair consisting of
   *                  - The stripped source
   *                  - The new position of the caret in the stripped source
   */
  def stripRight(cs: Array[Char], oldcaret: Int = -1): (Array[Char], Int) = {
    val caret = if (oldcaret == -1) cs.length else oldcaret
    val lines = new String(cs) split "\n"
    def isContinuation(str: String) =
      ((str contains "//>") || (str contains "//|")) && (leftPart(str) forall isWhitespace)
    def leftPart(str: String) =
      (str split """//>|//\|""").head
    def stripTrailingWS(str: String) =
      str take (str lastIndexWhere (!isWhitespace(_))) + 1
    val prefixes =
      lines filterNot isContinuation map leftPart map stripTrailingWS
    val newChars =
      (prefixes mkString "\n").toArray
    val newCaret = {
      val caretLine = cs.view take caret count (_ == '\n')
      val caretCol = caret - 1 - (cs lastIndexOf ('\n', caret - 1))
      (caretCol /: (prefixes.view take caretLine)) ((n, line) => n + line.length + 1)
    }
    (newChars, newCaret)
  }
}
