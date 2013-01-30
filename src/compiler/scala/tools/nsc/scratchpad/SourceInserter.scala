package scala.tools.nsc
package scratchpad

import scala.reflect.internal.Chars._

@deprecated("SI-6458: Instrumentation logic will be moved out of the compiler.","2.10.0")
object SourceInserter {
  def stripRight(cs: Array[Char]): Array[Char] = {
    val lines =
      new String(cs) split "\n"
    def leftPart(str: String) =
      (str split """//>|//\|""").head
    def isContinuation(str: String) =
      ((str contains "//>") || (str contains "//|")) && (leftPart(str) forall isWhitespace)
    def stripTrailingWS(str: String) =
      str take (str lastIndexWhere (!isWhitespace(_))) + 1
    val prefixes =
      lines filterNot isContinuation map leftPart map stripTrailingWS
    (prefixes mkString "\n").toArray
  }
}
