/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

/** If it looks like they're pasting in a scala interpreter
 *  transcript, remove all the formatting we inserted so we
 *  can make some sense of it.
 */
abstract class Pasted {
  def ContinueString: String
  def PromptString: String
  def interpret(line: String): Unit

  private def matchesString(line: String, target: String): Boolean = (
    (line startsWith target) ||
    (line.nonEmpty && " \t".toSet(line.head) && matchesString(line.tail, target))
  )
  private def stripString(line: String, target: String) = line indexOf target match {
    case -1   => line
    case idx  => line drop (idx + target.length)
  }

  def matchesPrompt(line: String) = matchesString(line, PromptString)
  def matchesContinue(line: String) = matchesString(line, ContinueString)

  private var isRunning = false
  def running = isRunning

  /** Commands start on lines beginning with "scala>" and each successive
   *  line which begins with the continuation string is appended to that command.
   *  Everything else is discarded.  When the end of the transcript is spotted,
   *  all the commands are replayed.
   */
  def apply(lines: TraversableOnce[String]) = {
    val cmds = lines reduceLeft append split PromptString filterNot (_.trim == "") toList;
    println("// Replaying %d commands from transcript.\n" format cmds.size)

    isRunning = true
    try cmds foreach interpret
    finally isRunning = false
  }

  private def append(code: String, line: String): String =
    if (matchesPrompt(line)) code + "\n" + line
    else if (matchesContinue(line)) code + "\n" + stripString(line, ContinueString)
    else fixResRefs(code, line)

  /** If the line looks like
   *    res15: Int
   *  then we go back in time to the preceding scala> prompt and rewrite
   *  the line containing <expr> as
   *    val res15 = { <expr> }
   *  and the rest as they say is rewritten history.
   *
   *  In all other cases, discard the line.
   */
  private val resRegex = """^\s*(res\d+):.*""".r
  private def fixResRefs(code: String, line: String) = line match {
    case resRegex(resName) if code contains PromptString =>
      val (str1, str2) = code splitAt code.lastIndexOf(PromptString) + PromptString.length
      "%sval %s = { %s }".format(str1, resName, str2)
    case _ => code
  }
}
