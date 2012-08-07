/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

/** If it looks like they're pasting in a scala interpreter
 *  transcript, remove all the formatting we inserted so we
 *  can make some sense of it.
 *
 *  Most of the interesting code in here is due to my goal of
 *  "paste idempotence" i.e. the transcript resulting from pasting
 *  a transcript should itself be pasteable and should achieve
 *  the same result.
 */
abstract class Pasted {
  def ContinueString: String
  def PromptString: String
  def interpret(line: String): Unit

  def matchesPrompt(line: String) = matchesString(line, PromptString)
  def matchesContinue(line: String) = matchesString(line, ContinueString)
  def running = isRunning

  private def matchesString(line: String, target: String): Boolean = (
    (line startsWith target) ||
    (line.nonEmpty && " \t".toSet(line.head) && matchesString(line.tail, target))
  )
  private def stripString(line: String, target: String) = line indexOf target match {
    case -1   => line
    case idx  => line drop (idx + target.length)
  }
  private var isRunning    = false
  private val resReference = """(?<!^)(res\d+)""".r
  private val resCreation  = """^\s*(res\d+):.*""".r
  private val resAssign    = """^val (res\d+).*""".r

  private class PasteAnalyzer(val lines: List[String]) {
    val referenced = lines flatMap (resReference findAllIn _.trim.stripPrefix("res")) toSet
    val cmds       = lines reduceLeft append split PromptString filterNot (_.trim == "") toList

    /** If it's a prompt or continuation line, strip the formatting bits and
     *  assemble the code.  Otherwise ship it off to be analyzed for res references
     *  and discarded.
     */
    def append(code: String, line: String): String =
      if (matchesPrompt(line)) code + "\n" + line
      else if (matchesContinue(line)) code + "\n" + stripString(line, ContinueString)
      else fixResRefs(code, line)

    /** If the line looks like
     *    res15: Int
     *
     *  and the additional conditions hold that:
     *    1) res15 is referenced from elsewhere in the transcript
     *    2) the preceding repl line is not "val res15 = ..." because that
     *    indicates it has already been "val-ified" on a previous paste
     *
     *  then we go back in time to the preceding scala> prompt and
     *  rewrite the line containing <expr> as
     *    val res15 = { <expr> }
     *  and the rest as they say is rewritten history.
     *
     *  In all other cases, discard the line.
     */
    def fixResRefs(code: String, line: String) = line match {
      case resCreation(resName) if referenced(resName) =>
        code.lastIndexOf(PromptString) match {
          case -1   => code
          case idx  =>
            val (str1, str2) = code splitAt (idx + PromptString.length)
            str2 match {
              case resAssign(`resName`) => code
              case _                    => "%sval %s = { %s }".format(str1, resName, str2)
            }
        }
      case _ => code
    }

    def run() {
      println("// Replaying %d commands from transcript.\n" format cmds.size)
      cmds foreach { cmd =>
        print(PromptString)
        interpret(cmd)
      }
    }
  }

  /** Commands start on lines beginning with "scala>" and each successive
   *  line which begins with the continuation string is appended to that command.
   *  Everything else is discarded.  When the end of the transcript is spotted,
   *  all the commands are replayed.
   */
  def apply(lines: TraversableOnce[String]) = {
    isRunning = true
    try new PasteAnalyzer(lines.toList) run()
    finally isRunning = false
  }
}
