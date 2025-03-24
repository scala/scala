/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.interpreter.shell

import scala.tools.nsc.interpreter.Results.{Result, Incomplete}

/** If it looks like they're pasting in a scala interpreter
 *  transcript, remove all the formatting we inserted so we
 *  can make some sense of it.
 *
 *  Most of the interesting code in here is due to my goal of
 *  "paste idempotence" i.e. the transcript resulting from pasting
 *  a transcript should itself be pasteable and should achieve
 *  the same result.
 */
abstract class Pasted(prompt: String, continuePrompt: String, continueText: String) {

  def interpret(line: String): Result
  def echo(message: String): Unit

  val PromptString    = prompt.linesIterator.toList.last
  val AltPromptString = "scala> "
  val ContinuePrompt  = continuePrompt
  val ContinueString  = continueText     // "     | "
  val anyPrompt = {
    import scala.util.matching.Regex.quote
    s"""\\s*(?:${quote(PromptString.trim)}|${quote(AltPromptString.trim)})\\s*""".r
  }

  def isPrompted(line: String)   = matchesPrompt(line)
  def isPromptOnly(line: String) = line match { case anyPrompt() => true ; case _ => false }

  private val testBoth = PromptString != AltPromptString
  private val spacey   = " \t".toSet

  def matchesPrompt(line: String) = matchesString(line, PromptString) || testBoth && matchesString(line, AltPromptString)
  def matchesContinue(line: String) = matchesString(line, ContinueString)
  def running = isRunning

  private def matchesString(line: String, target: String): Boolean =
    line.startsWith(target) || (line.nonEmpty && spacey(line.head) && matchesString(line.tail, target))

  private def stripString(line: String, target: String) = line indexOf target match {
    case -1   => line
    case idx  => line drop (idx + target.length)
  }
  private var isRunning    = false
  private val resReference = """(?<!^)(res\d+)""".r
  private val resCreation  = """^\s*(res\d+):.*""".r
  private val resAssign    = """^val (res\d+).*""".r

  private class PasteAnalyzer(val lines: List[String]) {
    val referenced = lines.flatMap(s => resReference.findAllIn(s.trim.stripPrefix("res"))).toSet
    val ActualPromptString = lines.find(matchesPrompt).map(s =>
      if (matchesString(s, PromptString)) PromptString else AltPromptString).getOrElse(PromptString)
    val cmds       = lines.reduceLeft(append).split(ActualPromptString).filterNot(_.trim == "").toList

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
        code.lastIndexOf(ActualPromptString) match {
          case -1   => code
          case idx  =>
            val (str1, str2) = code splitAt (idx + ActualPromptString.length)
            str2 match {
              case resAssign(`resName`) => code
              case _                    => "%sval %s = { %s }".format(str1, resName, str2)
            }
        }
      case _ => code
    }

    def interpreted(line: String) = {
      echo(line.trim)
      val res = interpret(line)
      if (res != Incomplete) echo("")
      res
    }
    def incompletely(cmd: String) = {
      print(ActualPromptString)
      interpreted(cmd) == Incomplete
    }
    def run(): Option[String] = {
      echo(s"// Replaying ${cmds.size} commands from transcript.\n")
      cmds find incompletely
    }
  }

  // Run transcript and return incomplete line if any.
  def transcript(lines: IterableOnce[String]): Option[String] = {
    echo("\n// Detected repl transcript. Paste more, or ctrl-D to finish.\n")
    apply(lines)
  }

  /** Commands start on lines beginning with "scala>" and each successive
   *  line which begins with the continuation string is appended to that command.
   *  Everything else is discarded.  When the end of the transcript is spotted,
   *  all the commands are replayed.
   */
  def apply(lines: IterableOnce[String]): Option[String] = {
    isRunning = true
    try new PasteAnalyzer(List.from(lines)).run()
    finally isRunning = false
  }

  // used during loop
  def unapply(line: String): Boolean = !running && isPrompted(line)
}
