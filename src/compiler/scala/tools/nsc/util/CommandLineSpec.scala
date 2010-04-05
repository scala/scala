/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package util

import Properties._
import io._
import CommandLineSpec._
import CommandLineParser.tokenize

/** This trait works together with CommandLine to allow declaratively
 *  specifying a command line program, with many attendant benefits.
 *  See scala.tools.partest.PartestSpec for a full example.
 */

trait CommandLineSpec {
  def parsed: CommandLine
  def isReferenceSpec: Boolean = false
  def isPassthroughProperty(name: String): Boolean = false
  def isSysPropOption(key: String): Option[String] = None

  private var _helpMessage: String          = ""
  private var _unaryOptions: List[String]   = Nil
  private var _binaryOptions: List[String]  = Nil
  private def allOptions                    = if (isReferenceSpec) Nil else parsed.allOptions
  private def longestArg                    = if (allOptions.isEmpty) 1 else allOptions map (_.length) max
  private def unquoted(s: String)           = {
    def isQuoted = (s.head == '\'' || s.head == '"') && s.head == s.last

    if (s == null || s.length < 2 || !isQuoted) s
    else s drop 1 dropRight 1
  }

  protected def help(str: String)           = if (isReferenceSpec) () else _helpMessage += (str.stripMargin + "\n")
  protected def heading(s: String)          = if (isReferenceSpec) () else help("\n  " + s)

  /** The various operators:
   *    val isCond1 = "cond1" ?     // --cond1 is unary, cond1 is boolean
   *    "cond2" ?> body             // --cond2 is unary, body is executed if it is given
   *    val val1 = "val1" |> "alt"  // --val1 is binary, val1 is String, alt used if none given
   *    val val2 = "val2" >>        // --val2 is binary, val2 is Option[String], None if none given
   */
  protected class OptionStringAdditions(name: String) {
    val s = toOpt(name)
    def ? : Boolean               = {  _unaryOptions +:= s ; if (isReferenceSpec) false else parsed isSet s }
    def ?>(body: => Unit): Unit   = {  _unaryOptions +:= s ; if (isReferenceSpec) () else if (parsed isSet s) body }
    def |>(alt: String): String   = { _binaryOptions +:= s ; if (isReferenceSpec) "" else parsed.getOrElse(s, alt) }
    def >> : Option[String]       = { _binaryOptions +:= s ; if (isReferenceSpec) None else parsed get s }

    def /(description: String)    = {
      val formatStr = "    %-" + longestArg + "s %s"
      help(formatStr.format(s, description))

      name
    }
  }
  protected implicit def stringAdditions(s: String) = new OptionStringAdditions(s)

  lazy val unaryOptions   = _unaryOptions.distinct
  lazy val binaryOptions  = _binaryOptions.distinct
  lazy val helpMsg        = _helpMessage

  def isUnaryOption(s: String)   = unaryOptions contains toOpt(s)
  def isBinaryOption(s: String)  = binaryOptions contains toOpt(s)

  private def sysPropToOptions(k: String, v: String): List[String] = {
    if (isPassthroughProperty(k)) toArgs(v)
    else isSysPropOption(k).toList flatMap { optName =>
      val opt = toOpt(optName)

      if (isUnaryOption(optName)) List(opt)
      else if (isBinaryOption(optName)) List(opt, v)
      else {
        if (warnSuspiciousProperties) {
          println("Warning, this looks like a command line option but I don't understand it.")
          println("Ignoring: " + k + "=" + v)
        }
        Nil
      }
    }
  }
  def warnSuspiciousProperties: Boolean = true
  def sysPropsAsOptions() = allSystemProperties.toList flatMap (sysPropToOptions _).tupled

  def isSet(s: String)  = parsed isSet toOpt(s)
  def reconstruct: List[String]     = {
    val unary   = unaryOptions filter (parsed isSet _)
    val binary  = binaryOptions collect { case x if parsed isSet x  => List(x, parsed(x)) }
    val resid   = parsed.residualArgs

    unary ++ binary.flatten ++ resid
  }

  def bashCompletion(programName: String) = {
    val opts = unaryOptions ++ binaryOptions
    bashCompletionTemplate.replaceAll("@@PROGRAM@@", programName).replaceAll("@@OPTIONS@@", opts mkString " ")
  }
}

object CommandLineSpec {
  def toOpt(s: String)              = if (s startsWith "--") s else "--" + s
  def fromOpt(s: String)            = s stripPrefix "--"
  def toArgs(line: String)          = tokenize(line)
  def fromArgs(args: List[String])  = args mkString " "

  def allSystemProperties: Map[String, String] = {
    import collection.JavaConversions._

    System.getProperties.toMap
  }

  /** A very simple template for generating bash completion functions.
   */
  val bashCompletionTemplate = """
    |_@@PROGRAM@@()
    |{
    |  local cur opts base
    |  COMPREPLY=()
    |  cur="${COMP_WORDS[COMP_CWORD]}"
    |  opts="@@OPTIONS@@"
    |
    |  COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
    |  _filedir
    |  return 0
    |}
    |complete -F _@@PROGRAM@@ @@PROGRAM@@
  """.stripMargin
}
