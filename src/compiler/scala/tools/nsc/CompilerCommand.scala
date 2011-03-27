/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import java.io.IOException
import scala.collection.mutable.ListBuffer
import io.File

/** A class representing command line info for scalac */
class CompilerCommand(arguments: List[String], val settings: Settings) {
  def this(arguments: List[String], error: String => Unit) = this(arguments, new Settings(error))
  type Setting = Settings#Setting

  /** file extensions of files that the compiler can process */
  lazy val fileEndings = Properties.fileEndings

  private val processArgumentsResult =
    if (shouldProcessArguments) processArguments
    else (true, Nil)
  def ok    = processArgumentsResult._1
  def files = processArgumentsResult._2

  /** The name of the command */
  def cmdName = "scalac"

  private def helpSyntaxColumnWidth: Int =
    (settings.visibleSettings map (_.helpSyntax.length)) max

  private def format(s: String): String =
    if (s.length >= helpSyntaxColumnWidth) s
    else s + (" " * (helpSyntaxColumnWidth - s.length))

  private def explainAdvanced = "\n" + """
    |-- Notes on option parsing --
    |Boolean settings are always false unless set.
    |Where multiple values are accepted, they should be comma-separated.
    |  example: -Xplugin:plugin1,plugin2
    |<phase> means one or a list of:
    |  (partial) phase names, phase ids, phase id ranges, or the string "all".
    |  example: -Xprint:all prints all phases.
    |  example: -Xprint:expl,24-26 prints phases explicitouter, closelim, dce, jvm.
    |  example: -Xprint:-4 prints only the phases up to typer.
    |
  """.stripMargin.trim + "\n\n"

  val shortUsage = "Usage: %s <options> <source files>" format cmdName
  def createUsagePreface(shouldExplain: Boolean) =
    if (shouldExplain) shortUsage + "\n" + explainAdvanced else ""

  /** Creates a help message for a subset of options based on cond */
  def createUsageMsg(cond: Setting => Boolean): String = {
    def helpStr(s: Setting) = format(s.helpSyntax) + "  " + s.helpDescription
    // Separating out any debugging options from others for easier reading
    val (debug, rest) = (settings.visibleSettings filter cond).toList sortBy (_.name) partition (_.isForDebug)

    (rest map helpStr).mkString("", "\n  ", "\n") + (
      if (debug.isEmpty) ""
      else (debug map helpStr).mkString("\nAdditional debug settings:\n  ", "\n  ", "\n")
    )
  }
  def createUsageMsg(label: String, shouldExplain: Boolean, cond: Setting => Boolean): String = {
    val prefix = List(
      Some(shortUsage),
      Some(explainAdvanced) filter (_ => shouldExplain),
      Some(label + " options include:\n  ")
    ).flatten mkString "\n"

    prefix + createUsageMsg(cond)
  }

  /** Messages explaining usage and options */
  def usageMsg    = createUsageMsg("where possible standard", false, _.isStandard)
  def xusageMsg   = createUsageMsg("Possible advanced", true, _.isAdvanced)
  def yusageMsg   = createUsageMsg("Possible private", true, _.isPrivate)

  // If any of these settings is set, the compiler shouldn't start;
  // an informative message of some sort should be printed instead.
  def shouldStopWithInfo = {
    import settings.{ Setting => _, _ }
    Set[BooleanSetting](help, Xhelp, Yhelp, showPlugins, showPhases) exists (_.value)
  }

  def getInfoMessage(global: Global): String = {
    import settings._
    if (help.value)               usageMsg + global.pluginOptionsHelp
    else if (Xhelp.value)         xusageMsg
    else if (Yhelp.value)         yusageMsg
    else if (showPlugins.value)   global.pluginDescriptions
    else if (showPhases.value)    global.phaseDescriptions
    else                          ""
  }

  /**
   * Expands all arguments starting with @ to the contents of the
   * file named like each argument.
   */
  def expandArg(arg: String): List[String] = {
    def stripComment(s: String) = s takeWhile (_ != '#')
    val file = File(arg stripPrefix "@")
    if (!file.exists)
      throw new java.io.FileNotFoundException("argument file %s could not be found" format file.name)

    settings splitParams (file.lines() map stripComment mkString " ")
  }

  // override this if you don't want arguments processed here
  def shouldProcessArguments: Boolean = true

  def processArguments: (Boolean, List[String]) = {
    // expand out @filename to the contents of that filename
    val expandedArguments = arguments flatMap {
      case x if x startsWith "@"  => expandArg(x)
      case x                      => List(x)
    }

    settings.processArguments(expandedArguments, true)
  }
}
