/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
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

  /** The name of the command */
  def cmdName = "scalac"
  private def isFsc = cmdName == "fsc"

  private val helpSyntaxColumnWidth: Int =
    (settings.visibleSettings map (_.helpSyntax.length)) max

  private def format(s: String): String =
    if (s.length >= helpSyntaxColumnWidth) s
    else s + (" " * (helpSyntaxColumnWidth - s.length))

  private val explainAdvanced = "\n" + """
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

  /** Creates a help message for a subset of options based on cond */
  def createUsageMsg(label: String, shouldExplain: Boolean, cond: (Setting) => Boolean): String = {
    def helpStr(s: Setting) = format(s.helpSyntax) + "  " + s.helpDescription

    val usage         = "Usage: %s <options> <source files>\n" format cmdName
    val explain       = if (shouldExplain) explainAdvanced else ""
    val prefix        = label + " options include:\n  "

    // Separating out any debugging options from others for easier reading
    val (debug, rest) = (settings.visibleSettings filter cond).toList sortBy (_.name) partition (_.isForDebug)

    (rest map helpStr).mkString(usage + explain + prefix, "\n  ", "\n") + (
      if (debug.isEmpty) ""
      else (debug map helpStr).mkString("\nAdditional debug settings:\n  ", "\n  ", "\n")
    )
  }

  /** Messages explaining usage and options */
  def usageMsg    = createUsageMsg("where possible standard", false, _.isStandard)
  def fscUsageMsg = createUsageMsg("where possible standard", false, ( st => st.isStandard || st.name == "-shutdown"))
  def xusageMsg   = createUsageMsg("Possible advanced", true, _.isAdvanced)
  def yusageMsg   = createUsageMsg("Possible private", true, _.isPrivate)

  // If any of these settings is set, the compiler shouldn't start;
  // an informative message of some sort should be printed instead.
  // (note: do not add "files.isEmpty" do this list)
  val stopSettings = List[(() => Boolean, (Global) => String)](
    ((() => (settings.help.value _)() && isFsc), fscUsageMsg + _.pluginOptionsHelp),
    (settings.help.value _,         usageMsg + _.pluginOptionsHelp),
    (settings.Xhelp.value _,        _ => xusageMsg),
    (settings.Yhelp.value _,        _ => yusageMsg),
    (settings.showPlugins.value _,  _.pluginDescriptions),
    (settings.showPhases.value _,   _.phaseDescriptions)
  )
  def shouldStopWithInfo: Boolean = stopSettings exists { _._1() }

  def getInfoMessage(compiler: Global): String =
    stopSettings.find(pair => (pair._1)()) match {
      case Some((test, getMessage)) => getMessage(compiler)
      case None => ""
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

  val (ok, files) =
    if (shouldProcessArguments) processArguments
    else (true, Nil)
}
