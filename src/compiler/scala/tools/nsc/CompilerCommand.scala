/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import io.File

/** A class representing command line info for scalac */
class CompilerCommand(arguments: List[String], val settings: Settings) {
  def this(arguments: List[String], error: String => Unit) = this(arguments, new Settings(error))
  def this(arguments: List[String], settings: Settings, error: String => Unit) = this(arguments, settings withErrorFn error)

  type Setting = Settings#Setting

  private val processArgumentsResult =
    if (shouldProcessArguments) processArguments
    else (true, Nil)
  def ok    = processArgumentsResult._1
  def files = processArgumentsResult._2

  /** The name of the command. */
  def cmdName = "scalac"

  /** A descriptive alias for version and help messages. */
  def cmdDesc = "compiler"

  private def explainAdvanced = "\n" + """
    |-- Notes on option parsing --
    |Boolean settings are always false unless set.
    |Where multiple values are accepted, they should be comma-separated.
    |  example: -Xplugin:option1,option2
    |<phases> means one or a comma-separated list of:
    |  (partial) phase names, phase ids, phase id ranges, or the string "all".
    |  example: -Xprint:all prints all phases.
    |  example: -Xprint:expl,24-26 prints phases explicitouter, closelim, dce, jvm.
    |  example: -Xprint:-4 prints only the phases up to typer.
    |
  """.stripMargin.trim + "\n"

  def shortUsage = "Usage: %s <options> <source files>" format cmdName

  /** Creates a help message for a subset of options based on cond */
  def createUsageMsg(cond: Setting => Boolean): String = {
    val baseList            = (settings.visibleSettings filter cond).toList sortBy (_.name)
    val width               = (baseList map (_.helpSyntax.length)).max
    def format(s: String)   = ("%-" + width + "s") format s
    def helpStr(s: Setting) = {
      val str    = format(s.helpSyntax) + "  " + s.helpDescription
      val suffix = s.deprecationMessage match {
        case Some(msg) => "\n" + format("") + "      deprecated: " + msg
        case _         => ""
      }
      str + suffix
    }
    val debugs      = baseList filter (_.isForDebug)
    val deprecateds = baseList filter (_.isDeprecated)
    val theRest     = baseList filterNot (debugs.toSet ++ deprecateds)

    def sstring(msg: String, xs: List[Setting]) =
      if (xs.isEmpty) None else Some(msg :: xs.map(helpStr) mkString "\n  ")

    List(
      sstring("", theRest),
      sstring("\nAdditional debug settings:", debugs),
      sstring("\nDeprecated settings:", deprecateds)
    ).flatten mkString "\n"
  }

  def createUsageMsg(label: String, shouldExplain: Boolean, cond: Setting => Boolean): String = {
    val prefix = List(
      Some(shortUsage),
      Some(explainAdvanced) filter (_ => shouldExplain),
      Some(label + " options include:")
    ).flatten mkString "\n"

    prefix + createUsageMsg(cond)
  }

  /** Messages explaining usage and options */
  def usageMsg    = createUsageMsg("where possible standard", shouldExplain = false, _.isStandard)
  def xusageMsg   = createUsageMsg("Possible advanced", shouldExplain = true, _.isAdvanced)
  def yusageMsg   = createUsageMsg("Possible private", shouldExplain = true, _.isPrivate)

  /** For info settings, compiler should just print a message and quit. */
  def shouldStopWithInfo = settings.isInfo

  def getInfoMessage(global: Global): String = {
    import settings._
    import Properties.{ versionString, copyrightString } //versionFor
    def versionFor(command: String) = f"Scala $command $versionString -- $copyrightString"

    if (version)            versionFor(cmdDesc)
    else if (help)          usageMsg + global.pluginOptionsHelp
    else if (Xhelp)         xusageMsg
    else if (Yhelp)         yusageMsg
    else if (showPlugins)   global.pluginDescriptions
    else if (showPhases)    global.phaseDescriptions + (
      if (debug) "\n" + global.phaseFlagDescriptions else ""
    )
    else if (genPhaseGraph.isSetByUser) {
      val components = global.phaseNames  // global.phaseDescriptors // one initializes
      s"Phase graph of ${components.size} components output to ${genPhaseGraph.value}*.dot."
    }
    // would be nicer if we could ask all the options for their helpful messages
    else {
      val sb = new StringBuilder
      allSettings foreach {
        case s if s.isHelping => sb append s.help
        case _ =>
      }
      sb.toString
    }
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

    settings.processArguments(expandedArguments, processAll = true)
  }
}
