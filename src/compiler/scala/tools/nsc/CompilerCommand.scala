/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import Settings.Setting
import java.io.IOException
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.util.ArgumentsExpander

/** A class representing command line info for scalac */
class CompilerCommand(
  arguments: List[String],
  val settings: Settings,
  error: String => Unit,
  interactive: Boolean,
  shouldProcessArguments: Boolean)
{
  def this(arguments: List[String], settings: Settings, error: String => Unit, interactive: Boolean) =
    this(arguments, settings, error, interactive, true)

  /** file extensions of files that the compiler can process */
  lazy val fileEndings = Properties.fileEndings

  /** The name of the command */
  val cmdName = "scalac"

  private val helpSyntaxColumnWidth: Int =
    (settings.visibleSettings map (_.helpSyntax.length)) max

  private def format(s: String): String =
    if (s.length >= helpSyntaxColumnWidth) s
    else s + (" " * (helpSyntaxColumnWidth - s.length))

  /** Creates a help message for a subset of options based on cond */
  def createUsageMsg(label: String, cond: (Setting) => Boolean): String =
    settings.visibleSettings .
      filter(cond) .
      map(s => format(s.helpSyntax) + "  " + s.helpDescription) .
      toList.sorted.mkString("Usage: %s <options> <source files>\n%s options include:\n  " .
        format(cmdName, label), "\n  ", "\n")

  /** Messages explaining usage and options */
  def usageMsg    = createUsageMsg("where possible standard", _.isStandard)
  def fscUsageMsg = createUsageMsg("where possible standard", ( st => st.isStandard || st.isFscSpecific ))
  def xusageMsg   = createUsageMsg("Possible advanced", _.isAdvanced)
  def yusageMsg   = createUsageMsg("Possible private", _.isPrivate)

  // If any of these settings is set, the compiler shouldn't start;
  // an informative message of some sort should be printed instead.
  // (note: do not add "files.isEmpty" do this list)
  val stopSettings = List[(() => Boolean, (Global) => String)](
    ((() => (settings.help.value _)() && (cmdName == "fsc")),
                                    fscUsageMsg + _.pluginOptionsHelp),
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

  // CompilerCommand needs processArguments called at the end of its constructor,
  // as does its subclass GenericRunnerCommand, but it cannot be called twice as it
  // accumulates arguments.  The fact that it's called from within the constructors
  // makes initialization order an obstacle to simplicity.
  val (ok: Boolean, files: List[String]) =
    if (shouldProcessArguments) {
      // expand out @filename to the contents of that filename
      val expandedArguments = arguments flatMap {
        case x if x startsWith "@"  => ArgumentsExpander expandArg x
        case x                      => List(x)
      }

      settings.processArguments(expandedArguments, true)
    }
    else (true, Nil)
}
