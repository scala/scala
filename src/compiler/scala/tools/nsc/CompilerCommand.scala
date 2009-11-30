/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import Settings.Setting
import java.io.IOException

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

  /** Private buffer for accumulating files to compile */
  private var fs: List[String] = List()

  /** Public list of files to compile */
  def files: List[String] = fs.reverse

  /** The name of the command */
  val cmdName = "scalac"

  private val helpSyntaxColumnWidth: Int =
    (settings.allSettings map (_.helpSyntax.length)) max

  private def format(s: String): String = {
    val buf = new StringBuilder(s)
    var i = s.length
    while (i < helpSyntaxColumnWidth) { buf.append(' '); i += 1 }
    buf.toString()
  }

  /** Creates a help message for a subset of options based on cond */
  def createUsageMsg(label: String, cond: (Setting) => Boolean): String =
    settings.allSettings .
      filter(cond) .
      map(s => format(s.helpSyntax) + "  " + s.helpDescription) .
      mkString("Usage: %s <options> <source files>\n%s options include:\n  " .
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

  /** Whether the command was processed okay */
  var ok = true

  /** Process the arguments and update the settings accordingly.
      This method is called only once, during initialization.  */
  protected def processArguments() {
    // initialization
    var args = arguments
    def errorAndNotOk(msg: String) = { error(msg) ; ok = false }

    // given a @ argument expands it out
    def doExpand(x: String) =
      try   { args = util.ArgumentsExpander.expandArg(x) ::: args.tail }
      catch { case ex: IOException  => errorAndNotOk(ex.getMessage) }

    // true if it's a legit looking source file
    def isSourceFile(x: String) =
      (settings.script.value != "") ||
      (fileEndings exists (x endsWith _))

    // given an option for scalac finds out what it is
    def doOption(x: String): Unit = {
      if (interactive)
        return errorAndNotOk("no options can be given in interactive mode")

      val argsLeft = settings.parseParams(args)
      if (args != argsLeft) args = argsLeft
      else errorAndNotOk("bad option: '" + x + "'")
    }

    // cycle through args until empty or error
    while (!args.isEmpty && ok) args.head match {
      case x if x startsWith "@"  => doExpand(x)
      case x if x startsWith "-"  => doOption(x)
      case x if isSourceFile(x)   => fs = x :: fs ; args = args.tail
      case ""                     => args = args.tail // quick fix [martin: for what?]
      case x                      => errorAndNotOk("don't know what to do with " + x)
    }

    ok &&= settings.checkDependencies
  }

  // CompilerCommand needs processArguments called at the end of its constructor,
  // as does its subclass GenericRunnerCommand, but it cannot be called twice as it
  // accumulates arguments.  The fact that it's called from within the constructors
  // makes initialization order an obstacle to simplicity.
  if (shouldProcessArguments)
    processArguments()
}
