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

import java.io.File
import java.util.{Formattable, FormattableFlags, Formatter}

import scala.annotation.nowarn
import scala.sys.{BooleanProp, Prop}
import scala.sys.Prop._

import scala.tools.nsc.{GenericRunnerSettings, Settings}
import scala.tools.nsc.Properties.{
  coloredOutputEnabled, envOrNone, javaVersion, javaVmName,
  shellBannerString, shellInterruptedString, shellPromptString, shellWelcomeString,
  userHome, versionString, versionNumberString,
}

object ShellConfig {
  val EDITOR = envOrNone("EDITOR")
  // how to say we :quit
  val InterruptedString = shellInterruptedString

  def apply(settings: Settings) = settings match {
    case settings: GenericRunnerSettings => new ShellConfig {
      val filesToPaste: List[String] = settings.pastefiles.value
      val filesToLoad: List[String] = settings.loadfiles.value
      val batchText: String = if (settings.execute.isSetByUser) settings.execute.value else ""
      val batchMode: Boolean = batchText.nonEmpty
      val doCompletion: Boolean = !(settings.noCompletion.value || batchMode)
      val haveInteractiveConsole: Boolean = !settings.Xnojline.value
      def xsource: String = if (settings.isScala3: @nowarn) settings.source.value.versionString else ""
    }
    case _ => new ShellConfig {
      val filesToPaste: List[String] = Nil
      val filesToLoad: List[String] = Nil
      val batchText: String = ""
      val batchMode: Boolean = false
      val doCompletion: Boolean = !settings.noCompletion.value
      val haveInteractiveConsole: Boolean = !settings.Xnojline.value
      def xsource: String = if (settings.isScala3: @nowarn) settings.source.value.versionString else ""
    }
  }
}

trait ShellConfig {
  def filesToPaste: List[String]
  def filesToLoad: List[String]
  def batchText: String
  def batchMode: Boolean
  def doCompletion: Boolean
  def haveInteractiveConsole: Boolean

  // source compatibility, i.e., -Xsource
  def xsource: String

  private def bool(name: String) = BooleanProp.keyExists(name)
  private def int(name: String)  = Prop[Int](name)

  // This property is used in TypeDebugging. Let's recycle it.
  val colorOk = coloredOutputEnabled

  val historyFile = s"$userHome/.scala_history_jline3"

  private val info  = bool("scala.repl.info")
  private val debug = bool("scala.repl.debug")
  private val trace = bool("scala.repl.trace")
  val power = bool("scala.repl.power")

  def enversion(s: String) = {
    import FormattableFlags._
    val v = new Formattable {
      override def formatTo(formatter: Formatter, flags: Int, width: Int, precision: Int) = {
        val version = if ((flags & ALTERNATE) != 0) versionNumberString else versionString
        val left    = if ((flags & LEFT_JUSTIFY) != 0) "-" else ""
        val w       = if (width >= 0) s"$width" else ""
        val p       = if (precision >= 0) s".$precision" else ""
        val fmt     = s"%${left}${w}${p}s"

        val xversion = if (xsource.isEmpty) version else s"$version -Xsource:$xsource"
        formatter.format(fmt, xversion)
      }
    }
    s.format(v, javaVersion, javaVmName)
  }
  def encolor(s: String)   = {
    import scala.io.AnsiColor.{MAGENTA, RESET}
    if (colorOk) s"$MAGENTA$s$RESET" else s
  }

  // Handy system prop for shell prompt, or else pick it up from compiler.properties
  val promptString = Prop[String]("scala.repl.prompt").option getOrElse (if (info) "%nscala %#s> " else shellPromptString)
  val promptText   = enversion(promptString)


  // Prompt for continued input, will be right-adjusted to width of the primary prompt
  val continueString = Prop[String]("scala.repl.continue").option getOrElse "| "
  val continueText   = {
    val text   = enversion(continueString)
    val margin = promptText.linesIterator.toList.last.length - text.length
    if (margin > 0) " " * margin + text else text
  }

  // What to display at REPL startup.
  val welcomeString  = Prop[String]("scala.repl.welcome").option match {
    case Some("banner") => shellBannerString
    case Some(text)     => text
    case _              => shellWelcomeString
  }

  val pasteDelimiter = Prop[String]("scala.repl.here")

  /** CSV of paged,across to enable pagination or `-x` style
   *  columns, "across" instead of down the column.  Since
   *  pagination turns off columnar output, these flags are
   *  currently mutually exclusive.
   */
  val format = Prop[String]("scala.repl.format")
  val isPaged: Boolean  = format.isSet && csv(format.get, "paged")
  val isAcross: Boolean = format.isSet && csv(format.get, "across")
  private def csv(p: String, v: String) = p.split(",").contains(v)

  val replAutorunCode = Prop[File]("scala.repl.autoruncode")
  val powerInitCode   = Prop[File]("scala.repl.power.initcode")
  val powerBanner     = Prop[File]("scala.repl.power.banner")

  val maxPrintString = int("scala.repl.maxprintstring")

  def isReplInfo: Boolean  = info || isReplDebug
  def replinfo(msg: => String)   = if (isReplInfo)  echo(msg)
  def isReplDebug: Boolean = debug || isReplTrace
  def repldbg(msg: => String)    = if (isReplDebug) echo(msg)
  def isReplTrace: Boolean = trace
  def repltrace(msg: => String)  = if (isReplTrace) echo(msg)

  def isReplPower: Boolean = power

  private def echo(msg: => String) =
    try Console.println(msg)
    catch {
      case e: AssertionError =>
        Console.println(s"Assertion error printing debugging output: $e")
    }
}
