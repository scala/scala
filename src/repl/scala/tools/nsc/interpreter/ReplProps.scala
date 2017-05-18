/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc.interpreter

import java.io.File
import java.util.{Formattable, FormattableFlags, Formatter}

import scala.tools.nsc.Properties
import Properties._
import scala.sys.{BooleanProp, Prop}
import scala.sys.Prop._

trait ReplProps {
  private def bool(name: String) = BooleanProp.keyExists(name)
  private def int(name: String)  = Prop[Int](name)

  // This property is used in TypeDebugging. Let's recycle it.
  val colorOk = Properties.coloredOutputEnabled

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
        formatter.format(fmt, version)
      }
    }
    s.format(v, javaVersion, javaVmName)
  }
  def encolor(s: String)   = {
    import scala.io.AnsiColor.{ MAGENTA, RESET }
    if (colorOk) s"$MAGENTA$s$RESET" else s
  }

  // Handy system prop for shell prompt, or else pick it up from compiler.properties
  val promptString = Prop[String]("scala.repl.prompt").option getOrElse (if (info) "%nscala %#s> " else shellPromptString)
  val promptText   = enversion(promptString)


  // Prompt for continued input, will be right-adjusted to width of the primary prompt
  val continueString = Prop[String]("scala.repl.continue").option getOrElse "| "
  val welcomeString  = Prop[String]("scala.repl.welcome").option getOrElse shellWelcomeString

  val pasteDelimiter = Prop[String]("scala.repl.here")

  /** CSV of paged,across to enable pagination or `-x` style
   *  columns, "across" instead of down the column.  Since
   *  pagination turns off columnar output, these flags are
   *  currently mutually exclusive.
   */
  val format = Prop[String]("scala.repl.format")

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
  def isPaged: Boolean     = format.isSet && csv(format.get, "paged")
  def isAcross: Boolean    = format.isSet && csv(format.get, "across")

  private def csv(p: String, v: String) = p split "," contains v
  private def echo(msg: => String) =
    try Console println msg
    catch { case x: AssertionError => Console.println("Assertion error printing debugging output: " + x) }
}
