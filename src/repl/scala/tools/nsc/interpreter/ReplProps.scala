/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import Properties.{ javaVersion, javaVmName, shellPromptString, shellWelcomeString,
                    versionString, versionNumberString }
import scala.sys._
import Prop._
import java.util.{ Formattable, FormattableFlags, Formatter }

class ReplProps {
  private def bool(name: String) = BooleanProp.keyExists(name)
  private def int(name: String)  = Prop[Int](name)

  // This property is used in TypeDebugging. Let's recycle it.
  val colorOk = bool("scala.color")

  val info  = bool("scala.repl.info")
  val debug = bool("scala.repl.debug")
  val trace = bool("scala.repl.trace")
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
  val prompt       = encolor(promptText)

  // Prompt for continued input, will be right-adjusted to width of the primary prompt
  val continueString = Prop[String]("scala.repl.continue").option getOrElse "| "
  val continueText   = {
    val text   = enversion(continueString)
    val margin = promptText.lines.toList.last.length - text.length
    if (margin > 0) " " * margin + text else text
  }
  val continuePrompt = encolor(continueText)

  // Next time.
  //def welcome = enversion(Prop[String]("scala.repl.welcome") or shellWelcomeString)
  def welcome = enversion {
    val p = Prop[String]("scala.repl.welcome")
    if (p.isSet) p.get else shellWelcomeString
  }

  val pasteDelimiter = Prop[String]("scala.repl.here")

  /** CSV of paged,across to enable pagination or `-x` style
   *  columns, "across" instead of down the column.  Since
   *  pagination turns off columnar output, these flags are
   *  currently mutually exclusive.
   */
  val format = Prop[String]("scala.repl.format")

  val replAutorunCode = Prop[JFile]("scala.repl.autoruncode")
  val powerInitCode   = Prop[JFile]("scala.repl.power.initcode")
  val powerBanner     = Prop[JFile]("scala.repl.power.banner")

  val vids = bool("scala.repl.vids")
  val maxPrintString = int("scala.repl.maxprintstring")
}
