/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.partest

import scala.tools.nsc._
import io.Directory
import util.BatchSourceFile

/** A class for testing code which is embedded as a string.
 *  It allows for more complete control over settings, compiler
 *  configuration, sequence of events, etc. than does partest.
 */
abstract class DirectTest extends App {
  // The program being tested in some fashion
  def code: String
  // produce the output to be compared against a checkfile
  def show(): Unit

  // override to add additional settings with strings
  def extraSettings: String = ""
  // a default Settings object
  def settings: Settings = newSettings(extraSettings)
  // a custom Settings object
  def newSettings(argString: String) = {
    val s = new Settings
    s processArgumentString (argString + " " + debugSettings)
    s
  }
  // compile the code, optionally first adding to the settings
  def compile(args: String*) = {
    val settings = newSettings(extraSettings +: args mkString " ")
    val global   = new Global(settings)
    new global.Run compileSources List(new BatchSourceFile("<partest>", code))
    !global.reporter.hasErrors
  }

  /**  Constructor/main body  **/
  try show()
  catch { case t => println(t) ; sys.exit(1) }

  /** Debugger interest only below this line **/
  protected val isDebug       = (sys.props contains "partest.debug") || (sys.env contains "PARTEST_DEBUG")
  protected def debugSettings = sys.props.getOrElse("partest.debug.settings", "")

  final def log(msg: => Any) { if (isDebug) Console println msg }
}
