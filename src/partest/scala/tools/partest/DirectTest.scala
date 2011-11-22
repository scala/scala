/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.partest

import scala.tools.nsc._
import io.Directory
import util.{BatchSourceFile, CommandLineParser}

/** A class for testing code which is embedded as a string.
 *  It allows for more complete control over settings, compiler
 *  configuration, sequence of events, etc. than does partest.
 */
abstract class DirectTest extends App {
  // The program being tested in some fashion
  def code: String
  // produce the output to be compared against a checkfile
  def show(): Unit

  // the test file or dir, and output directory
  def testPath   = io.File(sys.props("partest.test-path"))
  def testOutput = io.Directory(sys.props("partest.output"))

  // override to add additional settings with strings
  def extraSettings: String = ""
  // a default Settings object
  def settings: Settings = newSettings(CommandLineParser tokenize extraSettings)
  // a custom Settings object
  def newSettings(args: List[String]) = {
    val s = new Settings
    val allArgs = args ++ (CommandLineParser tokenize debugSettings)
    log("newSettings: allArgs = " + allArgs)
    s processArguments (allArgs, true)
    s
  }
  // compile the code, optionally first adding to the settings
  def compile(args: String*) = {
    val settings = newSettings((CommandLineParser tokenize extraSettings) ++ args.toList)
    val global   = new Global(settings)
    new global.Run compileSources List(new BatchSourceFile("<partest>", code))
    !global.reporter.hasErrors
  }

  /**  Constructor/main body  **/
  try show()
  catch { case t => println(t) ; sys.exit(1) }

  /** Debugger interest only below this line **/
  protected def isDebug       = (sys.props contains "partest.debug") || (sys.env contains "PARTEST_DEBUG")
  protected def debugSettings = sys.props.getOrElse("partest.debug.settings", "")

  final def log(msg: => Any) {
    if (isDebug) Console.err println msg
  }
}
