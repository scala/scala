/* NSC -- new Scala compiler
 * Copyright 2007-2012 LAMP/EPFL
 * @author  Lex Spoon
 */

package scala.tools.nsc

import GenericRunnerCommand._

/** A command for ScriptRunner */
class GenericRunnerCommand(
  args: List[String],
  override val settings: GenericRunnerSettings)
extends CompilerCommand(args, settings) {

  def this(args: List[String], error: String => Unit) =
    this(args, new GenericRunnerSettings(error))

  def this(args: List[String]) =
    this(args, str => Console.println("Error: " + str))

  /** name of the associated compiler command */
  override def cmdName = "scala"
  def compCmdName = "scalac"

  // change CompilerCommand behavior
  override def shouldProcessArguments: Boolean = false

  private lazy val (_ok, targetAndArguments) = settings.processArguments(args, false)
  override def ok = _ok
  private def guessHowToRun(target: String): GenericRunnerCommand.HowToRun = {
    if (!ok) Error
    else if (io.Jar.isJarOrZip(target)) AsJar
    else if (util.ScalaClassLoader.classExists(settings.classpathURLs, target)) AsObject
    else {
      val f = io.File(target)
      if (!f.hasExtension("class", "jar", "zip") && f.canRead) AsScript
      else {
        Console.err.println("No such file or class on classpath: " + target)
        Error
      }
    }
  }
  /** String with either the jar file, class name, or script file name. */
  def thingToRun = targetAndArguments.headOption getOrElse ""
  /** Arguments to thingToRun. */
  def arguments = targetAndArguments drop 1

  val howToRun = targetAndArguments match {
    case Nil      => AsRepl
    case hd :: _  => waysToRun find (_.name == settings.howtorun.value) getOrElse guessHowToRun(hd)
  }
  private def interpolate(s: String) = s.trim.replaceAll("@cmd@", cmdName).replaceAll("@compileCmd@", compCmdName) + "\n"

  def shortUsageMsg = interpolate("""
Usage: @cmd@ <options> [<script|class|object|jar> <arguments>]
   or  @cmd@ -help

All options to @compileCmd@ (see @compileCmd@ -help) are also allowed.
""")

  override def usageMsg = shortUsageMsg + interpolate("""
The first given argument other than options to @cmd@ designates
what to run.  Runnable targets are:

  - a file containing scala source
  - the name of a compiled class
  - a runnable jar file with a valid Main-Class attribute
  - or if no argument is given, the repl (interactive shell) is started

Options to @cmd@ which reach the java runtime:

 -Dname=prop  passed directly to java to set system properties
 -J<arg>      -J is stripped and <arg> passed to java as-is
 -nobootcp    do not put the scala jars on the boot classpath (slower)

Other startup options:

 -howtorun    what to run <script|object|jar|guess> (default: guess)
 -i <file>    preload <file> before starting the repl
 -e <string>  execute <string> as if entered in the repl
 -save        save the compiled script in a jar for future use
 -nc          no compilation daemon: do not use the fsc offline compiler

A file argument will be run as a scala script unless it contains only
self-contained compilation units (classes and objects) and exactly one
runnable main method.  In that case the file will be compiled and the
main method invoked.  This provides a bridge between scripts and standard
scala source.
  """) + "\n"
}

object GenericRunnerCommand {
  sealed abstract class HowToRun(val name: String) { }
  case object AsJar extends HowToRun("jar")
  case object AsObject extends HowToRun("object")
  case object AsScript extends HowToRun("script")
  case object AsRepl extends HowToRun("repl")
  case object Error extends HowToRun("<error>")
  val waysToRun = List(AsJar, AsObject, AsScript, AsRepl)
}
