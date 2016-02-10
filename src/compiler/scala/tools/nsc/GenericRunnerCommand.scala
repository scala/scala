/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  Lex Spoon
 */

package scala.tools.nsc

import GenericRunnerCommand._
import scala.reflect.internal.util.ScalaClassLoader

/** A command for ScriptRunner */
class GenericRunnerCommand(
  args: List[String],
  override val settings: GenericRunnerSettings)
extends CompilerCommand(args, settings) {

  def this(args: List[String], error: String => Unit) =
    this(args, new GenericRunnerSettings(error))

  def this(args: List[String]) =
    this(args, str => Console.println("Error: " + str))

  override def cmdName = "scala"
  override def cmdDesc = "code runner"

  def compCmdName = "scalac"  // super.cmdName

  // change CompilerCommand behavior
  override def shouldProcessArguments: Boolean = false

  private lazy val (_ok, targetAndArguments) = settings.processArguments(args, processAll = false)
  override def ok = _ok
  private def guessHowToRun(target: String): GenericRunnerCommand.HowToRun = {
    if (!ok) Error
    else if (io.Jar.isJarOrZip(target)) AsJar
    else if (ScalaClassLoader.classExists(settings.classpathURLs, target)) AsObject
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

  def shortUsageMsg =
s"""|Usage: $cmdName <options> [<script|class|object|jar> <arguments>]
    |   or  $cmdName -help
    |
    |All options to $compCmdName (see $compCmdName -help) are also allowed.
""".stripMargin

  override def usageMsg = f"""$shortUsageMsg
The first given argument other than options to $cmdName designates
what to run.  Runnable targets are:

  - a file containing scala source
  - the name of a compiled class
  - a runnable jar file with a valid Main-Class attribute
  - or if no argument is given, the repl (interactive shell) is started

Options to $cmdName which reach the java runtime:

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

When running a script or using -e, an already running compilation daemon
(fsc) is used, or a new one started on demand.  The -nc option can be
used to prevent this.%n"""
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
