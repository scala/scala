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

  private val (_ok, targetAndArguments) = settings.processArguments(args, processAll = false)
  override def ok = _ok
  private def guessHowToRun(target: String): GenericRunnerCommand.HowToRun = {
    if (!ok) Error
    else if (io.Jar.isJarOrZip(target)) AsJar
    else if (ScalaClassLoader.classExists(settings.classpathURLs, target)) AsObject
    else {
      val f = io.File(target)
      if (!f.hasExtension("class", "jar", "zip") && f.canRead) AsScript
      else {
        settings.errorFn("No such file or class on classpath: " + target)
        Error
      }
    }
  }
  /** String with either the jar file, class name, or script file name. */
  def thingToRun = targetAndArguments.headOption getOrElse ""
  /** Arguments to thingToRun. */
  def arguments = targetAndArguments drop 1

  val howToRun = waysToRun.find(_.name == settings.howtorun.value) match {
    case Some(how)                         => how
    case _ if settings.execute.isSetByUser => AsScript
    case _ if targetAndArguments.isEmpty   => AsRepl
    case _                                 => guessHowToRun(thingToRun)
  }

  def shortUsageMsg =
s"""|Usage: $cmdName <options> [<script|class|object|jar> <arguments>]
    |   or  $cmdName -help
    |
    |All options to $compCmdName (see $compCmdName -help) are also allowed.
""".stripMargin

  override def usageMsg = f"""$shortUsageMsg
The first argument to $cmdName after the options designates what to run.

If no argument is given, the Scala REPL, an interactive shell, is started.

Otherwise, the Scala runner will try to run the named target, either as
a compiled class with a main method, a jar file with a Main-Class manifest
header, or as a Scala source file to compile and run.

The REPL accepts expressions to evaluate. Try `:help` to see more commands.

The script runner will invoke the main method of a top-level object if
it finds one; otherwise, the script code is run locally to a synthetic
main method with arguments available in a variable `args`.

Options to $cmdName which reach the Java runtime:

 -Dname=prop  passed directly to Java to set system properties
 -J<arg>      -J is stripped and <arg> passed to Java as-is
 -nobootcp    do not put the Scala jars on the boot classpath (slower)

Other startup options:

 -i <file>    preload <file> before starting the REPL
 -I <file>    preload <file>, enforcing line-by-line interpretation
 -e <string>  execute <string> as if it were in a source file
 -save        save the compiled script in a jar for future use

If the runner does not correctly guess how to run the target:

 -howtorun    what to run <script|object|jar|repl|guess> (default: guess)
%n"""
}

object GenericRunnerCommand {
  sealed abstract class HowToRun(val name: String)
  case object AsJar extends HowToRun("jar")
  case object AsObject extends HowToRun("object")
  case object AsScript extends HowToRun("script")
  case object AsRepl extends HowToRun("repl")
  case object Error extends HowToRun("<error>")
  val waysToRun = List(AsJar, AsObject, AsScript, AsRepl)
}
