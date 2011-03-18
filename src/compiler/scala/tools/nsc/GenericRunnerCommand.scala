/* NSC -- new Scala compiler
 * Copyright 2007 LAMP/EPFL
 * @author  Lex Spoon
 */

package scala.tools.nsc

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
  override val cmdName = "scala"
  val compCmdName = "scalac"

  // change CompilerCommand behavior
  override def shouldProcessArguments: Boolean = false

  /** thingToRun: What to run.  If it is None, then the interpreter should be started
   *  arguments: Arguments to pass to the object or script to run
   */
  val (_ok, thingToRun, arguments) = {
    val (ok, remaining) = settings.processArguments(args, false)
    val mainClass =
      if (settings.jarfile.isDefault) None
      else new io.Jar(settings.jarfile.value).mainClass

    // If there is a jar with a main class, the remaining args are passed to that.
    // Otherwise, the first remaining argument is the program to run, and the rest
    // of the arguments go to it.  If remaining is empty, we'll start the repl.
    mainClass match {
      case Some(name) => (ok, Some(name), remaining)
      case _          => (ok, remaining.headOption, remaining drop 1)
    }
  }
  override def ok = _ok

  private def interpolate(s: String) = s.trim.replaceAll("@cmd@", cmdName).replaceAll("@compileCmd@", compCmdName) + "\n"

  def shortUsageMsg = interpolate("""
Usage: @cmd@ <options> [<script|class|object> <arguments>]
   or  @cmd@ <options> [-jar <jarfile> <arguments>]
   or  @cmd@ -help

All options to @compileCmd@ are also allowed.  See @compileCmd@ -help.
  """)

  override def usageMsg = shortUsageMsg + "\n" + interpolate("""
The first given argument other than options to @cmd@ designates
what to run.  Runnable targets are:

  - a file containing scala source
  - the name of a compiled class
  - a runnable jar file with a Main-Class attribute (if -jar is given)
  - if no argument is given, the repl (interactive shell) is started

Options to the runner which reach the java runtime:

 -Dname=prop  passed directly to java to set system properties
 -J<arg>      -J is stripped and <arg> passed to java as-is
 -nobootcp    do not put the scala jars on the boot classpath (slower)

Other scala startup options:

 -howtorun      specify what to run <script|object|guess> (default: guess)
 -i <file>      preload <file> before starting the repl
 -e <string>    execute <string> as if entered in the repl
 -nc            no compilation daemon: do not use the fsc offline compiler
 -savecompiled  save the compiled script in a jar for future use

A file argument will be run as a scala script unless it contains only top
level classes and objects, and exactly one runnable main method.  In that
case the file will be compiled and the main method invoked.  This provides
a bridge between scripts and standard scala source.
  """) + "\n"
}
