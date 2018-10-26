/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc

import scala.reflect.io.{Directory, File}
import scala.tools.nsc.classpath.DirectoryClassPath
import scala.tools.nsc.reporters.{Reporter,ConsoleReporter}
import scala.util.control.NonFatal
import java.io.IOException

/** An object that runs Scala code in script files.
 *
 *  For example, here is a complete Scala script on Unix:
 *  {{{
 *    #!/bin/sh
 *    exec scala "$0" "$@"
 *    !#
 *    Console.println("Hello, world!")
 *    args.toList foreach Console.println
 *  }}}
 *  And here is a batch file example on Windows XP:
 *  {{{
 *    ::#!
 *    @echo off
 *    call scala %0 %*
 *    goto :eof
 *    ::!#
 *    Console.println("Hello, world!")
 *    args.toList foreach Console.println
 *  }}}
 *
 *  @author  Lex Spoon
 *  @version 1.0, 15/05/2006
 *  @todo    It would be better if error output went to stderr instead
 *           of stdout...
 */
trait ScriptRunner {
  /** Run a script file by name, with the given arguments.
   *  @return optionally an error, None for success
   */
  def runScript(script: String, scriptArgs: List[String]): Option[Throwable]

  /** Run the script text as supplied, with the given arguments.
   *  @return optionally an error, None for success
   */
  def runScriptText(script: String, scriptArgs: List[String]): Option[Throwable]
}

class DefaultScriptRunner(settings: GenericRunnerSettings) extends AbstractScriptRunner(settings) {
  protected def doCompile(scriptFile: String) = {
    // Setting settings.script.value informs the compiler this is not a self-contained compilation unit.
    settings.script.value = mainClass
    val reporter = new ConsoleReporter(settings)
    val compiler = newGlobal(settings, reporter)
    val run      = new compiler.Run
    run.compile(List(scriptFile))
    !reporter.hasErrors
  }

  protected def newGlobal(settings: Settings, reporter: Reporter) = Global(settings, reporter)
}

abstract class AbstractScriptRunner(settings: GenericRunnerSettings) extends ScriptRunner {

  /** Do compile the given script file, returning true for success. */
  protected def doCompile(scriptFile: String): Boolean

  protected final def mainClass = ScriptRunner.scriptMain(settings)

  /** Compile a script and then run the specified closure with
   *  a classpath for the compiled script.
   *
   *  @return true if compilation and the handler succeeds, false otherwise.
   */
  private def withCompiledScript(scriptFile: String)(handler: String => Boolean): Boolean = {

    /* Compiles the script file, and returns the directory with the compiled
     * class files, if the compilation succeeded.
     */
    def compile: Option[Directory] = {
      val compiledPath = Directory.makeTemp("scalascript")

      // delete the directory after the user code has finished
      Runtime.getRuntime.addShutdownHook(new Thread(() => compiledPath.deleteRecursively()))

      settings.outdir.value = compiledPath.path

      if (doCompile(scriptFile)) Some(compiledPath) else None
    }

    def hasClassToRun(d: Directory): Boolean = DirectoryClassPath(d.jfile).findClass(mainClass).isDefined

    def withLatestJar(): Boolean = {
      /** Choose a jar filename to hold the compiled version of a script. */
      def jarFileFor(scriptFile: String) = File(
        if (scriptFile endsWith ".jar") scriptFile
        else scriptFile.stripSuffix(".scala") + ".jar"
      )
      val jarFile = jarFileFor(scriptFile)
      def jarOK   = jarFile.canRead && (jarFile isFresher File(scriptFile))

      def recompile() = {
        jarFile.delete()

        compile match {
          case Some(compiledPath) =>
            if (!hasClassToRun(compiledPath)) {
              // it compiled ok, but there is nothing to run;
              // running an empty script should succeed
              true
            } else {
              try io.Jar.create(jarFile, compiledPath, mainClass)
              catch { case NonFatal(_) => jarFile.delete() }

              if (jarOK) {
                compiledPath.deleteRecursively()
                handler(jarFile.toAbsolute.path)
              }
              // jar failed; run directly from the class files
              else handler(compiledPath.path)
            }
          case _  => false
        }
      }

      if (jarOK) handler(jarFile.toAbsolute.path) // pre-compiled jar is current
      else recompile()                            // jar old - recompile the script.
    }

    /* The script runner calls System.exit to communicate a return value, but this must
     * not take place until there are no non-daemon threads running.  Tickets #1955, #2006.
     */
    util.waitingForThreads {
      // either update the jar or don't use a cache jar at all, just use the class files, if they exist
      if (settings.save) withLatestJar()
      else compile.exists(cp => !hasClassToRun(cp) || handler(cp.path))
    }
  }

  /** Run a script after it has been compiled. Prints any exceptions.
   *
   * @return true if execution succeeded, false otherwise
   */
  private def runCompiled(compiledLocation: String, scriptArgs: List[String]): Boolean = {
    val cp = File(compiledLocation).toURL +: settings.classpathURLs
    ObjectRunner.runAndCatch(cp, mainClass, scriptArgs) match {
      case Some(e) => e.printStackTrace() ; false
      case _       => true
    }
  }

  final def runScript(scriptFile: String, scriptArgs: List[String]): Option[Throwable] = {
    val f = File(scriptFile)
    if (!f.exists) Some(new IOException(s"no such file: $scriptFile"))
    else if (!f.canRead) Some(new IOException(s"can't read: $scriptFile"))
    else if (f.isDirectory) Some(new IOException(s"can't compile a directory: $scriptFile"))
    else if (!settings.nc && !f.isFile) Some(new IOException(s"compile server requires a regular file: $scriptFile"))
    else {
      withCompiledScript(scriptFile) { runCompiled(_, scriptArgs) }
      None
    }
  }

  final def runScriptText(command: String, scriptArgs: List[String]): Option[Throwable] = {
    val scriptFile = File.makeTemp("scalacmd", ".scala")
    // save the command to the file
    scriptFile writeAll command

    try {
      withCompiledScript(scriptFile.path) { runCompiled(_, scriptArgs) }
      None
    }
    catch {
      case NonFatal(e) => Some(e)
    }
    finally scriptFile.delete()  // in case there was a compilation error
  }
}

object ScriptRunner {
  import scala.reflect.internal.util.ScalaClassLoader

  /** Default name to use for the wrapped script */
  val defaultScriptMain = "Main"

  /** Pick a main object name from the specified settings */
  def scriptMain(settings: Settings) = settings.script.value match {
    case "" => defaultScriptMain
    case x  => x
  }

  def apply(settings: GenericRunnerSettings): ScriptRunner =
    settings.Yscriptrunner.value match {
      case "default"  => new DefaultScriptRunner(settings)
      case "resident" => new fsc.ResidentScriptRunner(settings)
      case "shutdown" => new fsc.DaemonKiller(settings)
      case custom =>
        val loader = new ClassLoader(getClass.getClassLoader) with ScalaClassLoader
        loader.create[ScriptRunner](custom, settings.errorFn)(settings)
    }
}
