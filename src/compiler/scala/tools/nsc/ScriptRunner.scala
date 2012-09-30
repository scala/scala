/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import io.{ Directory, File, Path }
import java.io.IOException
import java.net.URL
import scala.tools.nsc.reporters.{Reporter,ConsoleReporter}
import util.Exceptional.unwrap

/** An object that runs Scala code in script files.
 *
 *  <p>For example, here is a complete Scala script on Unix:</pre>
 *  <pre>
 *    #!/bin/sh
 *    exec scala "$0" "$@"
 *    !#
 *    Console.println("Hello, world!")
 *    argv.toList foreach Console.println
 *  </pre>
 *  <p>And here is a batch file example on Windows XP:</p>
 *  <pre>
 *    ::#!
 *    @echo off
 *    call scala %0 %*
 *    goto :eof
 *    ::!#
 *    Console.println("Hello, world!")
 *    argv.toList foreach Console.println
 *  </pre>
 *
 *  @author  Lex Spoon
 *  @version 1.0, 15/05/2006
 *  @todo    It would be better if error output went to stderr instead
 *           of stdout...
 */
class ScriptRunner extends HasCompileSocket {
  lazy val compileSocket = CompileSocket

  /** Default name to use for the wrapped script */
  val defaultScriptMain = "Main"

  /** Pick a main object name from the specified settings */
  def scriptMain(settings: Settings) = settings.script.value match {
    case "" => defaultScriptMain
    case x  => x
  }

  def isScript(settings: Settings) = settings.script.value != ""

  /** Choose a jar filename to hold the compiled version of a script. */
  private def jarFileFor(scriptFile: String)= File(
    if (scriptFile endsWith ".jar") scriptFile
    else scriptFile.stripSuffix(".scala") + ".jar"
  )

  /** Read the entire contents of a file as a String. */
  private def contentsOfFile(filename: String) = File(filename).slurp()

  /** Split a fully qualified object name into a
   *  package and an unqualified object name */
  private def splitObjectName(fullname: String): (Option[String], String) =
    (fullname lastIndexOf '.') match {
      case -1   => (None, fullname)
      case idx  => (Some(fullname take idx), fullname drop (idx + 1))
    }

  /** Compile a script using the fsc compilation daemon.
   */
  private def compileWithDaemon(settings: GenericRunnerSettings, scriptFileIn: String) = {
    val scriptFile       = Path(scriptFileIn).toAbsolute.path
    val compSettingNames = new Settings(sys.error).visibleSettings.toList map (_.name)
    val compSettings     = settings.visibleSettings.toList filter (compSettingNames contains _.name)
    val coreCompArgs     = compSettings flatMap (_.unparse)
    val compArgs         = coreCompArgs ++ List("-Xscript", scriptMain(settings), scriptFile)

    CompileSocket getOrCreateSocket "" match {
      case Some(sock) => compileOnServer(sock, compArgs)
      case _          => false
    }
  }

  protected def newGlobal(settings: Settings, reporter: Reporter) =
    Global(settings, reporter)

  /** Compile a script and then run the specified closure with
    * a classpath for the compiled script.
    *
    * @return true if compilation and the handler succeeds, false otherwise.
    */
  private def withCompiledScript(
    settings: GenericRunnerSettings,
    scriptFile: String)
    (handler: String => Boolean): Boolean =
  {
    def mainClass = scriptMain(settings)

    /** Compiles the script file, and returns the directory with the compiled
     *  class files, if the compilation succeeded.
     */
    def compile: Option[Directory] = {
      val compiledPath = Directory makeTemp "scalascript"

      // delete the directory after the user code has finished
      sys.addShutdownHook(compiledPath.deleteRecursively())

      settings.outdir.value = compiledPath.path

      if (settings.nc.value) {
        /** Setting settings.script.value informs the compiler this is not a
         *  self contained compilation unit.
         */
        settings.script.value = mainClass
        val reporter = new ConsoleReporter(settings)
        val compiler = newGlobal(settings, reporter)

        new compiler.Run compile List(scriptFile)
        if (reporter.hasErrors) None else Some(compiledPath)
      }
      else if (compileWithDaemon(settings, scriptFile)) Some(compiledPath)
      else None
    }

    /** The script runner calls sys.exit to communicate a return value, but this must
     *  not take place until there are no non-daemon threads running.  Tickets #1955, #2006.
     */
    util.waitingForThreads {
      if (settings.save.value) {
        val jarFile = jarFileFor(scriptFile)
        def jarOK   = jarFile.canRead && (jarFile isFresher File(scriptFile))

        def recompile() = {
          jarFile.delete()

          compile match {
            case Some(compiledPath) =>
              try io.Jar.create(jarFile, compiledPath, mainClass)
              catch { case _: Exception => jarFile.delete() }

              if (jarOK) {
                compiledPath.deleteRecursively()
                handler(jarFile.toAbsolute.path)
              }
              // jar failed; run directly from the class files
              else handler(compiledPath.path)
            case _  => false
          }
        }

        if (jarOK) handler(jarFile.toAbsolute.path) // pre-compiled jar is current
        else recompile()                            // jar old - recompile the script.
      }
      // don't use a cache jar at all--just use the class files
      else compile exists (cp => handler(cp.path))
    }
  }

  /** Run a script after it has been compiled
   *
   * @return true if execution succeeded, false otherwise
   */
  private def runCompiled(
    settings: GenericRunnerSettings,
    compiledLocation: String,
    scriptArgs: List[String]): Boolean =
  {
    val cp = File(compiledLocation).toURL +: settings.classpathURLs
    ObjectRunner.runAndCatch(cp, scriptMain(settings), scriptArgs) match {
      case Left(ex) => ex.printStackTrace() ; false
      case _        => true
    }
  }

  /** Run a script file with the specified arguments and compilation
   *  settings.
   *
   * @return true if compilation and execution succeeded, false otherwise.
   */
  def runScript(
    settings: GenericRunnerSettings,
    scriptFile: String,
    scriptArgs: List[String]): Boolean =
  {
    if (File(scriptFile).isFile)
      withCompiledScript(settings, scriptFile) { runCompiled(settings, _, scriptArgs) }
    else
      throw new IOException("no such file: " + scriptFile)
  }

  /** Calls runScript and catches the enumerated exceptions, routing
   *  them to Left(ex) if thrown.
   */
  def runScriptAndCatch(
    settings: GenericRunnerSettings,
    scriptFile: String,
    scriptArgs: List[String]): Either[Throwable, Boolean] =
  {
    try Right(runScript(settings, scriptFile, scriptArgs))
    catch { case e: Throwable => Left(unwrap(e)) }
  }

  /** Run a command
   *
   * @return true if compilation and execution succeeded, false otherwise.
   */
  def runCommand(
    settings: GenericRunnerSettings,
    command: String,
    scriptArgs: List[String]): Boolean =
  {
    val scriptFile = File.makeTemp("scalacmd", ".scala")
    // save the command to the file
    scriptFile writeAll command

    try withCompiledScript(settings, scriptFile.path) { runCompiled(settings, _, scriptArgs) }
    finally scriptFile.delete()  // in case there was a compilation error
  }
}

object ScriptRunner extends ScriptRunner { }
