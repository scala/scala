/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package tools.nsc

import io.{ AbstractFile, Directory, File, Path }
import java.io.IOException
import scala.tools.nsc.classpath.DirectoryClassPath
import scala.tools.nsc.reporters.{Reporter,ConsoleReporter}
import util.Exceptional.unwrap

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
class ScriptRunner extends HasCompileSocket {
  lazy val compileSocket = CompileSocket

  /** Default name to use for the wrapped script */
  val defaultScriptMain = "Main"

  /** Pick a main object name from the specified settings */
  def scriptMain(settings: Settings) = settings.script.value match {
    case "" => defaultScriptMain
    case x  => x
  }

  /** Choose a jar filename to hold the compiled version of a script. */
  private def jarFileFor(scriptFile: String)= File(
    if (scriptFile endsWith ".jar") scriptFile
    else scriptFile.stripSuffix(".scala") + ".jar"
  )

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

    /* Compiles the script file, and returns the directory with the compiled
     * class files, if the compilation succeeded.
     */
    def compile: Option[Directory] = {
      val compiledPath = Directory makeTemp "scalascript"

      // delete the directory after the user code has finished
      sys.addShutdownHook(compiledPath.deleteRecursively())

      settings.outdir.value = compiledPath.path

      if (settings.nc) {
        /* Setting settings.script.value informs the compiler this is not a
         * self contained compilation unit.
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

    def hasClassToRun(d: Directory): Boolean = {
      val cp = DirectoryClassPath(d.jfile)
      cp.findClass(mainClass).isDefined
    }

    /* The script runner calls sys.exit to communicate a return value, but this must
     * not take place until there are no non-daemon threads running.  Tickets #1955, #2006.
     */
    util.waitingForThreads {
      if (settings.save) {
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
                catch { case _: Exception => jarFile.delete() }

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
      // don't use a cache jar at all--just use the class files, if they exist
      else compile exists (cp => !hasClassToRun(cp) || handler(cp.path))
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
