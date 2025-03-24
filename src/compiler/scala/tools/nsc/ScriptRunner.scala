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

import scala.reflect.io.{ AbstractFile, Directory, File, Path }
import scala.tools.nsc.classpath.ClassPathFactory
import scala.tools.nsc.io.Jar
import scala.tools.nsc.reporters.{ ConsoleReporter, Reporter }
import scala.util.chaining._
import scala.util.control.NonFatal
import java.io.IOException

/** An object that runs Scala code in script files.
 *
 *  For example, here is a complete Scala script on Unix:
 *  {{{
 *    #!/bin/sh
 *    exec scala "\$0" "\$@"
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
    val reporter = new ConsoleReporter(settings)
    val compiler = newGlobal(settings, reporter)
    if (settings.pastefiles.value.nonEmpty) new compiler.Run().compile(settings.pastefiles.value)
    // Setting settings.script.value informs the compiler this is not a self-contained compilation unit.
    settings.script.value = mainClass
    new compiler.Run().compile(scriptFile :: Nil)
    !reporter.hasErrors
  }

  protected def newGlobal(settings: Settings, reporter: Reporter) = Global(settings, reporter)
}

abstract class AbstractScriptRunner(settings: GenericRunnerSettings) extends ScriptRunner {

  /** Do compile the given script file, returning true for success. */
  protected def doCompile(scriptFile: String): Boolean

  protected final def mainClass: String = settings.script.value

  /** Compile a script and then run the specified closure with
   *  a classpath for the compiled script.
   *
   *  @return true if compilation and the handler succeeds, false otherwise.
   */
  private def withCompiledScript(scriptFile: String)(handler: String => Option[Throwable]): Option[Throwable] = {

    /* Compiles the script file, with the output set to either
     * the user-specified location (jar or dir), or a temp dir.
     * Returns the output location on success.
     */
    def compile: Option[Path] = {
      val outpath =
        if (settings.outdir.isSetByUser)
          Path(settings.outdir.value)
        else
          Directory.makeTemp("scalascript").tap { tmp =>
            // delete the directory after the user code has finished
            Runtime.getRuntime.addShutdownHook(new Thread(() => tmp.deleteRecursively()))
            settings.outdir.value = tmp.path
          }

      if (doCompile(scriptFile)) Some(outpath) else None
    }

    def hasClassToRun(location: Path): Boolean = {
      val cp = ClassPathFactory.newClassPath(AbstractFile.getDirectory(location), settings)
      cp.findClass(mainClass).isDefined
    }

    // under -save, compile to a jar, specified either by -d or based on script name.
    // if -d specifies a dir, assemble the jar by hand.
    def withLatestJar(): Option[Throwable] = {
      val outputToJar = settings.outdir.value.endsWith(".jar")
      def stripped = List(".scala", ".sc").find(scriptFile.endsWith).map(scriptFile.stripSuffix).getOrElse(scriptFile)
      val jarFile = File(
        if (outputToJar) settings.outdir.value
        else s"$stripped.jar".tap(j => if (!settings.outdir.isSetByUser) settings.outdir.value = j)
      )
      def jarOK = jarFile.canRead && jarFile.isFresher(File(scriptFile))

      def recompile(): Option[Throwable] = {
        jarFile.delete()

        compile match {
          case Some(compiledPath) =>
            if (hasClassToRun(compiledPath)) {
              // user -d mydir -save means assemble script.jar, don't delete mydir
              if (!Jar.isJarOrZip(compiledPath)) {
                try {
                  Jar.create(jarFile, compiledPath.toDirectory, mainClass)
                  None
                } catch {
                  case NonFatal(e) => jarFile.delete() ; Some(e)
                }
              } else None
            } else Some(NoScriptError)
          case _  => Some(ScriptCompileError)
        }
      }

      val err = if (!jarOK) recompile() else None
      err orElse handler(jarFile.toAbsolute.path) filterNot { case NoScriptError => true case _ => false }
    }

    /* The script runner calls System.exit to communicate a return value, but this must
     * not take place until there are no non-daemon threads running.  Tickets #1955, #2006.
     */
    util.waitingForThreads {
      // either update the jar or don't use a cache jar at all, just use the class files, if they exist
      if (settings.save.value) withLatestJar()
      else {
        compile match {
          case Some(cp) if hasClassToRun(cp) => handler(cp.path)
          case Some(_)                       => None
          case _                             => Some(ScriptCompileError)
        }
      }
    }
  }

  /** Run a script after it has been compiled. Prints any exceptions.
   *
   * @return true if execution succeeded, false otherwise
   */
  private def runCompiled(compiledLocation: String, scriptArgs: List[String]): Option[Throwable] = {
    val cp = File(compiledLocation).toURL +: settings.classpathURLs
    ObjectRunner.runAndCatch(cp, mainClass, scriptArgs)
  }

  final def runScript(scriptFile: String, scriptArgs: List[String]): Option[Throwable] = {
    val f = File(scriptFile)
    def usingCompilationServer = settings.Yscriptrunner.valueSetByUser.map(_ != "default").getOrElse(false)
    if (!f.exists) Some(new IOException(s"no such file: $scriptFile"))
    else if (!f.canRead) Some(new IOException(s"can't read: $scriptFile"))
    else if (f.isDirectory) Some(new IOException(s"can't compile a directory: $scriptFile"))
    else if (!f.isFile && usingCompilationServer) Some(new IOException(s"compile server requires a regular file: $scriptFile"))
    else withCompiledScript(scriptFile) { runCompiled(_, scriptArgs) }
  }

  final def runScriptText(command: String, scriptArgs: List[String]): Option[Throwable] = {
    val scriptFile = File.makeTemp("scalacmd", ".scala")
    // save the command to the file
    scriptFile writeAll command

    try withCompiledScript(scriptFile.path) { runCompiled(_, scriptArgs) }
    catch {
      case NonFatal(e) => Some(e)
    }
    finally scriptFile.delete()  // in case there was a compilation error
  }
}

object ScriptRunner {
  import scala.reflect.internal.util.ScalaClassLoader

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

object ScriptCompileError extends scala.util.control.ControlThrowable
object NoScriptError extends scala.util.control.ControlThrowable
