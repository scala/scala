/* NSC -- new Scala compiler
 * Copyright 2006-2013 LAMP/EPFL
 * @author  Lex Spoon
 */

package scala
package tools.nsc

import io.File
import util.ClassPath
import GenericRunnerCommand._

object JarRunner extends CommonRunner {
  def runJar(settings: GenericRunnerSettings, jarPath: String, arguments: Seq[String]): Either[Throwable, Boolean] = {
    val jar       = new io.Jar(jarPath)
    val mainClass = jar.mainClass getOrElse sys.error("Cannot find main class for jar: " + jarPath)
    val jarURLs   = ClassPath expandManifestPath jarPath
    val urls      = if (jarURLs.isEmpty) File(jarPath).toURL +: settings.classpathURLs else jarURLs

    if (settings.Ylogcp) {
      Console.err.println("Running jar with these URLs as the classpath:")
      urls foreach println
    }

    runAndCatch(urls, mainClass, arguments)
  }
}

/** An object that runs Scala code.  It has three possible
 *  sources for the code to run: pre-compiled code, a script file,
 *  or interactive entry.
 */
class MainGenericRunner {
  def errorFn(str: String, e: Option[Throwable] = None, isFailure: Boolean = true): Boolean = {
    if (str.nonEmpty) Console.err println str
    e foreach (_.printStackTrace())
    !isFailure
  }

  def process(args: Array[String]): Boolean = {
    val command = new GenericRunnerCommand(args.toList, (x: String) => errorFn(x))
    import command.{ settings, howToRun, thingToRun, shortUsageMsg, shouldStopWithInfo }
    def sampleCompiler = new Global(settings)   // def so it's not created unless needed

    def run(): Boolean = {
      def isE   = !settings.execute.isDefault
      def dashe = settings.execute.value

      def isI   = !settings.loadfiles.isDefault
      def dashi = settings.loadfiles.value

      // Deadlocks on startup under -i unless we disable async.
      if (isI)
        settings.Yreplsync.value = true

      def combinedCode  = {
        val files   = if (isI) dashi map (file => File(file).slurp()) else Nil
        val str     = if (isE) List(dashe) else Nil

        files ++ str mkString "\n\n"
      }

      def runTarget(): Either[Throwable, Boolean] = howToRun match {
        case AsObject =>
          ObjectRunner.runAndCatch(settings.classpathURLs, thingToRun, command.arguments)
        case AsScript =>
          ScriptRunner.runScriptAndCatch(settings, thingToRun, command.arguments)
        case AsJar    =>
          JarRunner.runJar(settings, thingToRun, command.arguments)
        case Error =>
          Right(false)
        case _  =>
          // We start the repl when no arguments are given.
          // If user is agnostic about both -feature and -deprecation, turn them on.
          if (settings.deprecation.isDefault && settings.feature.isDefault) {
            settings.deprecation.value = true
            settings.feature.value = true
          }
          Right(new interpreter.ILoop process settings)
      }

      /** If -e and -i were both given, we want to execute the -e code after the
       *  -i files have been included, so they are read into strings and prepended to
       *  the code given in -e.  The -i option is documented to only make sense
       *  interactively so this is a pretty reasonable assumption.
       *
       *  This all needs a rewrite though.
       */
      if (isE) {
        ScriptRunner.runCommand(settings, combinedCode, thingToRun +: command.arguments)
      }
      else runTarget() match {
        case Left(ex) => errorFn("", Some(ex))  // there must be a useful message of hope to offer here
        case Right(b) => b
      }
    }

    if (!command.ok)
      errorFn(f"%n$shortUsageMsg")
    else if (shouldStopWithInfo)
      errorFn(command getInfoMessage sampleCompiler, isFailure = false)
    else
      run()
  }
}

object MainGenericRunner extends MainGenericRunner {
  def main(args: Array[String]): Unit = if (!process(args)) sys.exit(1)
}
