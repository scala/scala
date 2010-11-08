/* NSC -- new Scala compiler
 * Copyright 2006-2010 LAMP/EPFL
 * @author  Lex Spoon
 */


package scala.tools.nsc

import java.io.IOException
import java.lang.{ClassNotFoundException, NoSuchMethodException}
import java.net.{ URL, MalformedURLException }
import scala.tools.util.PathResolver

import io.{ File, Process }
import util.{ ClassPath, ScalaClassLoader, waitingForThreads }
import Properties.{ versionString, copyrightString }

/** An object that runs Scala code.  It has three possible
  * sources for the code to run: pre-compiled code, a script file,
  * or interactive entry.
  */
object MainGenericRunner {
  def errorFn(ex: Throwable): Boolean = {
    ex.printStackTrace()
    false
  }
  def errorFn(str: String): Boolean = {
    Console println str
    false
  }

  def main(args: Array[String]) {
    if (!process(args))
      exit(1)
  }

  def process(args: Array[String]): Boolean = {
    val command = new GenericRunnerCommand(args.toList, (x: String) => errorFn(x))
    import command.settings
    def sampleCompiler = new Global(settings)   // def so its not created unless needed

    if (!command.ok)                      return errorFn("%s\n%s".format(command.usageMsg, sampleCompiler.pluginOptionsHelp))
    else if (settings.version.value)      return errorFn("Scala code runner %s -- %s".format(versionString, copyrightString))
    else if (command.shouldStopWithInfo)  return errorFn(command getInfoMessage sampleCompiler)

    def isE   = !settings.execute.isDefault
    def dashe = settings.execute.value

    def isI   = !settings.loadfiles.isDefault
    def dashi = settings.loadfiles.value

    def combinedCode  = {
      val files   = if (isI) dashi map (file => File(file).slurp()) else Nil
      val str     = if (isE) List(dashe) else Nil

      files ++ str mkString "\n\n"
    }

    val classpath: List[URL] = new PathResolver(settings) asURLs

    /** Was code given in a -e argument? */
    if (isE) {
      /** If a -i argument was also given, we want to execute the code after the
       *  files have been included, so they are read into strings and prepended to
       *  the code given in -e.  The -i option is documented to only make sense
       *  interactively so this is a pretty reasonable assumption.
       *
       *  This all needs a rewrite though.
       */
      val fullArgs = command.thingToRun.toList ::: command.arguments

      return ScriptRunner.runCommand(settings, combinedCode, fullArgs)
    }
    else command.thingToRun match {
      case None             =>
        // We start the repl when no arguments are given.
        new InterpreterLoop main settings
        true  // not actually reached in general

      case Some(thingToRun) =>
        val isObjectName =
          settings.howtorun.value match {
            case "object" => true
            case "script" => false
            case "guess"  => ScalaClassLoader.classExists(classpath, thingToRun)
          }

        if (isObjectName) {
          ObjectRunner.runAndCatch(classpath, thingToRun, command.arguments) match {
            case Left(ex) => errorFn(ex)
            case _        => true
          }
        }
        else {
          ScriptRunner.runScriptAndCatch(settings, thingToRun, command.arguments) match {
            case Left(ex) => errorFn(ex)
            case Right(b) => b
          }
        }
    }
  }
}
