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

import interpreter.shell.{ILoop, ShellConfig}

object JarRunner extends CommonRunner {
  def runJar(settings: GenericRunnerSettings, jarPath: String, arguments: Seq[String]): Option[Throwable] = {
    val jar       = new io.Jar(jarPath)
    val mainClass = jar.mainClass getOrElse (throw new IllegalArgumentException(s"Cannot find main class for jar: $jarPath"))
    val jarURLs   = util.ClassPath expandManifestPath jarPath
    val urls      = if (jarURLs.isEmpty) io.File(jarPath).toURL +: settings.classpathURLs else jarURLs

    if (settings.Ylogcp.value) {
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
    if (str.nonEmpty) Console.err.println(str)
    e.foreach(_.printStackTrace())
    !isFailure
  }

  def process(args: Array[String]): Boolean = {
    val command = new GenericRunnerCommand(args.toList, (x: String) => errorFn(x))
    import command.{settings, howToRun, thingToRun, shortUsageMsg}
    import MainGenericRunner.CommandFailure

    // only created for info message
    def sampleCompiler = new Global(settings)

    def run(): Boolean = {
      def isE   = settings.execute.isSetByUser
      def dashe = settings.execute.value

      // when -e expr -howtorun script, read any -i or -I files and append expr
      // the result is saved to a tmp script file and run
      def combinedCode  = {
        val files   =
          for {
            dashi <- List(settings.loadfiles, settings.pastefiles) if dashi.isSetByUser
            path  <- dashi.value
          } yield io.File(path).slurp()

        (files :+ dashe).mkString("\n\n")
      }

      import GenericRunnerCommand.{AsObject, AsScript, AsJar, Error}
      def runTarget(): Option[Throwable] = howToRun match {
        case AsObject =>
          ObjectRunner.runAndCatch(settings.classpathURLs, thingToRun, command.arguments)
        case AsScript if isE =>
          ScriptRunner(settings).runScriptText(combinedCode, thingToRun +: command.arguments)
        case AsScript =>
          ScriptRunner(settings).runScript(thingToRun, command.arguments)
        case AsJar    =>
          JarRunner.runJar(settings, thingToRun, command.arguments)
        case Error =>
          Some(CommandFailure)
        case _  =>
          // We start the repl when no arguments are given.
          if (settings.Wconf.isDefault && settings.lint.isDefault) {
            // If user is agnostic about -Wconf and -Xlint, enable -deprecation and -feature
            settings.deprecation.value = true
            settings.feature.value = true
          }
          val config = ShellConfig(settings)
          new ILoop(config).run(settings)
          None
      }

      runTarget() match {
        case Some(ScriptCompileError) => false
        case Some(CommandFailure) => false
        case e @ Some(ex) => errorFn("", e)
        case _            => true
      }
    }

    if (!command.ok)
      errorFn(f"%n$shortUsageMsg")
    else if (command.shouldStopWithInfo)
      errorFn(command.getInfoMessage(sampleCompiler), isFailure = false)
    else
      run()
  }
}

object MainGenericRunner extends MainGenericRunner {
  // control indicating command ran but non-zero exit
  object CommandFailure extends scala.util.control.ControlThrowable("Command failed")

  def main(args: Array[String]): Unit = if (!process(args)) System.exit(1)
}
