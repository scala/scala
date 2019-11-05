/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package xsbt

import xsbti.Logger
import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.interpreter.shell.{ ILoop, ShellConfig, ReplReporterImpl }
import scala.tools.nsc.{ GenericRunnerCommand, Settings }

class ConsoleInterface {
  def commandArguments(
      args: Array[String],
      bootClasspathString: String,
      classpathString: String,
      log: Logger
  ): Array[String] =
    MakeSettings.sync(args, bootClasspathString, classpathString, log).recreateArgs.toArray[String]

  def run(
      args: Array[String],
      bootClasspathString: String,
      classpathString: String,
      initialCommands: String,
      cleanupCommands: String,
      loader: ClassLoader,
      bindNames: Array[String],
      bindValues: Array[Any],
      log: Logger
  ): Unit = {
    lazy val interpreterSettings = MakeSettings.sync(args.toList, log)
    val compilerSettings = MakeSettings.sync(args, bootClasspathString, classpathString, log)

    log.info(Message("Starting scala interpreter..."))
    log.info(Message(""))

    val loop = new ILoop(ShellConfig(interpreterSettings)) {
      override def createInterpreter(interpreterSettings: Settings) = {
        if (loader ne null) {
          val reporter = new ReplReporterImpl(interpreterSettings)
          intp = new IMain(interpreterSettings, reporter) {
            override protected def parentClassLoader =
              if (loader eq null) super.parentClassLoader
              else loader
          }
          intp.setContextClassLoader()
        } else
          super.createInterpreter(interpreterSettings)

        for ((id, value) <- bindNames zip bindValues) {
          intp.beQuietDuring {
            intp.bind(id, value.asInstanceOf[AnyRef].getClass.getName, value)
            ()
          }
        }

        if (!initialCommands.isEmpty)
          intp.interpret(initialCommands)

        ()
      }

      override def closeInterpreter(): Unit = {
        if (!cleanupCommands.isEmpty)
          intp.interpret(cleanupCommands)
        super.closeInterpreter()
      }
    }

    loop.run(compilerSettings)
    ()
  }
}

object MakeSettings {
  def apply(args: List[String], log: Logger): Settings = {
    val command = new GenericRunnerCommand(args, message => log.error(Message(message)))
    if (command.ok)
      command.settings
    else
      throw new InterfaceCompileFailed(Array(), Array(), command.usageMsg)
  }

  def sync(
      args: Array[String],
      bootClasspathString: String,
      classpathString: String,
      log: Logger
  ): Settings = {
    val compilerSettings = sync(args.toList, log)
    if (!bootClasspathString.isEmpty)
      compilerSettings.bootclasspath.value = bootClasspathString
    compilerSettings.classpath.value = classpathString
    compilerSettings
  }

  def sync(options: List[String], log: Logger): Settings = {
    val settings = apply(options, log)
    settings.Yreplsync.value = true
    settings
  }
}
