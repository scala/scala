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
import scala.tools.nsc.interpreter.{ ILoop, IMain, InteractiveReader, NamedParam }
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.{ GenericRunnerCommand, Settings }

class ConsoleBridge extends xsbti.compile.ConsoleInterface1 {
  override def commandArguments(
      args: Array[String],
      bootClasspathString: String,
      classpathString: String,
      log: Logger
  ): Array[String] =
    MakeSettings.sync(args, bootClasspathString, classpathString, log).recreateArgs.toArray[String]

  override def run(
      args: Array[String],
      bootClasspathString: String,
      classpathString: String,
      initialCommands: String,
      cleanupCommands: String,
      loader: ClassLoader,
      bindNames: Array[String],
      bindValues: Array[AnyRef],
      log: Logger
  ): Unit = {
    lazy val interpreterSettings = MakeSettings.sync(args.toList, log)
    val compilerSettings = MakeSettings.sync(args, bootClasspathString, classpathString, log)

    log.info(Message("Starting scala interpreter..."))
    log.info(Message(""))

    val loop = new ILoop {
      override def createInterpreter() = {
        if (loader ne null) {
          in = InteractiveReader.apply()
          intp = new IMain(settings) {
            override protected def parentClassLoader =
              if (loader eq null) super.parentClassLoader else loader

            override protected def newCompiler(settings: Settings, reporter: Reporter) =
              super.newCompiler(compilerSettings, reporter)
          }
        } else
          super.createInterpreter()

        for ((id, value) <- bindNames zip bindValues)
          intp.quietBind(NamedParam.clazz(id, value))

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

    loop.process(if (loader eq null) compilerSettings else interpreterSettings)

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
