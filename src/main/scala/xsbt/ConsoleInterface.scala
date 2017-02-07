/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbt

import xsbti.Logger
import scala.tools.nsc.{ GenericRunnerCommand, Interpreter, ObjectRunner, Settings }
import scala.tools.nsc.interpreter.{ IMain, InteractiveReader, ILoop }
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.util.ClassPath

class ConsoleInterface {
  def commandArguments(args: Array[String], bootClasspathString: String, classpathString: String, log: Logger): Array[String] =
    MakeSettings.sync(args, bootClasspathString, classpathString, log).recreateArgs.toArray[String]

  def run(args: Array[String], bootClasspathString: String, classpathString: String, initialCommands: String, cleanupCommands: String, loader: ClassLoader, bindNames: Array[String], bindValues: Array[Any], log: Logger): Unit = {
    lazy val interpreterSettings = MakeSettings.sync(args.toList, log)
    val compilerSettings = MakeSettings.sync(args, bootClasspathString, classpathString, log)

    if (!bootClasspathString.isEmpty)
      compilerSettings.bootclasspath.value = bootClasspathString
    compilerSettings.classpath.value = classpathString
    log.info(Message("Starting scala interpreter..."))
    log.info(Message(""))
    val loop = new ILoop {

      override def createInterpreter() = {

        if (loader ne null) {
          in = InteractiveReader.apply()
          intp = new IMain(settings) {
            override protected def parentClassLoader = if (loader eq null) super.parentClassLoader else loader
            override protected def newCompiler(settings: Settings, reporter: Reporter) = super.newCompiler(compilerSettings, reporter)
          }
          intp.setContextClassLoader()
        } else
          super.createInterpreter()

        def bind(values: Seq[(String, Any)]): Unit = {
          // for 2.8 compatibility
          final class Compat {
            def bindValue(id: String, value: Any) =
              intp.bind(id, value.asInstanceOf[AnyRef].getClass.getName, value)
          }
          implicit def compat(a: AnyRef): Compat = new Compat

          for ((id, value) <- values)
            intp.beQuietDuring(intp.bindValue(id, value))
        }

        bind(bindNames zip bindValues)

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
  def apply(args: List[String], log: Logger) =
    {
      val command = new GenericRunnerCommand(args, message => log.error(Message(message)))
      if (command.ok)
        command.settings
      else
        throw new InterfaceCompileFailed(Array(), Array(), command.usageMsg)
    }

  def sync(args: Array[String], bootClasspathString: String, classpathString: String, log: Logger): Settings =
    {
      val compilerSettings = sync(args.toList, log)
      if (!bootClasspathString.isEmpty)
        compilerSettings.bootclasspath.value = bootClasspathString
      compilerSettings.classpath.value = classpathString
      compilerSettings
    }

  def sync(options: List[String], log: Logger) =
    {
      val settings = apply(options, log)

      // -Yrepl-sync is only in 2.9.1+
      final class Compat {
        def Yreplsync = settings.BooleanSetting("-Yrepl-sync", "For compatibility only.")
      }
      implicit def compat(s: Settings): Compat = new Compat

      settings.Yreplsync.value = true
      settings
    }
}
