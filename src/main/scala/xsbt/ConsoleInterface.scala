/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbt

import java.io.{ PrintWriter, StringWriter }

import xsbti.Logger
import ConsoleHelper._

import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.{ GenericRunnerCommand, Settings }

class ConsoleInterface(args: Array[String], bootClasspathString: String,
  classpathString: String, initialCommands: String, cleanupCommands: String,
  loader: ClassLoader, bindNames: Array[String], bindValues: Array[AnyRef],
  log: Logger) extends xsbti.ConsoleInterface {
  lazy val interpreterSettings = MakeSettings.sync(args.toList, { message => log.error(Message(message)) })
  // we need rt.jar from JDK, so java classpath is required
  val useJavaCp = "-usejavacp"
  val compilerSettings = MakeSettings.sync(args :+ useJavaCp, bootClasspathString, classpathString, { message => log.error(Message(message)) })
  if (!bootClasspathString.isEmpty)
    compilerSettings.bootclasspath.value = bootClasspathString
  compilerSettings.classpath.value = classpathString
  val outWriter: StringWriter = new StringWriter
  val poutWriter: PrintWriter = new PrintWriter(outWriter)

  val interpreter: IMain = new IMain(compilerSettings, new PrintWriter(outWriter)) {
    def lastReq = prevRequestList.last
  }

  override def interpret(line: String, synthetic: Boolean): ConsoleResponse =
    {
      clearBuffer()
      val r = interpreter.interpret(line, synthetic)
      ConsoleResponse(r, outWriter.toString)
    }
  def clearBuffer(): Unit = {
    // errorWriter.getBuffer.setLength(0)
    outWriter.getBuffer.setLength(0)
  }

  def reset(): Unit = {
    clearBuffer()
    interpreter.reset()
  }
}

object MakeSettings {
  def apply(args: List[String], onError: String => Unit) =
    {
      val command = new GenericRunnerCommand(args, onError(_))
      if (command.ok) command.settings
      // TODO: Provide better exception
      else throw new Exception(command.usageMsg)
    }

  def sync(args: Array[String], bootClasspathString: String, classpathString: String, onError: String => Unit): Settings =
    {
      val compilerSettings = sync(args.toList, onError)
      if (!bootClasspathString.isEmpty)
        compilerSettings.bootclasspath.value = bootClasspathString
      compilerSettings.classpath.value = classpathString
      compilerSettings
    }

  def sync(options: List[String], onError: String => Unit) = {
    val settings = apply(options, onError)
    settings.Yreplsync.value = true
    settings
  }
}
