/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbt

import java.io.{ PrintWriter, StringWriter }

import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.{ GenericRunnerCommand, Settings }

import xsbti.Logger

import Compat._
import InteractiveConsoleHelper._

class InteractiveConsoleInterface(
    args: Array[String],
    bootClasspathString: String,
    classpathString: String,
    initialCommands: String,
    cleanupCommands: String,
    loader: ClassLoader,
    bindNames: Array[String],
    bindValues: Array[AnyRef],
    log: Logger
) extends xsbti.InteractiveConsoleInterface {

  lazy val interpreterSettings: Settings = InteractiveMakeSettings.sync(args.toList, onError)

  val useJavaCp = "-usejavacp" // we need rt.jar from JDK, so java classpath is required

  val compilerSettings: Settings =
    InteractiveMakeSettings.sync(args :+ useJavaCp, bootClasspathString, classpathString, onError)

  val outWriter: StringWriter = new StringWriter
  val poutWriter: PrintWriter = new PrintWriter(outWriter)

  val interpreter: IMain =
    new IMain(compilerSettings, replReporter(compilerSettings, new PrintWriter(outWriter))) {
      def lastReq: Request = prevRequestList.last
    }

  def interpret(line: String, synthetic: Boolean): InteractiveConsoleResponse = {
    clearBuffer()
    val r = interpreter.interpret(line, synthetic)
    InteractiveConsoleResponse(r, outWriter.toString)
  }

  def clearBuffer(): Unit = {
    // errorWriter.getBuffer.setLength(0)
    outWriter.getBuffer.setLength(0)
  }

  def reset(): Unit = {
    clearBuffer()
    interpreter.reset()
  }

  private def onError(str: String) = log error Message(str)
}

object InteractiveMakeSettings {
  def apply(args: List[String], onError: String => Unit): Settings = {
    val command = new GenericRunnerCommand(args, onError)
    if (command.ok) command.settings
    // TODO: Provide better exception
    else throw new Exception(command.usageMsg)
  }

  def sync(
      args: Array[String],
      bootClasspathString: String,
      classpathString: String,
      onError: String => Unit
  ): Settings = {
    val compilerSettings = sync(args.toList, onError)
    if (!bootClasspathString.isEmpty)
      compilerSettings.bootclasspath.value = bootClasspathString
    compilerSettings.classpath.value = classpathString
    compilerSettings
  }

  def sync(options: List[String], onError: String => Unit): Settings = {
    val settings = apply(options, onError)
    settings.Yreplsync.value = true
    settings
  }
}
