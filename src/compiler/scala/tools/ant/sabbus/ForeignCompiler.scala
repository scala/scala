/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.ant.sabbus

import java.io.File

import scala.tools.nsc._
import scala.tools.nsc.reporters.ConsoleReporter

class ForeignCompiler {

  private var argsBuffer: Array[String] = null
  def args: Array[String] = argsBuffer
  def args_=(a: Array[String]) {
    argsBuffer = a
    nsc
  }

  private val error: (String => Nothing) = { msg => throw new Exception(msg) }

  private def settings = new scala.tools.nsc.Settings(error)

  private lazy val reporter = new ConsoleReporter(settings)

  private lazy val nsc: Global = {
    try {
      val command = new CompilerCommand(args.toList, settings)
      new Global(command.settings, reporter)
    }
    catch {
      case ex @ FatalError(msg) =>
        throw new Exception(msg, ex)
    }
  }

  def compile(files: Array[File]): Int = {
    val command = new CompilerCommand(files.toList map (_.toString), settings)
    (new nsc.Run) compile command.files
    reporter.errorCount << 16 | reporter.warningCount
  }

}
