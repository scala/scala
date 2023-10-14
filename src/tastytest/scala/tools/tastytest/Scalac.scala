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

package scala.tools.tastytest

import scala.collection.immutable.ArraySeq
import scala.util.{ Try, Success, chaining }, chaining._
import scala.tools.nsc.{Global, Settings, reporters}, reporters.ConsoleReporter
import java.io.OutputStream
import java.io.PrintWriter

object Scalac extends Script.Command {

  def scalac(out: String, additionalSettings: Seq[String], sources: String*): Try[Boolean] =
    scalac(Console.out, out, additionalSettings, sources:_*)

  def scalac(writer: OutputStream, out: String, additionalSettings: Seq[String], sources: String*) = {

    def runCompile(global: Global): Boolean = {
      global.reporter.reset()
      new global.Run() compile sources.toList
      val result = !global.reporter.hasErrors
      global.reporter.finish()
      result
    }

    def newCompiler(args: String*): Global =
      fromSettings(new Settings().tap(_.processArguments(args.toList, true)))

    def fromSettings(settings: Settings): Global = {
      val pwriter = new PrintWriter(writer, true)
      Global(settings, new ConsoleReporter(settings, Console.in, pwriter).tap(_.shortname = true))
    }

    def compile(args: String*) =
      Try(runCompile(newCompiler(args: _*)))

    if (sources.isEmpty) {
      Success(true)
    }
    else {
      val settings = Array(
        "-d", out,
        "-classpath", out,
        "-deprecation",
        "-Xfatal-warnings",
        "-usejavacp"
      ) ++ additionalSettings
      compile(ArraySeq.unsafeWrapArray(settings):_*)
    }
  }

  val commandName: String = "scalac"
  val describe: String = s"$commandName <out: Directory> <src: File> <args: String*>"

  def process(args: String*): Int = {
    if (args.length < 2) {
      println(red(s"please provide at least 2 arguments in sub-command: $describe"))
      return 1
    }
    val Seq(out, src, additionalArgs @ _*) = args: @unchecked
    val success = scalac(out, additionalArgs, src).get
    if (success) 0 else 1
  }
}
