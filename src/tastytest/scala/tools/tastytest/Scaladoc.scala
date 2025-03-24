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

package scala.tools.tastytest

import scala.collection.immutable.ArraySeq
import scala.util.{ Try, Success, chaining }, chaining._
import scala.tools.nsc.{ reporters}, reporters.{Reporter, ConsoleReporter}
import java.io.OutputStream
import java.io.PrintWriter

import scala.tools.nsc.{ScalaDoc => RealScaladoc, doc}
import scala.reflect.internal.util.NoPosition

object Scaladoc extends Script.Command {

  def scaladoc(out: String, additionalSettings: Seq[String], sources: String*): Try[Boolean] =
    scaladoc(Console.out, out, additionalSettings, sources:_*)

  def scaladoc(writer: OutputStream, out: String, additionalSettings: Seq[String], sources: String*) = {

    def setup(args: Seq[String]): (Reporter, doc.Settings, RealScaladoc.Command) = {
      lazy val (reporter: Reporter, docSettings) = {
        val docSettings = new doc.Settings(msg => reporter.error(NoPosition, msg), msg => reporter.echo(msg))
        val pwriter = new PrintWriter(writer, true)
        (new ConsoleReporter(docSettings, Console.in, pwriter).tap(_.shortname = true), docSettings)
      }
      (reporter, docSettings, new RealScaladoc.Command(args.toList, docSettings))
    }

    def compile(args: String*): Try[Boolean] = {
      val (reporter, docSettings, command) = setup(args)
      Try {
        assert(command.files.nonEmpty, "no files to compile")
        try { new doc.DocFactory(reporter, docSettings).document(command.files) }
        finally reporter.finish()
      }.map(_ => !reporter.hasErrors)
    }

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
      ) ++ additionalSettings ++ sources
      compile(ArraySeq.unsafeWrapArray(settings):_*)
    }
  }

  val commandName: String = "scaladoc"
  val describe: String = s"$commandName <out: Directory> <src: File> <args: String*>"

  def process(args: String*): Int = {
    if (args.length < 2) {
      println(red(s"please provide at least 2 arguments in sub-command: $describe"))
      return 1
    }
    val Seq(out, src, additionalArgs @ _*) = args: @unchecked
    val success = scaladoc(out, additionalArgs, src).get
    if (success) 0 else 1
  }
}
