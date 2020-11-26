package scala.tools.tastytest

import scala.collection.immutable.ArraySeq
import scala.util.{ Try, Success, chaining }, chaining._
import scala.tools.nsc.{Global, Settings, reporters}, reporters.ConsoleReporter

object Scalac extends Script.Command {

  def scalac(out: String, additionalSettings: Seq[String], sources: String*): Try[Boolean] = {

    def runCompile(global: Global): Boolean = {
      global.reporter.reset()
      new global.Run() compile sources.toList
      val result = !global.reporter.hasErrors
      global.reporter.finish()
      result
    }

    def newCompiler(args: String*): Global =
      fromSettings(new Settings().tap(_.processArguments(args.toList, true)))

    def fromSettings(settings: Settings): Global =
      Global(settings, new ConsoleReporter(settings).tap(_.shortname = true))

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
