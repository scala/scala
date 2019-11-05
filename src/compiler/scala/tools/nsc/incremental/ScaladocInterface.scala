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
import Log.debug

class ScaladocInterface {
  def run(args: Array[String], log: Logger, delegate: xsbti.Reporter) =
    (new Runner(args, log, delegate)).run
}
private class Runner(args: Array[String], log: Logger, delegate: xsbti.Reporter) {
  import scala.tools.nsc.{ doc, Global, reporters }
  import reporters.Reporter
  val docSettings: doc.Settings = new doc.Settings(Log.settingsError(log))
  val command = Command(args.toList, docSettings)
  val reporter = DelegatingReporter(docSettings, delegate)
  def noErrors = !reporter.hasErrors && command.ok

  def run(): Unit = {
    debug(log, "Calling Scaladoc with arguments:\n\t" + args.mkString("\n\t"))
    if (noErrors) {
      import doc._ // 2.8 trunk and Beta1-RC4 have doc.DocFactory.  For other Scala versions, the next line creates forScope.DocFactory
      val processor = new DocFactory(reporter, docSettings)
      processor.document(command.files)
    }
    reporter.printSummary()
    if (!noErrors)
      throw new InterfaceCompileFailed(args, reporter.problems, "Scaladoc generation failed")
  }

  object forScope {
    class DocFactory(reporter: Reporter, docSettings: doc.Settings) // 2.7 compatibility
    {
      // see https://github.com/paulp/scala-full/commit/649823703a574641407d75d5c073be325ea31307
      trait GlobalCompat {
        def onlyPresentation = false

        def forScaladoc = false
      }

      object compiler extends Global(command.settings, reporter) with GlobalCompat {
        override def onlyPresentation = true
        override def forScaladoc = true
        class DefaultDocDriver // 2.8 source compatibility
        {
          assert(false)
          def process(units: Iterator[CompilationUnit]) = error("for 2.8 compatibility only")
        }
      }
      def document(ignore: Seq[String]): Unit = {
        import compiler._
        val run = new Run
        run compile command.files

        val generator = {
          new DefaultDocDriver {
            lazy val global: compiler.type = compiler
            lazy val settings = docSettings
          }
        }
        generator.process(run.units)
      }
    }
  }
}
