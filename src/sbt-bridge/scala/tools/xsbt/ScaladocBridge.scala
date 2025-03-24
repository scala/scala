/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Scala Center, Lightbend dba Akka, and Mark Harrah
 *
 * Scala (https://www.scala-lang.org)
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
package xsbt

import xsbti.{ Logger, VirtualFile }
import scala.reflect.io.AbstractFile
import Log.debug
import scala.tools.nsc.CompilerCommand

class ScaladocBridge extends xsbti.compile.ScaladocInterface2 {
  def run(sources: Array[VirtualFile], args: Array[String], log: Logger, delegate: xsbti.Reporter) =
    (new Runner(sources, args, log, delegate)).run()
}

private class Runner(
    sources: Array[VirtualFile],
    args: Array[String],
    log: Logger,
    delegate: xsbti.Reporter
) {
  import scala.tools.nsc.{ doc, Global, reporters }
  import reporters.Reporter
  val docSettings: doc.Settings = new doc.Settings(Log.settingsError(log))
  val fullArgs = args.toList ++ sources.map(_.toString)
  val command = new CompilerCommand(fullArgs, docSettings)
  val reporter = DelegatingReporter(docSettings, delegate)
  def noErrors = !reporter.hasErrors && command.ok

  def run(): Unit = {
    debug(log, "Calling Scaladoc with arguments:\n\t" + fullArgs.mkString("\n\t"))
    if (noErrors) {
      import doc._ // 2.8 trunk and Beta1-RC4 have doc.DocFactory.  For other Scala versions, the next line creates forScope.DocFactory
      val processor = new DocFactory(reporter, docSettings)
      processor.document(command.files)
    }
    reporter.printSummary()
    if (!noErrors)
      throw new InterfaceCompileFailed(
        args ++ sources.map(_.toString),
        reporter.problems,
        "Scaladoc generation failed"
      )
  }

  object forScope {
    class DocFactory(reporter: Reporter, docSettings: doc.Settings) {
      object compiler extends Global(command.settings, reporter) {
        // override def onlyPresentation = true
        // override def forScaladoc = true

        // 2.8 source compatibility
        class DefaultDocDriver {
          assert(false, "2.8 compatibility")
          def process(units: Iterator[CompilationUnit]) = globalError("for 2.8 compatibility only")
        }
      }
      def document(ignore: Seq[String]): Unit = {
        import compiler._
        val run = new Run
        val wrappedFiles = sources.toList.map(AbstractZincFile(_))
        val sortedSourceFiles: List[AbstractFile] =
          wrappedFiles.sortWith(_.underlying.id < _.underlying.id)
        run.compileFiles(sortedSourceFiles)
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
