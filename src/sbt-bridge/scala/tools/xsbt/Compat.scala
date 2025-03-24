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

import java.io.PrintWriter
import xsbti.compile.Output
import scala.tools.nsc.Global
import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.shell.ReplReporterImpl

abstract class Compat {
  val global: Global
  import global._

  /** If given tree contains object tree attachment calls func on tree from attachment. */
  protected def processOriginalTreeAttachment(in: Tree)(func: Tree => Unit): Unit = {
    import analyzer._
    in.attachments.get[OriginalTreeAttachment].foreach { a =>
      func(a.original)
    }
  }
}
object Compat {
  // IR is renamed to Results
  val Results = scala.tools.nsc.interpreter.Results

  // IMain in 2.13 accepts ReplReporter
  def replReporter(settings: Settings, writer: PrintWriter) =
    new ReplReporterImpl(settings, writer)
}

/** Defines compatibility utils for [[ZincCompiler]]. */
trait ZincGlobalCompat {
  protected def superDropRun(): Unit = ()
}

private trait CachedCompilerCompat { self: CachedCompiler0 =>
  def newCompiler(settings: Settings, reporter: DelegatingReporter, output: Output): ZincCompiler =
    new ZincCompiler(settings, reporter, output)
}
