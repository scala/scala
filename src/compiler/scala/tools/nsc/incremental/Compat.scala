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

import java.io.PrintWriter
import xsbti.compile.Output
import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.shell.ReplReporterImpl

abstract class Compat
object Compat {
  // IR is renanmed to Results
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
