/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbt

import scala.tools.nsc.interpreter.IR
import xsbti.ConsoleResult

object ConsoleHelper {
  implicit def toConsoleResult(ir: IR.Result): ConsoleResult =
    ir match {
      case IR.Success    => ConsoleResult.Success
      case IR.Incomplete => ConsoleResult.Incomplete
      case IR.Error      => ConsoleResult.Error
    }
}
