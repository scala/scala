/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbt

import scala.tools.nsc.interpreter.IR
import xsbti.InteractiveConsoleResult

object InteractiveConsoleHelper {
  implicit def toConsoleResult(ir: IR.Result): InteractiveConsoleResult =
    ir match {
      case IR.Success    => InteractiveConsoleResult.Success
      case IR.Incomplete => InteractiveConsoleResult.Incomplete
      case IR.Error      => InteractiveConsoleResult.Error
    }
}
