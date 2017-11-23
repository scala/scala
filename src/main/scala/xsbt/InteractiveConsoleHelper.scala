/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbt

import Compat._
import xsbti.InteractiveConsoleResult

object InteractiveConsoleHelper {
  implicit def toConsoleResult(ir: Results.Result): InteractiveConsoleResult =
    ir match {
      case Results.Success    => InteractiveConsoleResult.Success
      case Results.Incomplete => InteractiveConsoleResult.Incomplete
      case Results.Error      => InteractiveConsoleResult.Error
    }
}
