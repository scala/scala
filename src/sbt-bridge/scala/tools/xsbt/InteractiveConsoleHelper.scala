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

import scala.language.implicitConversions
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
