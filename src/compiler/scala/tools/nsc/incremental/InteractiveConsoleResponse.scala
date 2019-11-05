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

import xsbti.InteractiveConsoleResult

case class InteractiveConsoleResponse(result: InteractiveConsoleResult, output: String)
    extends xsbti.InteractiveConsoleResponse
