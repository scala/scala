/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package typechecker
package splain

trait SplainDiagnostics extends splain.SplainFormatting {
  self: Analyzer =>

  import global._

  def splainFoundReqMsg(found: Type, req: Type): String = {
    if (settings.VtypeDiffs.value) ";\n" + showFormattedL(formatDiff(found, req, top = true), break = true).indent.joinLines
    else ""
  }
}
