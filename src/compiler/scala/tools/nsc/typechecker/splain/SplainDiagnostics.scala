/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
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

trait SplainDiagnostics
extends SplainFormatting
{ self: Analyzer with SplainData =>
  import global._

  def splainFoundReqMsg(found: Type, req: Type): Option[String] =
    if (settings.typeDiffsSettingEnable)
      Some(";\n" + showFormattedL(formatDiff(found, req, true), true).indent.joinLines)
    else
      None
}
