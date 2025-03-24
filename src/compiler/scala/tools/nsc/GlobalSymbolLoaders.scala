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

package scala
package tools
package nsc

import scala.tools.nsc.Reporting.WarningCategory

/**
 * Symbol loaders implementation that wires dependencies using Global.
 */
abstract class GlobalSymbolLoaders extends symtab.SymbolLoaders {
  val global: Global
  val symbolTable: global.type = global
  val platform: symbolTable.platform.type
  import global._
  def lookupMemberAtTyperPhaseIfPossible(sym: Symbol, name: Name): Symbol = {
    def lookup = sym.info.member(name)
    // if loading during initialization of `definitions` typerPhase is not yet set.
    // in that case we simply load the member at the current phase
    if (currentRun.typerPhase eq null)
      lookup
    else
      enteringTyper { lookup }
  }

  protected def compileLate(srcfile: io.AbstractFile): Unit =
    currentRun.compileLate(srcfile)

  def warning(pos: Position, msg: String, category: WarningCategory, site: String): Unit =
    runReporting.warning(pos, msg, category, site)
}
