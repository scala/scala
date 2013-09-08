/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package tools
package nsc

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
}
