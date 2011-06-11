package scala.reflect
package internal

import settings.MutableSettings

trait Required { self: SymbolTable =>

  type AbstractFileType >: Null <: { def path: String }

  def picklerPhase: Phase

  val treePrinter: TreePrinter

  val gen: TreeGen { val global: Required.this.type }

  def settings: MutableSettings
}
