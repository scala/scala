package scala.reflect
package internal

import settings.MutableSettings

trait Required { self: SymbolTable =>

  type AbstractFileType >: Null <: {
    def path: String
    def canonicalPath: String
  }

  def picklerPhase: Phase

  def settings: MutableSettings

  def forInteractive: Boolean

  def forScaladoc: Boolean
}
