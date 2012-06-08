package scala.reflect
package internal

import settings.MutableSettings

trait Required { self: SymbolTable =>

  type AbstractFileType >: Null <: AbstractFileApi

  def picklerPhase: Phase

  def settings: MutableSettings

  def forInteractive: Boolean

  def forScaladoc: Boolean
}
