package scala.reflect
package internal

import settings.MutableSettings

trait Required { self: SymbolTable =>

  def picklerPhase: Phase

  def erasurePhase: Phase

  def settings: MutableSettings

  def forInteractive: Boolean

  def forScaladoc: Boolean
}
