/* NSC -- new Scala compiler
 * Copyright 2011-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package reflect
package internal

import settings.MutableSettings

trait Required { self: SymbolTable =>
  def picklerPhase: Phase

  def erasurePhase: Phase

  def settings: MutableSettings

  @deprecated("Interactive is implemented with a custom Global; this flag is ignored", "2.11.0") def forInteractive = false
  @deprecated("Scaladoc is implemented with a custom Global; this flag is ignored", "2.11.0")    def forScaladoc = false
}
