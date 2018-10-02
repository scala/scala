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
