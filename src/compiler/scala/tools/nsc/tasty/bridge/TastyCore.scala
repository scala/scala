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

package scala.tools.nsc.tasty.bridge

import scala.tools.nsc
import nsc.symtab, nsc.tasty.TastyUniverse

/**The base of the `TastyUniverse` cake, providing aliases to types from `scala.reflect` at the same import level
 * as new TASTy specific types.
 */
abstract class TastyCore { self: TastyUniverse =>
  import self.{symbolTable => u}

  // Compiler Entry Point
  type SymbolTable <: symtab.SymbolTable { def settings: nsc.Settings }
  val symbolTable: SymbolTable

  // Misc
  type Symbol     = u.Symbol
  type Type       = u.Type
  type Tree       = u.Tree
  type Constant   = u.Constant

  private val Identity = (x: Any) => x

  def id[T]: T => T = Identity.asInstanceOf[T => T]

}
