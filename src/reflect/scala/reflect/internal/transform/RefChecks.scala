/* NSC -- new Scala compiler
 * Copyright 2011-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package reflect
package internal
package transform

trait RefChecks {

  val global: SymbolTable
  import global._

  def transformInfo(sym: Symbol, tp: Type): Type =
    if (sym.isModule && !sym.isStatic) NullaryMethodType(tp)
    else tp
}
