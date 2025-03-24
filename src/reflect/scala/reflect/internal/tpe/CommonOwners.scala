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
package reflect
package internal
package tpe

private[internal] trait CommonOwners {
  self: SymbolTable =>

  /** The most deeply nested owner that contains all the symbols
    *  of thistype or prefixless typerefs/singletype occurrences in given type.
    */
  protected[internal] def commonOwner(t: Type): Symbol = commonOwner(t :: Nil)

  /** The most deeply nested owner that contains all the symbols
    *  of thistype or prefixless typerefs/singletype occurrences in given list
    *  of types.
    */
  protected[internal] def commonOwner(tps: List[Type]): Symbol =
    if (tps.isEmpty) NoSymbol
    else {
      commonOwnerMap.clear()
      tps foreach (commonOwnerMap)
      if (commonOwnerMap.result ne null) commonOwnerMap.result else NoSymbol
    }

  protected def commonOwnerMap: CommonOwnerMap = commonOwnerMapObj

  protected class CommonOwnerMap extends TypeCollector[Symbol](null) {
    def clear(): Unit = { result = null }

    private def register(sym: Symbol): Unit = {
      // First considered type is the trivial result.
      if ((result eq null) || (sym eq NoSymbol))
        result = sym
      else
        while ((result ne NoSymbol) && (result ne sym) && !(sym isNestedIn result))
          result = result.owner
    }
    def apply(tp: Type) = tp.normalize match {
      case ThisType(sym)                => register(sym)
      case TypeRef(NoPrefix, sym, args) => register(sym.owner) ; args foreach apply
      case SingleType(NoPrefix, sym)    => register(sym.owner)
      case _                            => tp.foldOver(this)
    }
  }

  private lazy val commonOwnerMapObj = new CommonOwnerMap
}
