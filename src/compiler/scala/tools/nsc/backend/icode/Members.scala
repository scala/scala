/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package tools.nsc
package backend
package icode

import scala.collection.{ mutable, immutable }
import scala.reflect.internal.Flags
import scala.reflect.internal.util.{ SourceFile, NoSourceFile }

trait Members {
  self: ICodes =>

  import global._

  /** Common interface for IClass/IField/IMethod. */
  trait IMember extends Ordered[IMember] {
    def symbol: Symbol

    def compare(other: IMember) =
      if (symbol eq other.symbol) 0
      else if (symbol isLess other.symbol) -1
      else 1

    override def equals(other: Any): Boolean =
      other match {
        case other: IMember => (this compare other) == 0
        case _ => false
      }

    override def hashCode = symbol.##
  }

  /** Represent a class in ICode */
  class IClass(val symbol: Symbol) extends IMember
}
