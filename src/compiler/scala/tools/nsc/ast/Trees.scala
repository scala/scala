/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

trait Trees extends reflect.internal.Trees { self: Global =>

  lazy val treePrinter = newTreePrinter()

  class Transformer extends super.Transformer {
    def transformUnit(unit: CompilationUnit) { unit.body = transform(unit.body) }
  }
}