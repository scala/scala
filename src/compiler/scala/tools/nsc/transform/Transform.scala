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

package scala.tools.nsc
package transform

/** A base class for transforms.
 *  A transform contains a compiler phase that applies a tree transformer.
 *
 *  @author Martin Odersky
 */
trait Transform extends SubComponent {

  /** The transformer factory */
  protected def newTransformer(unit: global.CompilationUnit): global.AstTransformer

  /** Create a new phase which applies transformer */
  def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new Phase(prev)

  /** The phase defined by this transform */
  class Phase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    def apply(unit: global.CompilationUnit): Unit = {
      newTransformer(unit).transformUnit(unit)
    }
  }
}
