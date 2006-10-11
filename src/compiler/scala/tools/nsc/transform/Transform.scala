/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc.transform

/** <p>
 *    A base class for transforms.
 *  </p>
 *  <p>
 *    A transform contains a compiler phase which applies a tree transformer.
 *  </p>
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class Transform extends SubComponent {

  /** The transformer factory */
  protected def newTransformer(unit: global.CompilationUnit): global.Transformer

  /** Create a new phase which applies transformer */
  def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new Phase(prev)

  /** The phase defined by this transform */
  class Phase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    def apply(unit: global.CompilationUnit): unit = {
      newTransformer(unit).transformUnit(unit)
    }
  }
}

