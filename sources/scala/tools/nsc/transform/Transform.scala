/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author
 */
// $Id$
package scala.tools.nsc.transform;

/** A base class for transforms.
 *  A transform contains a compiler phase which applies a tree transformer.
 */
abstract class Transform extends SubComponent {
  protected def newTransformer(unit: global.CompilationUnit): global.Transformer;
  def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new Phase(prev);
  class Phase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    def apply(unit: global.CompilationUnit): unit = {
      unit.body = newTransformer(unit).transform(unit.body);
    }
  }
}

