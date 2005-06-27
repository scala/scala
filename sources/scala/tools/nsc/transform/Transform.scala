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

  protected val phaseName: String;
  protected def newTransformer(unit: global.CompilationUnit): global.Transformer;

  class Phase(prev: scala.tools.nsc.Phase) extends global.StdPhase(prev) {
    def name: String = phaseName;
    def apply(unit: global.CompilationUnit): unit = {
      unit.body = newTransformer(unit).transform(unit.body);
    }
  }
}

