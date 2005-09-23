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

  /** The transformer factory */
  protected def newTransformer(unit: global.CompilationUnit): global.Transformer;

  def resetTransform: unit = {}

  /** Create a new phase which applies transformer */
  def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new Phase(prev);

  /** The phase defined by this transform */
  class Phase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    override def resetPhase: unit = resetTransform;
    def apply(unit: global.CompilationUnit): unit = {
      newTransformer(unit).transformUnit(unit);
    }
  }
}

