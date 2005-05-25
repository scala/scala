/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author
 */
// $Id$
package scala.tools.nsc.transform;

/** A sample transform.
 */
abstract class Transform {

  val global: Global;

  protected val phaseName: String;
  protected def newTransformer: global.Transformer;

  class Phase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    val global: Transform.this.global.type = Transform.this.global;
    def name: String = phaseName;
    def apply(unit: global.CompilationUnit): unit =
      unit.body = newTransformer.transform(unit.body);
  }
}

