/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$
package scala.tools.nsc;

/** An nsc sub-component.
 */
abstract class SubComponent {

  val global: Global;

  abstract class StdPhase(prev: Phase) extends Phase(prev) {
    def run: unit =
      for (val unit <- global.units) {
        if (global.settings.debug.value) System.out.println("[running phase " + name + " on " + unit + "]");//debug
        apply(unit);
      }
    def apply(unit: global.CompilationUnit): unit;
  }
}

