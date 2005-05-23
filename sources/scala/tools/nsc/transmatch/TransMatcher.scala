/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author buraq
 */
// $Id$
package scala.tools.nsc.transmatch;

/** Translation of pattern matching
 */
abstract class TransMatcher {

  val global: Global;

  import global._;
  import definitions._;
  import posAssigner.atPos;

  class TransMatchPhase(prev: Phase) extends StdPhase(prev) {
    def name = "transmatcher";
    val global: TransMatcher.this.global.type = TransMatcher.this.global;
    def apply(unit: CompilationUnit): unit =
      unit.body = newTransMatcher.transform(unit.body);
  }

  def newTransMatcher = new TransMatch();

  class TransMatch extends Transformer(copy) {
    override def transform(tree: Tree) = tree match {
      case _ => super.transform(tree);
    }
  }

}
