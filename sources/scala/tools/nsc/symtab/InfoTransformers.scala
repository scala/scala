/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.symtab;

abstract class InfoTransformers: SymbolTable {

  abstract class InfoTransformer {
    private var prev: InfoTransformer = this;
    private var next: InfoTransformer = this;

    val phase: Phase;
    def transform(sym: Symbol, tpe: Type): Type;

    def insert(that: InfoTransformer): unit = {
      assert(this.phase.id != that.phase.id);
      if (that.phase.id < this.phase.id) {
	prev insert that
      } else if (next.phase.id <= that.phase.id && next.phase != NoPhase) {
	next insert that
      } else {
	that.next = next;
	that.prev = this;
	next.prev = that;
	this.next = that
      }
    }

    def nextFrom(from: Phase): InfoTransformer =
      if (from.id == this.phase.id) this
      else if (from.id < this.phase.id)
	if (prev.phase.id < from.id) this
	else prev.nextFrom(from);
      else if (next.phase == NoPhase) next
      else next.nextFrom(from);
  }
}





