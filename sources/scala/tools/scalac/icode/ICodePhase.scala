/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scalac.transformer.{ICodePhase => scalac_ICodePhase}
import scalac.{Global => scalac_Global}
import scalac.{Unit => scalac_Unit}
import scalac.PhaseDescriptor;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.Phase;
import scalac.atree._;

import ch.epfl.lamp.util.CodePrinter;

package scala.tools.scalac.icode {

/** This class represents the ICode phase. This phase use the ATrees
  * that represents the code with some primitives backends-like
  * and convert them in a inline form: The ICode (for intermediate
  * code). The ICode will be produced in the icode variable
  * of all AMethod objects. */
class ICodePhase(global: scalac_Global, descriptor: PhaseDescriptor) extends scalac_ICodePhase (global, descriptor) {

  // ##################################################
  // Public methods

  /* Apply the icode phase to the given units */
  override def apply(units: Array[scalac_Unit]) = {
    val units_it = new IterableArray(units).elements;

    units_it.foreach(translate);
  }

  /* Return a CodePrinter for the ICode */
  override def getPrinter(cp: CodePrinter) = new ICodePrinter(cp);

  // ##################################################
  // Private methods

  /** This method translate a single unit, it traverses all methods and
    * generate ICode for each of them */
  private def translate(u: scalac_Unit) : unit = {
    val classes_it = new IterableArray(u.repository.classes()).elements;
    classes_it.foreach((c: AClass) => {
      val methods_it = new IterableArray(c.methods()).elements;
      methods_it.foreach((m: AMethod) => {
	val label : String = m.symbol().name.toString();

	val ic : ICode = new ICode(label, global);
	ic.generate(m.code());

	// save the produced ICode
	m.icode = ic;

      });
    });
  }

}
}
