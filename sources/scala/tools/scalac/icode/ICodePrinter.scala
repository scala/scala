/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import ch.epfl.lamp.util.CodePrinter;

import scalac.Unit;
import scalac.{Global => scalac_Global}
import scalac.atree._;

import scalac.symtab.{Symbol => scalac_Symbol}

package scala.tools.scalac.icode {

/* This class implements a Printer for the ICode */
class ICodePrinter (printer: CodePrinter) extends ATreePrinter (printer: CodePrinter) {

  // ##################################################
  // Public methods

  /* This method print all ICode produced after the ICode phase */
  override def printGlobal(global: scalac_Global) : ATreePrinter = { // !!! Extends ATree !!!
    val phase = global.currentPhase;

    printer.println("[[ICode at "+phase+" (after "+phase.prev+")]]");
    val units_it = new IterableArray(global.units).elements;
    units_it.foreach(printAnUnit);

    this;
  }

  /* This method print a single unit. */
  def printAnUnit(unit: Unit) = { // ??? Private
    printer.println ("// Scala source: "+unit.source);
    val classes_it = new IterableArray(unit.repository.classes()).elements;
    classes_it.foreach((c: AClass) => {
      printer.println ("");
      printer.println ("// ICode for class <"+c.symbol().name+">");
      val methods_it = new IterableArray(c.methods()).elements;
      methods_it.foreach((m: AMethod) => {
	val icode : ICode = m.icode.asInstanceOf[ICode];
	printer.println ("// ::"+m.symbol().name);
	icode.icTraverse((bb: IBasicBlock) => {
	  printer.println("Block #"+bb.label);
	  printer.println("Initial stack -> "+bb.initialStack);
	  printer.println("Substituable variables : ");
	  if (bb.substituteVars != null)
	    bb.substituteVars.foreach((s: scalac_Symbol) => printer.print(s.name.toString()));
	  else
	    printer.println(" {Empty} ");
	  printer.println("Instructions:");
	  printer.indent();
	  bb.bbTraverse((ici: ICInstruction) =>
	    printer.println(ici.toString()));
	  printer.undent();
	  printer.println("End stack -> "+bb.endStack);
	  printer.print  ("Successors: ");
	  bb.successors.foreach((bb: IBasicBlock) => printer.print(bb.label+", "));
	  printer.println (""); // ?? Del
	  printer.println ();
	});
      });
    });


  }
}
}
