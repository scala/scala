/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import ch.epfl.lamp.util.CodePrinter;

import scalac.CompilationUnit;
import scalac.{Global => scalac_Global}

package scala.tools.scalac.icode {

/** This class implements a Printer for the ICode. */
class ICodePrinter(global0: scalac_Global, printer0: CodePrinter) {

  //##########################################################################
  // Public Fields

  val global: scalac_Global = global0;
  val printer: CodePrinter = printer0;

  //##########################################################################
  // Public Methods

  /** Prints the ICode of the given units. */
  def printUnits(units: Array[CompilationUnit]): Unit = {
    val phase = global.currentPhase;
    printer.println("[[ICode at "+phase+" (after "+phase.prev+")]]");
    Iterator.fromArray(units).foreach(printUnit);
  }

  /** Prints the ICode of the given unit. */
  def printUnit(unit: CompilationUnit): Unit = {
    printer.println ("// Scala source: "+unit.source);
    val classes_it = Iterator.fromArray(unit.repository.classes());
    classes_it.foreach(c => {
      printer.println ("");
      printer.println ("// ICode for class <"+c.symbol().name+">");
      val methods_it = Iterator.fromArray(c.methods());
      methods_it.foreach(m => {
	val icode : ICode = m.icode.asInstanceOf[ICode];
	printer.println ("// ::"+m.symbol().name);
	icode.icTraverse((bb: IBasicBlock) => {
	  printer.println("Block #"+bb.label);
	  printer.println("Initial stack -> "+bb.initialStack);
	  printer.println("Substituable variables : ");
	  if (bb.substituteVars != null)
	    bb.substituteVars.foreach(s => printer.print(s.name.toString()));
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

  //##########################################################################
}
}
