/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id: Global.scala
\*                                                                      */

import scalac.{CompilationUnit, CompilerCommand, Global => scalac_Global};
import scalac.ast.printer.TreePrinter;
import scalac.backend.jvm.GenJVM;
import scalac.backend.msil.GenMSIL;
import scalac.symtab.Symbol;
import scalac.util.Debug;
import scala.tools.scalac.backend.GenJVMFromICode;
import scala.tools.util.Timer;

package scala.tools.scalac {

import ast.printer._;
import java.io.PrintWriter;
import typechecker.Infer;

/** The global environment of a compiler run
 *
 */
class Global(args: CompilerCommand, timer: Timer, interpret: Boolean) extends scalac_Global(args, timer, interpret) {

  def this(args: CompilerCommand, interpret: Boolean) =
    this(args, scalac_Global.getTimer(args.reporter()), interpret);
  def this(args: CompilerCommand) = this(args, false);

  override def newInfer(): Infer =
    new Infer(this, treeGen, make);
  override def newTextTreePrinter(writer: PrintWriter): TreePrinter =
    new TextTreePrinter(this, writer);
  override def newHTMLTreePrinter(writer: PrintWriter): TreePrinter =
    new HTMLTreePrinter(this, writer);
  override def newSwingTreePrinter(writer: PrintWriter): TreePrinter =
    new SwingTreePrinter(this);

  override def dump(units: Array[CompilationUnit]): Unit = {
    if (target == scalac_Global.TARGET_JVM) {
      GenJVM.translate(this, units);
    } else if (target == scalac_Global.TARGET_MSIL) {
      GenMSIL.translate(this, units);
    } else if (target == scalac_Global.TARGET_JVMFROMICODE) {
      GenJVMFromICode.translate(this, units);
    }
    symdata.clear();
  }


  protected override def loadFunctions(): Unit = {
    val mixinOnly = target != scalac_Global.TARGET_INT;
    List.range(0, definitions.FUNCTION_COUNT).foreach(
      i => loadCode(definitions.FUNCTION_CLASS(i), mixinOnly));
  }

  private def loadCode(clasz: Symbol, mixinOnly: boolean): unit = {
    assert(clasz.isClass() && !clasz.isModuleClass(), Debug.show(clasz));
    if (clasz.isExternal()) {
      try {
        compileLate(getSourceFile(clasz), mixinOnly);
      } catch {
        case exception: java.io.IOException =>
          if (debug) exception.printStackTrace();
          error(exception.getMessage() + "; source file for "
                + clasz + " is needed");
      }
    }
  }

}
}
