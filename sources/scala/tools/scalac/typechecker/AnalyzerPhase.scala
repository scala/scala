/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import ch.epfl.lamp.util.Position;
import scalac._;
import scalac.util._;
import scalac.ast._;
import scalac.symtab._;
import scalac.checkers._;
import java.util.HashMap;
import java.util.ArrayList;
import scala.tools.scalac.util.NewArray;
import scalac.typechecker.{AnalyzerPhase => scalac_AnalyzerPhase}
import scalac.{Global => scalac_Global}

package scala.tools.scalac.typechecker {

class AnalyzerPhase(global: scalac_Global, descriptor: PhaseDescriptor) extends scalac_AnalyzerPhase(global, descriptor) {

  val startContext = new Context(
    Tree.Empty,
    global.definitions.ROOT_CLASS,
    global.definitions.ROOT_CLASS.members(),
    Context.NONE);
  startContext.enclClass = startContext;

  if (!global.noimports) {
    addImport(startContext, global.definitions.JAVALANG);
    addImport(startContext, global.definitions.SCALA);
  }

  if (!global.noimports && !global.nopredefs) {
    addImport(startContext, global.definitions.PREDEF);
  }

  val consoleContext = new Context(
    Tree.Empty,
    global.definitions.ROOT_CLASS,
    global.definitions.ROOT_CLASS.members(),
    startContext);

  val contexts = new HashMap/*<Unit,Context>*/();
  val newSources = new ArrayList/*<Unit>*/();

  override def addConsoleImport(module: Symbol): unit =
    addImport(consoleContext, module);

  private def addImport(context: Context, module: Symbol): unit = {
    global.prevPhase();
    val tree = gen.mkImportAll(Position.NOPOS, module);
    global.nextPhase();
    context.imports = new ImportList(tree, new Scope(), context.imports);
  }

  override def apply(units: Array[Unit]): unit =
    new Analyzer(global, this).apply(units);

  override def lateEnter(global: scalac_Global, unit: Unit, symbol: Symbol): unit = {
    new Analyzer(global, this).lateEnter(unit, symbol);
  }

  override def postCheckers(global: scalac_Global): Array[Checker] =
    NewArray.Checker(
      new CheckSymbols(global),
      new CheckTypes(global)
    );
}
}
