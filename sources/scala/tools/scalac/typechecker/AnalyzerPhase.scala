/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scalac.typechecker;

import ch.epfl.lamp.util.Position;
import scalac._;
import scalac.util._;
import scalac.ast._;
import scalac.symtab._;
import scalac.checkers._;
import java.util.HashMap;
import java.util.ArrayList;
import scala.tools.scalac.util.NewArray;

class AnalyzerPhase(global: Global, descriptor: PhaseDescriptor) extends scalac.typechecker.AnalyzerPhase(global, descriptor) {

  val startContext = new Context(
    Tree.Empty,
    global.definitions.ROOT_CLASS,
    global.definitions.ROOT_CLASS.members(),
    Context.NONE);
  startContext.enclClass = startContext;

  if (!global.noimports) {
    addImport(startContext, global.definitions.getModule(Names.java_lang));
    addImport(startContext, global.definitions.getModule(Names.scala));
  }

  if (!global.noimports && !global.nopredefs) {
    addImport(startContext, global.definitions.getModule(Names.scala_Predef));
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
    val tree = global.treeGen.mkImportAll(Position.NOPOS, module);
    global.nextPhase();
    context.imports = new ImportList(tree, new Scope(), context.imports);
  }

  override def apply(units: Array[Unit]): unit =
    new Analyzer(global, this).apply(units);

  override def lateEnter(global: Global, unit: Unit, symbol: Symbol): unit = {
    new Analyzer(global, this).lateEnter(unit, symbol);
  }

  override def postCheckers(global: Global): Array[Checker] =
    NewArray.Checker(
      new CheckSymbols(global),
      new CheckTypes(global),
      new CheckOwners(global),
      new CheckNames(global)
    );
}

