/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scala.tools.util.Position;
import scalac._;
import scalac.symtab._;
import scalac.util._;
import scalac.ast._;
import java.util.HashMap;
import java.util.ArrayList;
import scala.tools.scalac.util.NewArray;
import scalac.typechecker.{AnalyzerPhase => scalac_AnalyzerPhase}
import scalac.{Global => scalac_Global}

package scala.tools.scalac.typechecker {

class AnalyzerPhase(global: scalac_Global, descriptor: PhaseDescriptor) extends scalac_AnalyzerPhase(global, descriptor) {

  var startContext = new Context(
    Tree.Empty,
    global.definitions.ROOT_CLASS,
    global.definitions.ROOT_CLASS.members(),
    Context.NONE);
  startContext.enclClass = startContext;

  if (!global.noimports) {
    startContext = addImport(startContext, global.definitions.JAVALANG);
    startContext = addImport(startContext, global.definitions.SCALA);
  }

  if (!global.noimports && !global.nopredefs) {
    startContext = addImport(startContext, global.definitions.PREDEF);
  }

  startContext = new Context(
    Tree.Empty,
    startContext.owner,
    global.definitions.ROOT_CLASS.members(),
    startContext);

  var consoleContext = new Context(
    Tree.Empty,
    global.definitions.ROOT_CLASS,
    global.definitions.ROOT_CLASS.members(),
    startContext);

  val contexts = new HashMap/*<CompilationUnit,Context>*/();
  val newSources = new ArrayList/*<CompilationUnit>*/();

  override def addConsoleImport(module: Symbol): unit =
    consoleContext = addImport(consoleContext, module);

  private def addImport(context: Context, module: Symbol): Context = {
    global.prevPhase();
    val tree = gen.mkImportAll(Position.NOPOS, module);
    global.nextPhase();
    val c = new Context(tree, context.owner, new Scope(), context);
    c.depth = context.depth;
    c
  }

  override def apply(units: Array[CompilationUnit]): unit =
    new Analyzer(global, this).apply(units);

  override def lateEnter(unit: CompilationUnit): unit = {
    new Analyzer(global, this).lateEnter(unit);
  }

}
}
