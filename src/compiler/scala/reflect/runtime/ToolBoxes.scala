package scala.reflect
package runtime

import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.ReflectGlobal
import scala.tools.nsc.CompilerCommand
import scala.tools.nsc.Global
import scala.tools.nsc.typechecker.Modes

trait ToolBoxes extends { self: Universe =>

  class ToolBox(val reporter: Reporter = new StoreReporter, val options: String = "") {

    lazy val compiler: Global = {
      val command = new CompilerCommand(options.split(" ").toList, reporter.error(scala.tools.nsc.util.NoPosition, _))
      new ReflectGlobal(command.settings, reporter)
    }

    lazy val importer = new compiler.Importer {
      val from: self.type = self
    }

    lazy val exporter = importer.reverse

    def typeCheck(tree: Tree, expectedType: Type = WildcardType): Tree = {
      new compiler.Run
      val ctree: compiler.Tree = importer.importTree(tree)
      val pt: compiler.Type = importer.importType(expectedType)
      val ttree: compiler.Tree = compiler.typer.typed(ctree, compiler.analyzer.EXPRmode, pt)
      exporter.importTree(ttree)
    }
  }

}