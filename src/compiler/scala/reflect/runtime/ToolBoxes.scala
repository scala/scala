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

    def typeCheck(tree: reflect.mirror.Tree, expectedType: reflect.mirror.Type): reflect.mirror.Tree = {
      if (compiler.settings.verbose.value) println("typing "+tree+", pt = "+expectedType)
      val run = new compiler.Run
      compiler.phase = run.refchecksPhase
      val ctree: compiler.Tree = importer.importTree(tree.asInstanceOf[Tree])
      val pt: compiler.Type = importer.importType(expectedType.asInstanceOf[Type])
      val ttree: compiler.Tree = compiler.typer.typed(ctree, compiler.analyzer.EXPRmode, pt)
      exporter.importTree(ttree).asInstanceOf[reflect.mirror.Tree]
    }

    def typeCheck(tree: reflect.mirror.Tree): reflect.mirror.Tree =
      typeCheck(tree, WildcardType.asInstanceOf[reflect.mirror.Type])

    def showAttributed(tree: reflect.mirror.Tree): String = {
      val saved = compiler.settings.printtypes.value
      try {
        compiler.settings.printtypes.value = true
        importer.importTree(tree.asInstanceOf[Tree]).toString
      } finally
        compiler.settings.printtypes.value = saved
    }
  }
}