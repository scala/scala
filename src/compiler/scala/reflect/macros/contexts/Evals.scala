package scala.reflect.macros
package contexts

import scala.reflect.runtime.{universe => ru}
import scala.tools.reflect.ToolBox

trait Evals {
  self: Context =>

  private lazy val evalMirror = ru.runtimeMirror(universe.analyzer.defaultMacroClassloader)
  private lazy val evalToolBox = evalMirror.mkToolBox()
  private lazy val evalImporter = ru.internal.createImporter(universe).asInstanceOf[ru.Importer { val from: universe.type }]

  def eval[T](expr: Expr[T]): T = {
    expr.tree match {
      case global.Literal(global.Constant(value)) =>
        value.asInstanceOf[T]
      case _ =>
        val imported = evalImporter.importTree(expr.tree)
        evalToolBox.eval(imported).asInstanceOf[T]
    }
  }
}