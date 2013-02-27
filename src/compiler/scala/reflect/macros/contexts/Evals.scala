package scala.reflect.macros
package contexts

import scala.reflect.runtime.{universe => ru}
import scala.tools.reflect.ToolBox

trait Evals {
  self: Context =>

  private lazy val evalMirror = ru.runtimeMirror(universe.analyzer.defaultMacroClassloader)
  private lazy val evalToolBox = evalMirror.mkToolBox()
  private lazy val evalImporter = ru.mkImporter(universe).asInstanceOf[ru.Importer { val from: universe.type }]

  def eval[T](expr: Expr[T]): T = {
    val imported = evalImporter.importTree(expr.tree)
    evalToolBox.eval(imported).asInstanceOf[T]
  }
}