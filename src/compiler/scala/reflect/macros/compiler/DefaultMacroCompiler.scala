package scala.reflect.macros
package compiler

import scala.tools.nsc.Global
import scala.reflect.macros.contexts.Context

abstract class DefaultMacroCompiler extends Resolvers
                                       with Validators
                                       with Errors {
  val global: Global
  import global._

  val typer: global.analyzer.Typer
  private implicit val context0 = typer.context
  val context = typer.context

  val macroDdef: DefDef
  lazy val macroDef = macroDdef.symbol

  private case class MacroImplResolutionException(pos: Position, msg: String) extends Exception
  def abort(pos: Position, msg: String) = throw MacroImplResolutionException(pos, msg)

  def resolveMacroImpl: Tree = {
    try {
      validateMacroImplRef()
      macroImplRef
    } catch {
      case MacroImplResolutionException(pos, msg) =>
        context.error(pos, msg)
        EmptyTree
    }
  }
}