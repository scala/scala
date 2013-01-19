package scala.reflect.macros
package compiler

import scala.reflect.macros.runtime.Context

abstract class DefaultMacroCompiler extends Resolvers
                                       with Validators
                                       with Errors {
  val c: Context
  lazy val global: c.universe.type = c.universe
  import global._

  lazy val typer = c.callsiteTyper
  private implicit val context0 = typer.context
  val context = typer.context

  val macroDdef: DefDef
  lazy val macroDef = macroDdef.symbol

  def resolveMacroImpl: Tree = {
    validateMacroImplRef()
    macroImplRef
  }
}