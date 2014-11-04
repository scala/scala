package scala.reflect.macros
package compiler

trait Resolvers {
  self: DefaultMacroCompiler =>

  import global._
  import analyzer._
  import treeInfo._

  trait Resolver {
    self: MacroImplRefCompiler =>

    val isImplBundle: Boolean
    val isImplMethod = !isImplBundle

    lazy val looksCredible: Boolean = {
      val Applied(core, _, _) = untypedMacroImplRef
      typer.silent(_.typed(markMacroImplRef(core)), reportAmbiguousErrors = false).nonEmpty
    }

    lazy val (macroImplRef, isBlackbox, macroImplOwner, macroImpl, targs) =
      typer.silent(_.typed(markMacroImplRef(untypedMacroImplRef)), reportAmbiguousErrors = false) match {
        case SilentResultValue(macroImplRef @ MacroImplReference(_, isBlackbox, owner, meth, targs)) => (macroImplRef, isBlackbox, owner, meth, targs)
        case SilentResultValue(macroImplRef) => MacroImplReferenceWrongShapeError()
        case SilentTypeError(err) => abort(err.errPos, err.errMsg)
      }
  }
}
