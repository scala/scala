package scala.reflect.macros
package compiler

import scala.reflect.internal.Flags._
import scala.reflect.macros.TypecheckException

trait Resolvers {
  self: DefaultMacroCompiler =>

  import global._
  import analyzer._
  import definitions._
  import treeInfo._
  import gen._
  import runDefinitions._

  trait Resolver {
    self: MacroImplRefCompiler =>

    val isImplBundle: Boolean
    val isImplMethod = !isImplBundle

    lazy val looksCredible: Boolean = {
      val Applied(core, _, _) = untypedMacroImplRef
      typer.silent(_.typed(markMacroImplRef(core)), reportAmbiguousErrors = false).nonEmpty
    }

    lazy val macroImplRef: Tree =
      typer.silent(_.typed(markMacroImplRef(untypedMacroImplRef)), reportAmbiguousErrors = false) match {
        case SilentResultValue(success) => success
        case SilentTypeError(err) => abort(err.errPos, err.errMsg)
      }

    // FIXME: cannot write this concisely because of SI-7507
    // lazy val (_, macroImplOwner, macroImpl, macroImplTargs) =
    private lazy val dissectedMacroImplRef =
      macroImplRef match {
        case MacroImplReference(isBundle, isBlackbox, owner, meth, targs) => (isBlackbox, owner, meth, targs)
        case _ => MacroImplReferenceWrongShapeError()
      }
    lazy val isImplBlackbox = dissectedMacroImplRef._1
    lazy val macroImplOwner = dissectedMacroImplRef._2
    lazy val macroImpl = dissectedMacroImplRef._3
    lazy val targs = dissectedMacroImplRef._4
  }
}
