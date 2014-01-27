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
  private val runDefinitions = currentRun.runDefinitions
  import runDefinitions.{Predef_???, _}

  /** Resolves a macro impl reference provided in the right-hand side of the given macro definition.
   *
   *  Acceptable shapes of the right-hand side:
   *    1) [<static object>].<method name>[[<type args>]] // vanilla macro def
   *    2) [<macro bundle>].<method name>[[<type args>]]  // shiny new macro bundle
   *
   *  Produces a tree, which represents a reference to a macro implementation if everything goes well,
   *  otherwise reports found errors and returns EmptyTree. The resulting tree should have the following format:
   *
   *    qualifier.method[targs]
   *
   *  Qualifier here might be omitted (macro defs local to blocks), be a static object (vanilla macro defs)
   *  or be a dummy instance of a macro bundle (e.g. new MyMacro(???).expand).
   */
  lazy val macroImplRef: Tree = {
    val (maybeBundleRef, methName, targs) = macroDdef.rhs match {
      case Applied(Select(Applied(RefTree(qual, bundleName), _, Nil), methName), targs, Nil) =>
        (RefTree(qual, bundleName.toTypeName), methName, targs)
      case Applied(Ident(methName), targs, Nil) =>
        (Ident(context.owner.enclClass), methName, targs)
      case _ =>
        (EmptyTree, TermName(""), Nil)
    }

    val untypedImplRef = typer.silent(_.typedTypeConstructor(maybeBundleRef)) match {
      case SilentResultValue(result) if looksLikeMacroBundleType(result.tpe) =>
        val bundle = result.tpe.typeSymbol
        if (!isMacroBundleType(bundle.tpe)) MacroBundleWrongShapeError()
        if (!bundle.owner.isStaticOwner) MacroBundleNonStaticError()
        atPos(macroDdef.rhs.pos)(gen.mkTypeApply(Select(New(bundle, Ident(Predef_???)), methName), targs))
      case _ =>
        macroDdef.rhs
    }

    val typedImplRef = typer.silent(_.typed(markMacroImplRef(untypedImplRef)), reportAmbiguousErrors = false)
    typedImplRef match {
      case SilentResultValue(success) => success
      case SilentTypeError(err) => abort(err.errPos, err.errMsg)
    }
  }

  // FIXME: cannot write this concisely because of SI-7507
  // lazy val (isImplBundle, macroImplOwner, macroImpl, macroImplTargs) =
  private lazy val dissectedMacroImplRef =
    macroImplRef match {
      case MacroImplReference(isBundle, isBlackbox, owner, meth, targs) => (isBundle, isBlackbox, owner, meth, targs)
      case _ => MacroImplReferenceWrongShapeError()
    }
  lazy val isImplBundle = dissectedMacroImplRef._1
  lazy val isImplMethod = !isImplBundle
  lazy val isImplBlackbox = dissectedMacroImplRef._2
  lazy val macroImplOwner = dissectedMacroImplRef._3
  lazy val macroImpl = dissectedMacroImplRef._4
  lazy val targs = dissectedMacroImplRef._5
}
