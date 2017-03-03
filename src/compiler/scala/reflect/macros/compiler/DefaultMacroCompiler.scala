package scala.reflect.macros
package compiler

import scala.tools.nsc.Global

abstract class DefaultMacroCompiler extends Resolvers
                                       with Validators
                                       with Errors {
  val global: Global
  import global._
  import analyzer._
  import treeInfo._
  import definitions._
  val runDefinitions = currentRun.runDefinitions

  val typer: global.analyzer.Typer
  val context = typer.context

  val macroDdef: DefDef
  lazy val macroDef = macroDdef.symbol

  case class MacroImplRefCompiler(untypedMacroImplRef: Tree, isImplBundle: Boolean) extends Resolver with Validator with Error
  private case class MacroImplResolutionException(pos: Position, msg: String) extends Exception
  def abort(pos: Position, msg: String) = throw MacroImplResolutionException(pos, msg)

  /** Resolves a macro impl reference provided in the right-hand side of the given macro definition.
   *
   *  Acceptable shapes of the right-hand side:
   *    1) [<static object>].<method name>[[<type args>]] // vanilla macro impl ref
   *    2) [<macro bundle>].<method name>[[<type args>]]  // shiny new macro bundle impl ref
   *
   *  Produces a tree, which represents a reference to a macro implementation if everything goes well,
   *  otherwise reports found errors and returns EmptyTree. The resulting tree should have the following format:
   *
   *    qualifier.method[targs]
   *
   *  Qualifier here might be omitted (local macro defs), be a static object (vanilla macro defs)
   *  or be a dummy instance of a macro bundle (e.g. new MyMacro(???).expand).
   */
  def resolveMacroImpl: Tree = {
    def tryCompile(compiler: MacroImplRefCompiler): scala.util.Try[Tree] = {
      try { compiler.validateMacroImplRef(); scala.util.Success(compiler.macroImplRef) }
      catch { case ex: MacroImplResolutionException => scala.util.Failure(ex) }
    }
    val vanillaImplRef = MacroImplRefCompiler(macroDdef.rhs.duplicate, isImplBundle = false)
    val (maybeBundleRef, methName, targs) = macroDdef.rhs.duplicate match {
      case Applied(Select(Applied(RefTree(qual, bundleName), _, Nil), methName), targs, Nil) =>
        (RefTree(qual, bundleName.toTypeName), methName, targs)
      case Applied(Ident(methName), targs, Nil) =>
        (Ident(context.owner.enclClass), methName, targs)
      case _ =>
        (EmptyTree, TermName(""), Nil)
    }
    val bundleImplRef = MacroImplRefCompiler(
      atPos(macroDdef.rhs.pos)(gen.mkTypeApply(Select(New(maybeBundleRef, List(List(Literal(Constant(null))))), methName), targs)),
      isImplBundle = true
    )
    val vanillaResult = tryCompile(vanillaImplRef)
    val bundleResult = tryCompile(bundleImplRef)

    def ensureUnambiguousSuccess() = {
      // we now face a hard choice of whether to report ambiguity:
      //   1) when there are eponymous methods in both bundle and object
      //   2) when both references to eponymous methods are resolved successfully
      // doing #1 would cause less confusion in the long run, but it would also cause more frequent source incompatibilities
      // e.g. it would fail to compile https://github.com/ReifyIt/basis
      // therefore here we go for #2
      // if (vanillaImplRef.looksCredible && bundleImplRef.looksCredible) MacroImplAmbiguousError()
      if (vanillaResult.isSuccess && bundleResult.isSuccess) MacroImplAmbiguousError()
    }

    def reportMostAppropriateFailure() = {
      typer.silent(_.typedTypeConstructor(maybeBundleRef)) match {
        case SilentResultValue(result) if looksLikeMacroBundleType(result.tpe) =>
          val bundle = result.tpe.typeSymbol
          if (!isMacroBundleType(bundle.tpe)) MacroBundleWrongShapeError()
          if (!bundle.owner.isStaticOwner) MacroBundleNonStaticError()
          bundleResult.get
        case _ =>
          vanillaResult.get
      }
    }

    try {
      if (vanillaResult.isSuccess || bundleResult.isSuccess) ensureUnambiguousSuccess()
      if (vanillaResult.isFailure && bundleResult.isFailure) reportMostAppropriateFailure()
      vanillaResult.orElse(bundleResult).get
    } catch {
      case MacroImplResolutionException(pos, msg) =>
        context.error(pos, msg)
        EmptyTree
    }
  }
}
