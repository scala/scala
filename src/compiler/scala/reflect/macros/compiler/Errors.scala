/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.reflect.macros
package compiler

import scala.annotation.tailrec
import scala.reflect.macros.util.Traces

trait Errors extends Traces {
  self: DefaultMacroCompiler =>

  import global._
  import analyzer._
  import definitions._
  import treeInfo._
  import typer.infer.InferErrorGen._
  import runDefinitions._
  def globalSettings = global.settings

  private def implRefError(message: String) = {
    val Applied(culprit, _, _) = macroDdef.rhs
    abort(culprit.pos, message)
  }

  private def bundleRefError(message: String) = {
    val Applied(core, _, _) = macroDdef.rhs
    val culprit = core match {
      case Select(Applied(core, _, _), _) => core
      case _ => core
    }
    abort(culprit.pos, message)
  }

  def MacroImplAmbiguousError() = implRefError(
    "macro implementation reference is ambiguous: makes sense both as\n"+
    "a macro bundle method reference and a vanilla object method reference")

  private def replClassBasedMacroAddendum(isReplClassBased: Boolean): String =
    if (isReplClassBased)
      "\nnote: macro definition is not supported in the REPL when using -Yrepl-classbased, run :replay -Yrepl-class-based:false."
    else ""


  def MacroBundleNonStaticError(isReplClassBased: Boolean) = {
    bundleRefError("macro bundles must be static" + replClassBasedMacroAddendum(isReplClassBased))
  }

  def MacroBundleWrongShapeError() = bundleRefError("macro bundles must be concrete monomorphic classes having a single constructor with a `val c: Context` parameter")

  trait Error {
    self: MacroImplRefCompiler =>

    // check errors

    def MacroImplReferenceWrongShapeError(isReplClassBased: Boolean = false) = implRefError(
      "macro implementation reference has wrong shape. required:\n"+
      "macro [<static object>].<method name>[[<type args>]] or\n" +
      "macro [<macro bundle>].<method name>[[<type args>]]" + replClassBasedMacroAddendum(isReplClassBased))

    def MacroImplWrongNumberOfTypeArgumentsError() = {
      val diagnostic = if (macroImpl.typeParams.sizeCompare(targs) > 0) "has too few type arguments" else "has too many arguments"
      implRefError(s"macro implementation reference $diagnostic for " + treeSymTypeMsg(macroImplRef))
    }

    private def macroImplementationWording =
      if (isImplBundle) "bundle implementation"
      else "macro implementation"

    def MacroImplNotPublicError() = implRefError(s"${macroImplementationWording} must be public")

    def MacroImplOverloadedError() = implRefError(s"${macroImplementationWording} cannot be overloaded")

    def MacroImplNonTagImplicitParameters(params: List[Symbol]) = implRefError(s"${macroImplementationWording}s cannot have implicit parameters other than WeakTypeTag evidences")

    // compatibility errors

    // helpers

    private def lengthMsg(flavor: String, violation: String, extra: Symbol) = {
      val noun = if (flavor == "value") "parameter" else "type parameter"
      val message = noun + " lists have different length, " + violation + " extra " + noun
      val suffix = if (extra ne NoSymbol) " " + extra.defString else ""
      message + suffix
    }

    private def abbreviateCoreAliases(s: String): String = {
      val coreAliases = List("WeakTypeTag", "Expr", "Tree")
      coreAliases.foldLeft(s)((res, x) => res.replace("c.universe." + x, "c." + x))
    }

    private def showMeth(pss: List[List[Symbol]], restpe: Type, abbreviate: Boolean, untype: Boolean) = {
      def preprocess(tpe: Type) = if (untype) untypeMetalevel(tpe) else tpe
      var pssPart = pss.map(_.map(p => p.defStringSeenAs(preprocess(p.info))).mkString("(", ", ", ")")).mkString
      if (abbreviate) pssPart = abbreviateCoreAliases(pssPart)
      var retPart = preprocess(restpe).toString
      if (abbreviate || macroDdef.tpt.tpe == null) retPart = abbreviateCoreAliases(retPart)
      pssPart + ": " + retPart
    }

    // not exactly an error generator, but very related
    // and I dearly wanted to push it away from Macros.scala
    private def checkConforms(slot: String, rtpe: Type, atpe: Type) = {
      val verbose = macroDebugVerbose

      @tailrec
      def check(rtpe: Type, atpe: Type): Boolean = {
        def success() = { if (verbose) println(f"$rtpe <: $atpe?%ntrue"); true }
        (rtpe, atpe) match {
          case _ if rtpe eq atpe => success()
          case (TypeRef(_, RepeatedParamClass, rtpe :: Nil), TypeRef(_, RepeatedParamClass, atpe :: Nil)) => check(rtpe, atpe)
          case (ExprClassOf(_), TreeType()) if rtpe.prefix =:= atpe.prefix => success()
          case (SubtreeType(), ExprClassOf(_)) if rtpe.prefix =:= atpe.prefix => success()
          case _ => rtpe <:< atpe
        }
      }

      val ok =
        if (verbose) withTypesExplained(check(rtpe, atpe))
        else check(rtpe, atpe)
      if (!ok) {
        if (!verbose) explainTypes(rtpe, atpe)
        val msg = {
          val ss = Seq(rtpe, atpe) map (this abbreviateCoreAliases _.toString)
          s"type mismatch for $slot: ${ss(0)} does not conform to ${ss(1)}"
        }
        compatibilityError(msg)
      }
    }

    private def compatibilityError(message: String) =
      implRefError(
        s"${macroImplementationWording} has incompatible shape:"+
        "\n required: " + showMeth(rparamss, rret, abbreviate = true, untype = false) +
        "\n or      : " + showMeth(rparamss, rret, abbreviate = true, untype = true) +
        "\n found   : " + showMeth(aparamss, aret, abbreviate = false, untype = false) +
        "\n" + message)

    def MacroImplParamssMismatchError() = compatibilityError("number of parameter sections differ")

    def MacroImplExtraParamsError(aparams: List[Symbol], rparams: List[Symbol]) = compatibilityError(lengthMsg("value", "found", aparams(rparams.length)))

    def MacroImplMissingParamsError(aparams: List[Symbol], rparams: List[Symbol]) = compatibilityError(abbreviateCoreAliases(lengthMsg("value", "required", rparams(aparams.length))))

    def checkMacroImplParamTypeMismatch(atpe: Type, rparam: Symbol) = checkConforms("parameter " + rparam.name, rparam.tpe, atpe)

    def checkMacroImplResultTypeMismatch(atpe: Type, rret: Type) = checkConforms("return type", atpe, rret)

    def MacroImplParamNameMismatchError(aparam: Symbol, rparam: Symbol) = compatibilityError("parameter names differ: " + rparam.name + " != " + aparam.name)

    def MacroImplVarargMismatchError(aparam: Symbol, rparam: Symbol) = {
      def fail(paramName: Name) = compatibilityError("types incompatible for parameter " + paramName + ": corresponding is not a vararg parameter")
      if (isRepeated(rparam) && !isRepeated(aparam)) fail(rparam.name)
      if (!isRepeated(rparam) && isRepeated(aparam)) fail(aparam.name)
    }

    def MacroImplTargMismatchError(atargs: List[Type], atparams: List[Symbol]) =
      compatibilityError(NotWithinBoundsErrorMessage("", atargs, atparams, macroDebugVerbose || settings.explaintypes.value))

    def MacroImplTparamInstantiationError(atparams: List[Symbol], e: NoInstance) = {
      val badps = atparams map (_.defString) mkString ", "
      compatibilityError(f"type parameters $badps cannot be instantiated%n${e.getMessage}")
    }
  }
}
