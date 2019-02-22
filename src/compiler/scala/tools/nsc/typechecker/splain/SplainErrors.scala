/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package typechecker
package splain

trait SplainErrors { self: Analyzer with SplainFormatting =>
  import global._

  def splainPushNotFound(tree: Tree, param: Symbol): Unit =
    ImplicitErrors.stack
      .headOption
      .map(ImplicitError.notFound(_, tree, ImplicitErrors.nesting)(param))
      .foreach(err => ImplicitErrors.push(err))

  def splainPushOrReportNotFound(tree: Tree, param: Symbol, annotationMsg: Option[String]): Option[String] =
    if (settings.implicitsSettingEnable)
      if (ImplicitErrors.nested) {
        splainPushNotFound(tree, param)
        None
      }
      else pluginsNoImplicitFoundError(param, ImplicitErrors.errors, formatImplicitError(param, ImplicitErrors.errors, annotationMsg))
    else None

  def splainPushNonconformantBonds(
    tpe: Type,
    candidate: Tree,
    targs: List[Type],
    tparams: List[Symbol],
    originalError: Option[AbsTypeError],
  ): Unit = {
    if (settings.implicitsSettingEnable) {
      val err = ImplicitError.nonconformantBounds(tpe, candidate, ImplicitErrors.nesting)(targs, tparams, originalError)
      ImplicitErrors.push(err)
    }
  }

  def splainPushImplicitSearchFailure(implicitTree: Tree, expectedType: Type, originalError: AbsTypeError): Unit = {
    def pushImpFailure(fun: Tree, args: List[Tree]): Unit = {
      fun.tpe match {
        case PolyType(tparams, restpe) if tparams.nonEmpty && sameLength(tparams, args) =>
          val targs = mapList(args)(_.tpe)
          splainPushNonconformantBonds(expectedType, implicitTree, targs, tparams, Some(originalError))
        case _ => ()
      }
    }
    if (settings.implicitsSettingEnable) {
      (implicitTree: @unchecked) match {
        case TypeApply(fun, args) => pushImpFailure(fun, args)
        case Apply(TypeApply(fun, args), _) => pushImpFailure(fun, args)
      }
    }
  }
}
