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

package scala.tools.nsc
package typechecker
package splain

trait SplainErrors { self: Analyzer with SplainFormatting =>
  import global._

  def splainPushNotFound(tree: Tree, param: Symbol): Unit =
    ImplicitErrors.stack.headOption.foreach { pt =>
      val specifics = ImplicitErrorSpecifics.NotFound(param)
      ImplicitErrors.push(ImplicitError(pt, tree, ImplicitErrors.nesting, specifics))
    }

  def splainPushOrReportNotFound(tree: Tree, param: Symbol, annotationMsg: String): String =
    if (settings.Vimplicits.value)
      if (ImplicitErrors.nested) {
        splainPushNotFound(tree, param)
        ""
      }
      else pluginsNoImplicitFoundError(param, ImplicitErrors.errors, formatImplicitError(param, ImplicitErrors.errors, annotationMsg))
    else ""

  def splainPushNonconformantBonds(
    tpe: Type,
    candidate: Tree,
    targs: List[Type],
    tparams: List[Symbol],
    originalError: Option[AbsTypeError],
  ): Unit = {
    if (settings.Vimplicits.value) {
      val specifics = ImplicitErrorSpecifics.NonconformantBounds(targs, tparams, originalError)
      ImplicitErrors.push(ImplicitError(tpe, candidate, ImplicitErrors.nesting, specifics))
    }
  }

  def splainPushImplicitSearchFailure(implicitTree: Tree, expectedType: Type, originalError: AbsTypeError): Unit = {
    def pushImpFailure(fun: Tree, args: List[Tree]): Unit = {
      fun.tpe match {
        case PolyType(tparams, _) if tparams.nonEmpty && sameLength(tparams, args) =>
          splainPushNonconformantBonds(expectedType, implicitTree, mapList(args)(_.tpe), tparams, Some(originalError))
        case _ =>
      }
    }
    if (settings.Vimplicits.value) {
      implicitTree match {
        case       TypeApply(fun, args)     => pushImpFailure(fun, args)
        case Apply(TypeApply(fun, args), _) => pushImpFailure(fun, args)
        case _                              =>
      }
    }
  }
}
