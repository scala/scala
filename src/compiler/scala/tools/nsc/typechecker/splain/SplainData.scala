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

import scala.util.matching.Regex

trait SplainData { self: Analyzer =>

  import global._

  sealed trait ImplicitErrorSpecifics

  object ImplicitErrorSpecifics
  {
    case class NotFound(param: Symbol)
    extends ImplicitErrorSpecifics

    case class NonconformantBounds(targs: List[Type], tparams: List[Symbol], originalError: Option[AbsTypeError])
    extends ImplicitErrorSpecifics
  }

  object ImplicitErrors
  {
    var stack: List[Type] = Nil

    var errors: List[ImplicitError] = Nil

    def push(error: ImplicitError): Unit = errors = error :: errors

    def nesting: Int = stack.length - 1

    def nested: Boolean = stack.nonEmpty

    def removeErrorsFor(tpe: Type): Unit = errors = errors.dropWhile(_.tpe == tpe)

    def startSearch(expectedType: Type): Unit = {
      if (settings.implicitsSettingEnable) {
        if (!nested) errors = List()
        stack = expectedType :: stack
      }
    }

    def finishSearch(success: Boolean, expectedType: Type): Unit = {
      if (settings.implicitsSettingEnable) {
        if (success) removeErrorsFor(expectedType)
        stack = stack.drop(1)
      }
    }
  }

  case class ImplicitError(tpe: Type, candidate: Tree, nesting: Int, specifics: ImplicitErrorSpecifics)
  {
    override def equals(other: Any) = other match {
      case o: ImplicitError =>
        o.tpe.toString == tpe.toString && ImplicitError.candidateName(this) == ImplicitError.candidateName(o)
      case _ => false
    }

    override def hashCode = (tpe.toString.hashCode, ImplicitError.candidateName(this).hashCode).hashCode

    override def toString: String =
        s"NotFound(${ImplicitError.shortName(tpe.toString)}, ${ImplicitError.shortName(candidate.toString)}), $nesting, $specifics)"
  }

  object ImplicitError
  {
    def notFound(tpe: Type, candidate: Tree, nesting: Int)(param: Symbol): ImplicitError =
      ImplicitError(tpe, candidate, nesting, ImplicitErrorSpecifics.NotFound(param))

    def nonconformantBounds
    (tpe: Type, candidate: Tree, nesting: Int)
    (targs: List[Type], tparams: List[Symbol], originalError: Option[AbsTypeError])
    : ImplicitError =
      ImplicitError(tpe, candidate, nesting, ImplicitErrorSpecifics.NonconformantBounds(targs, tparams, originalError))

    def unapplyCandidate(e: ImplicitError): Tree =
      e.candidate match {
        case TypeApply(name, _) => name
        case a => a
      }

    def candidateName(e: ImplicitError): String =
      unapplyCandidate(e) match {
        case Select(_, name) => name.toString
        case Ident(name) => name.toString
        case a => a.toString
      }

    val candidateRegex: Regex = """.*\.this\.(.*)""".r

    def cleanCandidate(e: ImplicitError): String =
      unapplyCandidate(e).toString match {
        case candidateRegex(suf) => suf
        case a => a
      }

    def shortName(ident: String): String = ident.split('.').toList.lastOption.getOrElse(ident)
  }
}
