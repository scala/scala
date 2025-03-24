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

import scala.annotation.tailrec
import scala.util.matching.Regex

trait SplainData {
  self: Analyzer =>

  import global._

  sealed trait ImplicitErrorSpecifics

  object ImplicitErrorSpecifics {
    case class NotFound(param: Symbol) extends ImplicitErrorSpecifics

    case class NonconformantBounds(
        targs: List[Type], tparams: List[Symbol], originalError: Option[AbsTypeError],
    ) extends ImplicitErrorSpecifics
  }

  object ImplicitErrors {
    var stack: List[Type]           = Nil
    var errors: List[ImplicitError] = Nil

    def push(error: ImplicitError): Unit = errors ::= error
    def nesting: Int                     = stack.length - 1
    def nested: Boolean                  = stack.nonEmpty
    def removeErrorsFor(tpe: Type): Unit = errors = errors.dropWhile(_.tpe == tpe)

    def startSearch(expectedType: Type): Unit = {
      if (settings.Vimplicits.value) {
        if (!nested) errors = List()
        stack = expectedType :: stack
      }
    }

    def finishSearch(success: Boolean, expectedType: Type): Unit = {
      if (settings.Vimplicits.value) {
        if (success) removeErrorsFor(expectedType)
        stack = stack.drop(1)
      }
    }
  }

  case class ImplicitError(tpe: Type, candidate: Tree, nesting: Int, specifics: ImplicitErrorSpecifics) {
    import ImplicitError._

    override def equals(other: Any) = other match {
      case o: ImplicitError => o.tpe.toString == tpe.toString && candidateName(this) == candidateName(o)
      case _                => false
    }

    override def hashCode = (tpe.toString.##, ImplicitError.candidateName(this).##).##
    override def toString = s"ImplicitError(${shortName(tpe.toString)}, ${shortName(candidate.toString)}), $nesting, $specifics)"
  }

  object ImplicitError {
    def unapplyCandidate(e: ImplicitError): Tree = unapplyRecursively(e.candidate)

    @tailrec
    private def unapplyRecursively(tree: Tree): Tree =
      tree match {
        case TypeApply(fun, _) => unapplyRecursively(fun)
        case Apply(fun, _) => unapplyRecursively(fun)
        case a => a
      }

    def cleanCandidate(e: ImplicitError): String =
      unapplyCandidate(e).toString match {
        case ImplicitError.candidateRegex(suf) => suf
        case a => a
      }

    def candidateName(e: ImplicitError): String =
      unapplyCandidate(e) match {
        case Select(_, name) => name.toString
        case Ident(name)     => name.toString
        case a               => a.toString
      }

    val candidateRegex: Regex = """.*\.this\.(.*)""".r

    def shortName(ident: String): String = ident.substring(ident.lastIndexOf(".") + 1)
  }
}
