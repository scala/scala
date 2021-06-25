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

import scala.tools.nsc.Reporting.WarningCategory

/** A provider of the logic for assessing the validity of argument
 *  adaptations, such as tupling, unit-insertion, widening, etc.  Such
 *  logic is spread around the compiler, without much ability on the
 *  part of the user to tighten the potentially dangerous bits.
 *
 *  @author  Paul Phillips
 */
trait Adaptations {
  self: Analyzer =>

  import global._
  import definitions._

  trait Adaptation {
    self: Typer =>

    def checkValidAdaptation(t: Tree, args: List[Tree]): Boolean = {
      def applyArg = t match {
        case Apply(_, arg :: Nil) => arg
        case _                    => EmptyTree
      }
      def isInfix = t match {
        case Apply(_, arg :: Nil) => t.hasAttachment[MultiargInfixAttachment.type]
        case _                    => false
      }
      def callString = (
        ( if (t.symbol.isConstructor) "new " else "" ) +
        ( t.symbol.owner.decodedName ) +
        ( if (t.symbol.isConstructor || t.symbol.name == nme.apply) "" else "." + t.symbol.decodedName )
      )
      def sigString = t.symbol.owner.decodedName + (
        if (t.symbol.isConstructor) t.symbol.signatureString
        else "." + t.symbol.decodedName + t.symbol.signatureString
      )
      def givenString = if (args.isEmpty) "<none>" else args.mkString(", ")
      def adaptedArgs = if (args.isEmpty) "(): Unit" else args.mkString("(", ", ", "): " + applyArg.tpe)

      def adaptWarningMessage(msg: String, showAdaptation: Boolean = true) = msg +
        "\n        signature: " + sigString +
        "\n  given arguments: " + givenString +
        (if (showAdaptation) "\n after adaptation: " + callString + "(" + adaptedArgs + ")" else "")

      // A one-argument method accepting Object (which may look like "Any"
      // at this point if the class is java defined) is a "leaky target" for
      // which we should be especially reluctant to insert () or auto-tuple.
      def isLeakyTarget = {
        val oneArgObject = t.symbol.paramss match {
          case (param :: Nil) :: Nil  => ObjectClass isSubClass param.tpe.typeSymbol
          case _                      => false
        }
        // Unfortunately various "universal" methods and the manner in which
        // they are used limits our ability to enforce anything sensible until
        // an opt-in compiler option is given.
        oneArgObject && !(
             currentRun.runDefinitions.isStringAddition(t.symbol)
          || currentRun.runDefinitions.isArrowAssoc(t.symbol)
          || t.symbol.name == nme.equals_
          || t.symbol.name == nme.EQ
          || t.symbol.name == nme.NE
        )
      }
      @inline def msg(what: String): String = s"adaptation of an empty argument list by inserting () $what"
      @inline def noAdaptation = {
        context.error(t.pos, adaptWarningMessage(msg("has been removed"), showAdaptation = false))
        false // drop adaptation
      }
      @inline def deprecatedAdaptation = {
        val twist =
          if (isLeakyTarget) "leaky (Object-receiving) target makes this especially dangerous"
          else "this is unlikely to be what you want"
        val text = s"${msg("is deprecated")}: ${twist}"
        context.deprecationWarning(t.pos, t.symbol, adaptWarningMessage(text), "2.11.0")
        true // keep adaptation
      }
      @inline def warnAdaptation = {
        if (settings.warnAdaptedArgs && !isInfix) context.warning(t.pos, adaptWarningMessage(
          s"adapted the argument list to the expected ${args.size}-tuple: add additional parens instead"),
          WarningCategory.LintAdaptedArgs)
        true // keep adaptation
      }
      if (args.nonEmpty)
        warnAdaptation
      else if (currentRun.isScala3)
        noAdaptation
      else
        deprecatedAdaptation
    }
  }
}
