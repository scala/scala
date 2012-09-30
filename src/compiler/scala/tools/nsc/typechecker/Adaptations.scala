/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package typechecker

/** This trait provides logic for assessing the validity of argument
 *  adaptations, such as tupling, unit-insertion, widening, etc.  Such
 *  logic is spread around the compiler, without much ability on the
 *  part of the user to tighten the potentially dangerous bits.
 *
 *  TODO: unifying/consolidating said logic under consistent management.
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

      def adaptWarning(msg: String) = context.warning(t.pos, msg +
        "\n        signature: " + sigString +
        "\n  given arguments: " + givenString +
        "\n after adaptation: " + callString + "(" + adaptedArgs + ")"
      )
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
             isStringAddition(t.symbol)
          || isArrowAssoc(t.symbol)
          || t.symbol.name == nme.equals_
          || t.symbol.name == nme.EQ
          || t.symbol.name == nme.NE
        )
      }

      if (settings.noAdaptedArgs.value)
        adaptWarning("No automatic adaptation here: use explicit parentheses.")
      else if (settings.warnAdaptedArgs.value)
        adaptWarning(
          if (args.isEmpty) "Adapting argument list by inserting (): " + (
            if (isLeakyTarget) "leaky (Object-receiving) target makes this especially dangerous."
            else "this is unlikely to be what you want."
          )
          else "Adapting argument list by creating a " + args.size + "-tuple: this may not be what you want."
        )

      !settings.noAdaptedArgs.value
    }
  }
}
