/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
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

    import runDefinitions._

    /** True to keep this adapted tree. */
    def checkValidAdaptation(t: Tree, args: List[Tree]): Boolean = {
      def applyArg = t match {
        case Apply(_, arg :: Nil) => arg
        case _                    => EmptyTree
      }
      def callString = s"${
          if (t.symbol.isConstructor) "new " else ""
        }${
          t.symbol.owner.decodedName
        }${
          if (t.symbol.isConstructor || t.symbol.name == nme.apply) "" else s".${ t.symbol.decodedName }"
        }"
      def sigString = s"${
          t.symbol.owner.decodedName
        }${
          if (t.symbol.isConstructor) t.symbol.signatureString
          else s".${t.symbol.decodedName}${t.symbol.signatureString}"
        }"
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
             isStringAddition(t.symbol)
          || isArrowAssoc(t.symbol)
          || t.symbol.name == nme.equals_
          || t.symbol.name == nme.EQ
          || t.symbol.name == nme.NE
        )
      }

      def error(msg: String) = { context.error(t.pos, adaptWarningMessage(msg)) ; false }
      def short(msg: String) = { context.error(t.pos, adaptWarningMessage(msg, showAdaptation = false)) ; false }
      def scare(msg: String) = { context.warning(t.pos, adaptWarningMessage(msg)) ; false }
      def warn (msg: String) = { context.warning(t.pos, adaptWarningMessage(msg)) ; true }
      def depp (msg: String) = { context.deprecationWarning(t.pos, t.symbol, adaptWarningMessage(msg)) ; true }

      if (settings.YnoTupling)
        error("Tupling of arguments is disabled: use explicit parentheses.")
      else if (t.symbol.isJavaDefined && settings.YnoJavaTupling)
        error("No adaptation of argument list for Java method.")
      else if (settings.noAdaptedArgs)
        scare("No automatic adaptation here: use explicit parentheses.")
      else if (args.isEmpty) {
        if (settings.future)
          short("Adaptation of argument list by inserting () has been removed.")
        else
          depp(s"Adaptation of argument list by inserting () has been deprecated: ${
            if (isLeakyTarget) "leaky (Object-receiving) target makes this especially dangerous."
            else "this is unlikely to be what you want."
          }")
      } else if (settings.warnAdaptedArgs)
        warn(s"Adapting argument list by creating a ${args.size}-tuple: this may not be what you want.")
      else true
    }
  }
}
