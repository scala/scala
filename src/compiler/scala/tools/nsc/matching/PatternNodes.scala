/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author Burak Emir
 */
// $Id$

package scala.tools.nsc
package matching

import scala.tools.nsc.util.NoPosition

/**
 *  @author Burak Emir
 */
trait PatternNodes extends ast.TreeDSL
{
  self: transform.ExplicitOuter =>

  import global.{ typer => _, _ }
  import CODE._
  import definitions.{ ListClass, ConsClass }

  object Types {
    import definitions._
    implicit def enrichType(x: Type): RichType = new RichType(x)

    class RichType(undecodedTpe: Type) {
      def tpe = decodedEqualsType(undecodedTpe)
      def isAnyRef = tpe <:< AnyRefClass.tpe

      // These tests for final classes can inspect the typeSymbol
      private def is(s: Symbol) = tpe.typeSymbol eq s
      def      isByte = is(ByteClass)
      def     isShort = is(ShortClass)
      def       isInt = is(IntClass)
      def      isChar = is(CharClass)
      def   isBoolean = is(BooleanClass)
      def   isNothing = is(NothingClass)
      def     isArray = is(ArrayClass)
    }
  }

  /** For folding a list into a well-typed x :: y :: etc :: tree. */
  private def listFolder(tpe: Type) = {
    val MethodType(_, TypeRef(pre, sym, _)) = ConsClass.primaryConstructor.tpe
    val consRef                             = typeRef(pre, sym, List(tpe))
    val listRef                             = typeRef(pre, ListClass, List(tpe))

    def fold(x: Tree, xs: Tree) = unbind(x) match {
      case _: Star  => makeBind(Pattern(x).definedVars, WILD(x.tpe))
      case _        =>
        val dummyMethod = new TermSymbol(NoSymbol, NoPosition, "matching$dummy")
        val consType    = MethodType(dummyMethod newSyntheticValueParams List(tpe, listRef), consRef)

        Apply(TypeTree(consType), List(x, xs)) setType consRef
    }

    fold _
  }

  def normalizedListPattern(pats: List[Tree], tptArg: Type): Tree =
    pats.foldRight(gen.mkNil)(listFolder(tptArg))
}
