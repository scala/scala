/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 *
 * @author  Paul Phillips
 */

package scala.tools.nsc.ast

/** A DSL for generating scala code.  The goal is that the
 *  code generating code should look a lot like the code it
 *  generates.
 */

trait TreeDSL {
  val global: Global

  import global._
  import definitions._

  object CODE {
    def LIT(x: Any)   = Literal(Constant(x))
    def TRUE          = LIT(true)
    def FALSE         = LIT(false)
    def WILD          = Ident(nme.WILDCARD)

    class TreeMethods(target: Tree) {
      private def binop(lhs: Tree, op: Name, rhs: Tree) = Apply(Select(lhs, op), List(rhs))
      private def toAnyRef(x: Tree) = x setType AnyRefClass.tpe

      case class ExpectApply(target: Tree) {
        def apply(args: Tree*) = Apply(target, args.toList)
      }

      /** logical/comparison ops **/
      def OR(other: Tree) =
        if (target == EmptyTree) other
        else if (other == EmptyTree) target
        else gen.mkOr(target, other)

      def AND(other: Tree) =
        if (target == EmptyTree) other
        else if (other == EmptyTree) target
        else gen.mkAnd(target, other)

      def EQREF(other: Tree)        = binop(target, nme.eq, toAnyRef(other))
      def EQEQ(other: Tree)         = binop(target, nme.EQ, other)

      /** Apply, Select, Match **/
      def APPLY(params: List[Tree]) = Apply(target, params)
      def MATCH(cases: CaseDef*)    = Match(target, cases.toList)
      def DOT(member: Name)         = ExpectApply(Select(target, member))
      def DOT(sym: Symbol)          = ExpectApply(Select(target, sym))

      /** Casting */
      def AS(tpe: Type) = TypeApply(Select(target, Any_asInstanceOf), List(TypeTree(tpe)))
      def TOSTRING() = Select(target, nme.toString_)
    }

    class CaseStart(pat: Tree, guard: Tree) {
      def IF(g: Tree): CaseStart    = new CaseStart(pat, g)
      def ==>(body: Tree): CaseDef  = CaseDef(pat, guard, body) // DSL for =>
    }
    class DefStart(sym: Symbol) {
      def ===(body: Tree) = DefDef(sym, body) // DSL for =
    }

    def CASE(pat: Tree): CaseStart  = new CaseStart(pat, EmptyTree)
    def DEFAULT: CaseStart          = new CaseStart(WILD, EmptyTree)

    class NameMethods(target: Name) {
      def BIND(body: Tree) = Bind(target, body)
    }

    class SymbolMethods(target: Symbol) {
      def BIND(body: Tree) = Bind(target, body)

      // name of nth indexed argument to a method (first parameter list), defaults to 1st
      def ARG(idx: Int = 0) = Ident(target.paramss.head(idx))
      def ARGS = target.paramss.head
      def ARGNAMES = ARGS map Ident
    }

    /** Top level accessible. */
    def THROW(sym: Symbol, msg: Tree) = Throw(New(TypeTree(sym.tpe), List(List(msg.TOSTRING))))
    def DEF(sym: Symbol) = new DefStart(sym)
    def AND(guards: Tree*) =
      if (guards.isEmpty) EmptyTree
      else guards reduceLeft gen.mkAnd

    /** Implicits - some of these should probably disappear **/
    implicit def mkTreeMethods(target: Tree): TreeMethods = new TreeMethods(target)
    implicit def mkTreeMethodsFromSymbol(target: Symbol): TreeMethods = new TreeMethods(Ident(target))
    implicit def mkTreeMethodsFromName(target: Name): TreeMethods = new TreeMethods(Ident(target))
    implicit def mkTreeMethodsFromString(target: String): TreeMethods = new TreeMethods(Ident(target))

    implicit def mkNameMethodsFromName(target: Name): NameMethods = new NameMethods(target)
    implicit def mkNameMethodsFromString(target: String): NameMethods = new NameMethods(target)

    implicit def mkSymbolMethodsFromSymbol(target: Symbol): SymbolMethods = new SymbolMethods(target)
  }
}