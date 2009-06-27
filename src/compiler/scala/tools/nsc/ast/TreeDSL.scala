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
    def NULL          = LIT(null)
    def UNIT          = LIT(())
    def ZERO          = LIT(0)
    def WILD          = Ident(nme.WILDCARD)

    case class ExpectApply(target: Tree) {
      def apply(args: Tree*) = Apply(target, args.toList)
    }

    class TreeMethods(target: Tree) {
      private def binop(lhs: Tree, op: Name, rhs: Tree)   = Apply(Select(lhs, op), List(rhs))
      private def binop(lhs: Tree, op: Symbol, rhs: Tree) = Apply(Select(lhs, op), List(rhs))
      private def toAnyRef(x: Tree) = x setType AnyRefClass.tpe

      /** logical/comparison ops **/
      def OR(other: Tree) =
        if (target == EmptyTree) other
        else if (other == EmptyTree) target
        else gen.mkOr(target, other)

      def AND(other: Tree) =
        if (target == EmptyTree) other
        else if (other == EmptyTree) target
        else gen.mkAnd(target, other)

      def BIT_AND(other: Tree)      = binop(target, Int_And, other)
      def EQREF(other: Tree)        = binop(target, nme.eq, toAnyRef(other))
      def NE_REF(other: Tree)       = binop(target, nme.ne, other)
      def EQEQ(other: Tree)         = binop(target, nme.EQ, other)
      def EQINT(other: Tree)        = binop(target, Int_==, other)
      def EQANY(other: Tree)        = binop(target, Any_==, other)
      def NOT_==(other: Tree)       = binop(target, Object_ne, other)

      /** Apply, Select, Match **/
      def APPLY(params: List[Tree]) = Apply(target, params)
      def MATCH(cases: CaseDef*)    = Match(target, cases.toList)
      def DOT(member: Name)         = ExpectApply(Select(target, member))
      def DOT(sym: Symbol)          = ExpectApply(Select(target, sym))

      /** Assignment */
      def ===(rhs: Tree)            = Assign(target, rhs)

      /** Casting & type tests -- working our way toward understanding exactly
       *  what differs between the different forms of IS and AS.
       */
      def AS(tpe: Type)       = TypeApply(Select(target, Any_asInstanceOf), List(TypeTree(tpe)))
      def AS_ATTR(tpe: Type)  = gen.mkAttributedCast(target, tpe)

      def IS(tpe: Type)       = gen.mkIsInstanceOf(target, tpe, true)
      def IS_OBJ(tpe: Type)   = gen.mkIsInstanceOf(target, tpe, false)

      def TOSTRING()  = Apply(Select(target, nme.toString_), Nil)
      def GETCLASS()  = Apply(Select(target, Object_getClass), Nil)
    }

    class CaseStart(pat: Tree, guard: Tree) {
      def IF(g: Tree): CaseStart    = new CaseStart(pat, g)
      def ==>(body: Tree): CaseDef  = CaseDef(pat, guard, body)
    }

    abstract class ValOrDefStart(sym: Symbol) {
      def ===(body: Tree): ValOrDefDef
    }
    class DefStart(sym: Symbol) extends ValOrDefStart(sym) {
      def ===(body: Tree) = DefDef(sym, body)
    }
    class ValStart(sym: Symbol) extends ValOrDefStart(sym) {
      def ===(body: Tree) = ValDef(sym, body)
    }
    class IfStart(cond: Tree, thenp: Tree) {
      def THEN(x: Tree) = new IfStart(cond, x)
      def ELSE(elsep: Tree) = If(cond, thenp, elsep)
      def ENDIF = If(cond, thenp, EmptyTree)
    }
    class TryStart(body: Tree, catches: List[CaseDef], fin: Tree) {
      def CATCH(xs: CaseDef*) = new TryStart(body, xs.toList, fin)
      def FINALLY(x: Tree)    = Try(body, catches, x)
      def ENDTRY              = Try(body, catches, fin)
    }

    def CASE(pat: Tree): CaseStart  = new CaseStart(pat, EmptyTree)
    def DEFAULT: CaseStart          = new CaseStart(WILD, EmptyTree)

    class NameMethods(target: Name) {
      def BIND(body: Tree) = Bind(target, body)
    }

    class SymbolMethods(target: Symbol) {
      def BIND(body: Tree) = Bind(target, body)
      // def DOT(member: Symbol) = new TreeMethods(Ident(target)) DOT member

      // name of nth indexed argument to a method (first parameter list), defaults to 1st
      def ARG(idx: Int = 0) = Ident(target.paramss.head(idx))
      def ARGS = target.paramss.head
      def ARGNAMES = ARGS map Ident
    }

    /** Top level accessible. */
    def THROW(sym: Symbol, msg: Tree = null) = {
      val arg = if (msg == null) Nil else List(msg.TOSTRING)
      Throw(New(TypeTree(sym.tpe), List(arg)))
    }
    def NEW(tpe: Tree, args: Tree*) = New(tpe, List(args.toList))

    def VAL(sym: Symbol) = new ValStart(sym)
    def DEF(sym: Symbol) = new DefStart(sym)
    def AND(guards: Tree*) =
      if (guards.isEmpty) EmptyTree
      else guards reduceLeft gen.mkAnd

    def IF(tree: Tree)    = new IfStart(tree, EmptyTree)
    def TRY(tree: Tree)   = new TryStart(tree, Nil, EmptyTree)
    def REF(sym: Symbol)  = gen.mkAttributedRef(sym)
    def BLOCK(xs: Tree*)  = Block(xs.init.toList, xs.last)

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