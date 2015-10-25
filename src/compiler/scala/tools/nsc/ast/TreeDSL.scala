/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 *
 * @author  Paul Phillips
 */

package scala.tools.nsc
package ast

import scala.language.implicitConversions

/** A DSL for generating scala code.  The goal is that the
 *  code generating code should look a lot like the code it
 *  generates.
 */

trait TreeDSL {
  val global: Global

  import global._
  import definitions._

  object CODE {
    // Add a null check to a Tree => Tree function
    def nullSafe[T](f: Tree => Tree, ifNull: Tree): Tree => Tree =
      tree => IF (tree MEMBER_== NULL) THEN ifNull ELSE f(tree)

    def returning[T](x: T)(f: T => Unit): T = util.returning(x)(f)

    object LIT extends (Any => Literal) {
      def typed(x: Any)   = apply(x) setType ConstantType(Constant(x))
      def apply(x: Any)   = Literal(Constant(x))
    }

    // Boring, predictable trees.
    def TRUE  = LIT typed true
    def FALSE = LIT typed false
    def ZERO  = LIT(0)
    def NULL  = LIT(null)
    def UNIT  = LIT(())

    def fn(lhs: Tree, op:   Name, args: Tree*)  = Apply(Select(lhs, op), args.toList)
    def fn(lhs: Tree, op: Symbol, args: Tree*)  = Apply(Select(lhs, op), args.toList)

    class TreeMethods(target: Tree) {
      /** logical/comparison ops **/
      def OR(other: Tree) =
        if (target == EmptyTree) other
        else if (other == EmptyTree) target
        else gen.mkOr(target, other)

      def AND(other: Tree) =
        if (target == EmptyTree) other
        else if (other == EmptyTree) target
        else gen.mkAnd(target, other)

      /** Note - calling ANY_== in the matcher caused primitives to get boxed
       *  for the comparison, whereas looking up nme.EQ does not.  See #3570 for
       *  an example of how target.tpe can be non-null, yet it claims not to have
       *  a member called nme.EQ.  Not sure if that should happen, but we can be
       *  robust by dragging in Any regardless.
       */
      def MEMBER_== (other: Tree)   = {
        val opSym = if (target.tpe == null) NoSymbol else target.tpe member nme.EQ
        if (opSym == NoSymbol) ANY_==(other)
        else fn(target, opSym, other)
      }
      def ANY_EQ  (other: Tree)     = OBJ_EQ(other AS ObjectTpe)
      def ANY_==  (other: Tree)     = fn(target, Any_==, other)
      def ANY_!=  (other: Tree)     = fn(target, Any_!=, other)
      def OBJ_EQ  (other: Tree)     = fn(target, Object_eq, other)
      def OBJ_NE  (other: Tree)     = fn(target, Object_ne, other)

      def INT_>=  (other: Tree)     = fn(target, getMember(IntClass, nme.GE), other)
      def INT_==  (other: Tree)     = fn(target, getMember(IntClass, nme.EQ), other)
      def INT_-   (other: Tree)     = fn(target, getMember(IntClass, nme.MINUS), other)

      // generic operations on ByteClass, IntClass, LongClass
      def GEN_|   (other: Tree, kind: ClassSymbol)  = fn(target, getMember(kind, nme.OR), other)
      def GEN_&   (other: Tree, kind: ClassSymbol)  = fn(target, getMember(kind, nme.AND), other)
      def GEN_==  (other: Tree, kind: ClassSymbol)  = fn(target, getMember(kind, nme.EQ), other)
      def GEN_!=  (other: Tree, kind: ClassSymbol)  = fn(target, getMember(kind, nme.NE), other)

      /** Apply, Select, Match **/
      def APPLY(params: Tree*)      = Apply(target, params.toList)
      def APPLY(params: List[Tree]) = Apply(target, params)

      def DOT(member: Name)         = SelectStart(Select(target, member))
      def DOT(sym: Symbol)          = SelectStart(Select(target, sym))

      /** Assignment */
      // !!! This method is responsible for some tree sharing, but a diligent
      // reviewer pointed out that we shouldn't blindly duplicate these trees
      // as there might be DefTrees nested beneath them.  It's not entirely
      // clear how to proceed, so for now it retains the non-duplicating behavior.
      def ===(rhs: Tree)            = Assign(target, rhs)

      /** Casting & type tests -- working our way toward understanding exactly
       *  what differs between the different forms of IS and AS.
       *
       *  See ticket #2168 for one illustration of AS vs. AS_ANY.
       */
      def AS(tpe: Type)       = gen.mkAsInstanceOf(target, tpe, any = true, wrapInApply = false)
      def IS_OBJ(tpe: Type)   = gen.mkIsInstanceOf(target, tpe, any = false)

      def GETCLASS()          = fn(target, Object_getClass)
    }

    case class SelectStart(tree: Select) {
      def apply(args: Tree*) = Apply(tree, args.toList)
    }

    class CaseStart(pat: Tree, guard: Tree) {
      def IF(g: Tree): CaseStart    = new CaseStart(pat, g)
      def ==>(body: Tree): CaseDef  = CaseDef(pat, guard, body)
    }

    class IfStart(cond: Tree, thenp: Tree) {
      def THEN(x: Tree)     = new IfStart(cond, x)
      def ELSE(elsep: Tree) = If(cond, thenp, elsep)
      def ENDIF             = If(cond, thenp, EmptyTree)
    }
    class TryStart(body: Tree, catches: List[CaseDef], fin: Tree) {
      def CATCH(xs: CaseDef*) = new TryStart(body, xs.toList, fin)
      def ENDTRY              = Try(body, catches, fin)
    }

    def CASE(pat: Tree): CaseStart  = new CaseStart(pat, EmptyTree)
    def DEFAULT: CaseStart          = new CaseStart(Ident(nme.WILDCARD), EmptyTree)

    def NEW(tpt: Tree, args: Tree*): Tree   = New(tpt, List(args.toList))

    def NOT(tree: Tree)   = Select(tree, Boolean_not)
    def AND(guards: Tree*) = if (guards.isEmpty) EmptyTree else guards reduceLeft gen.mkAnd

    def IF(tree: Tree)    = new IfStart(tree, EmptyTree)
    def TRY(tree: Tree)   = new TryStart(tree, Nil, EmptyTree)
    def BLOCK(xs: Tree*)  = Block(xs.init.toList, xs.last)
    def SOME(xs: Tree*)   = Apply(SomeClass.companionSymbol, gen.mkTuple(xs.toList))

    /** Typed trees from symbols. */
    def REF(sym: Symbol)            = gen.mkAttributedRef(sym)
    def REF(pre: Type, sym: Symbol) = gen.mkAttributedRef(pre, sym)

    /** Implicits - some of these should probably disappear **/
    implicit def mkTreeMethods(target: Tree): TreeMethods = new TreeMethods(target)
    implicit def mkTreeMethodsFromSymbol(target: Symbol): TreeMethods = new TreeMethods(Ident(target))

    /** (foo DOT bar) might be simply a Select, but more likely it is to be immediately
     *  followed by an Apply.  We don't want to add an actual apply method to arbitrary
     *  trees, so SelectStart is created with an apply - and if apply is not the next
     *  thing called, the implicit from SelectStart -> Tree will provide the tree.
     */
    implicit def mkTreeFromSelectStart(ss: SelectStart): Select = ss.tree
    implicit def mkTreeMethodsFromSelectStart(ss: SelectStart): TreeMethods = mkTreeMethods(ss.tree)
  }
}
