/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 *
 * @author  Paul Phillips
 */

package scala.tools.nsc
package ast

import PartialFunction._
import symtab.Flags
import language.implicitConversions

/** A DSL for generating scala code.  The goal is that the
 *  code generating code should look a lot like the code it
 *  generates.
 */

trait TreeDSL {
  val global: Global

  import global._
  import definitions._
  import gen.{ scalaDot }

  object CODE {
    // Add a null check to a Tree => Tree function
    def nullSafe[T](f: Tree => Tree, ifNull: Tree): Tree => Tree =
      tree => IF (tree MEMBER_== NULL) THEN ifNull ELSE f(tree)

    def returning[T](x: T)(f: T => Unit): T = util.returning(x)(f)

    object LIT extends (Any => Literal) {
      def apply(x: Any)   = Literal(Constant(x))
      def unapply(x: Any) = condOpt(x) { case Literal(Constant(value)) => value }
    }

    // You might think these could all be vals, but empirically I have found that
    // at least in the case of UNIT the compiler breaks if you re-use trees.
    // However we need stable identifiers to have attractive pattern matching.
    // So it's inconsistent until I devise a better way.
    val TRUE          = LIT(true)
    val FALSE         = LIT(false)
    val ZERO          = LIT(0)
    def NULL          = LIT(null)
    def UNIT          = LIT(())

    // for those preferring boring, predictable lives, without the thrills of tree-sharing
    // (but with the perk of typed trees)
    def TRUE_typed  = LIT(true) setType ConstantType(Constant(true))
    def FALSE_typed = LIT(false) setType ConstantType(Constant(false))

    object WILD {
      def empty               = Ident(nme.WILDCARD)
      def apply(tpe: Type)    = Ident(nme.WILDCARD) setType tpe
      def unapply(other: Any) = cond(other) { case Ident(nme.WILDCARD) => true }
    }

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
      def ANY_EQ  (other: Tree)     = OBJ_EQ(other AS ObjectClass.tpe)
      def ANY_==  (other: Tree)     = fn(target, Any_==, other)
      def ANY_!=  (other: Tree)     = fn(target, Any_!=, other)
      def OBJ_==  (other: Tree)     = fn(target, Object_==, other)
      def OBJ_!=  (other: Tree)     = fn(target, Object_!=, other)
      def OBJ_EQ  (other: Tree)     = fn(target, Object_eq, other)
      def OBJ_NE  (other: Tree)     = fn(target, Object_ne, other)

      def INT_|   (other: Tree)     = fn(target, getMember(IntClass, nme.OR), other)
      def INT_&   (other: Tree)     = fn(target, getMember(IntClass, nme.AND), other)
      def INT_>=  (other: Tree)     = fn(target, getMember(IntClass, nme.GE), other)
      def INT_==  (other: Tree)     = fn(target, getMember(IntClass, nme.EQ), other)
      def INT_!=  (other: Tree)     = fn(target, getMember(IntClass, nme.NE), other)
      
      // generic operations on ByteClass, IntClass, LongClass
      def GEN_|   (other: Tree, kind: ClassSymbol)  = fn(target, getMember(kind, nme.OR), other)
      def GEN_&   (other: Tree, kind: ClassSymbol)  = fn(target, getMember(kind, nme.AND), other)
      def GEN_==  (other: Tree, kind: ClassSymbol)  = fn(target, getMember(kind, nme.EQ), other)
      def GEN_!=  (other: Tree, kind: ClassSymbol)  = fn(target, getMember(kind, nme.NE), other)

      def BOOL_&& (other: Tree)     = fn(target, Boolean_and, other)
      def BOOL_|| (other: Tree)     = fn(target, Boolean_or, other)

      /** Apply, Select, Match **/
      def APPLY(params: Tree*)      = Apply(target, params.toList)
      def APPLY(params: List[Tree]) = Apply(target, params)
      def MATCH(cases: CaseDef*)    = Match(target, cases.toList)

      def DOT(member: Name)         = SelectStart(Select(target, member))
      def DOT(sym: Symbol)          = SelectStart(Select(target, sym))

      /** Assignment */
      def ===(rhs: Tree)            = Assign(target, rhs)

      /** Methods for sequences **/
      def DROP(count: Int): Tree =
        if (count == 0) target
        else (target DOT nme.drop)(LIT(count))

      /** Casting & type tests -- working our way toward understanding exactly
       *  what differs between the different forms of IS and AS.
       *
       *  See ticket #2168 for one illustration of AS vs. AS_ANY.
       */
      def AS(tpe: Type)       = gen.mkAsInstanceOf(target, tpe, any = true, wrapInApply = false)
      def IS(tpe: Type)       = gen.mkIsInstanceOf(target, tpe, true)
      def IS_OBJ(tpe: Type)   = gen.mkIsInstanceOf(target, tpe, false)

      // XXX having some difficulty expressing nullSafe in a way that doesn't freak out value types
      // def TOSTRING()          = nullSafe(fn(_: Tree, nme.toString_), LIT("null"))(target)
      def TOSTRING()          = fn(target, nme.toString_)
      def GETCLASS()          = fn(target, Object_getClass)
    }

    case class SelectStart(tree: Select) {
      def apply(args: Tree*) = Apply(tree, args.toList)
    }

    class CaseStart(pat: Tree, guard: Tree) {
      def IF(g: Tree): CaseStart    = new CaseStart(pat, g)
      def ==>(body: Tree): CaseDef  = CaseDef(pat, guard, body)
    }

    /** VODD, if it's not obvious, means ValOrDefDef.  This is the
     *  common code between a tree based on a pre-existing symbol and
     *  one being built from scratch.
     */
    trait VODDStart {
      def name: Name
      def defaultMods: Modifiers
      def defaultTpt: Tree
      def defaultPos: Position

      type ResultTreeType <: ValOrDefDef
      def mkTree(rhs: Tree): ResultTreeType
      def ===(rhs: Tree): ResultTreeType

      private var _mods: Modifiers = null
      private var _tpt: Tree = null
      private var _pos: Position = null

      def withType(tp: Type): this.type = {
        _tpt = TypeTree(tp)
        this
      }
      def withFlags(flags: Long*): this.type = {
        if (_mods == null)
          _mods = defaultMods

        _mods = flags.foldLeft(_mods)(_ | _)
        this
      }
      def withPos(pos: Position): this.type = {
        _pos = pos
        this
      }

      final def mods = if (_mods == null) defaultMods else _mods
      final def tpt  = if (_tpt == null) defaultTpt else _tpt
      final def pos  = if (_pos == null) defaultPos else _pos
    }
    trait SymVODDStart extends VODDStart {
      def sym: Symbol
      def symType: Type

      def name        = sym.name
      def defaultMods = Modifiers(sym.flags)
      def defaultTpt  = TypeTree(symType) setPos sym.pos.focus
      def defaultPos  = sym.pos

      final def ===(rhs: Tree): ResultTreeType =
        atPos(pos)(mkTree(rhs) setSymbol sym)
    }
    trait ValCreator {
      self: VODDStart =>

      type ResultTreeType = ValDef
      def mkTree(rhs: Tree): ValDef = ValDef(mods, name, tpt, rhs)
    }
    trait DefCreator {
      self: VODDStart =>

      def tparams: List[TypeDef]
      def vparamss: List[List[ValDef]]

      type ResultTreeType = DefDef
      def mkTree(rhs: Tree): DefDef = DefDef(mods, name, tparams, vparamss, tpt, rhs)
    }

    class DefSymStart(val sym: Symbol) extends SymVODDStart with DefCreator {
      def symType  = sym.tpe.finalResultType
      def tparams  = sym.typeParams map TypeDef
      def vparamss = mapParamss(sym)(ValDef)
    }
    class ValSymStart(val sym: Symbol) extends SymVODDStart with ValCreator {
      def symType = sym.tpe
    }

    trait TreeVODDStart extends VODDStart {
      def defaultMods = NoMods
      def defaultTpt  = TypeTree()
      def defaultPos  = NoPosition

      final def ===(rhs: Tree): ResultTreeType =
        if (pos == NoPosition) mkTree(rhs)
        else atPos(pos)(mkTree(rhs))
    }

    class ValTreeStart(val name: Name) extends TreeVODDStart with ValCreator {
    }
    class DefTreeStart(val name: Name) extends TreeVODDStart with DefCreator {
      def tparams: List[TypeDef] = Nil
      def vparamss: List[List[ValDef]] = List(Nil)
    }

    class IfStart(cond: Tree, thenp: Tree) {
      def THEN(x: Tree)     = new IfStart(cond, x)
      def ELSE(elsep: Tree) = If(cond, thenp, elsep)
      def ENDIF             = If(cond, thenp, EmptyTree)
    }
    class TryStart(body: Tree, catches: List[CaseDef], fin: Tree) {
      def CATCH(xs: CaseDef*) = new TryStart(body, xs.toList, fin)
      def FINALLY(x: Tree)    = Try(body, catches, x)
      def ENDTRY              = Try(body, catches, fin)
    }

    def CASE(pat: Tree): CaseStart  = new CaseStart(pat, EmptyTree)
    def DEFAULT: CaseStart          = new CaseStart(WILD.empty, EmptyTree)

    class SymbolMethods(target: Symbol) {
      def BIND(body: Tree) = Bind(target, body)
      def IS_NULL()  = REF(target) OBJ_EQ NULL
      def NOT_NULL() = REF(target) OBJ_NE NULL

      def GET() = fn(REF(target), nme.get)

      // name of nth indexed argument to a method (first parameter list), defaults to 1st
      def ARG(idx: Int = 0) = Ident(target.paramss.head(idx))
      def ARGS = target.paramss.head
      def ARGNAMES = ARGS map Ident
    }

    /** Top level accessible. */
    def MATCHERROR(arg: Tree) = Throw(MatchErrorClass.tpe, arg)
    def THROW(sym: Symbol, msg: Tree): Throw = Throw(sym.tpe, msg.TOSTRING())

    def NEW(tpt: Tree, args: Tree*): Tree   = New(tpt, List(args.toList))
    def NEW(sym: Symbol, args: Tree*): Tree = New(sym.tpe, args: _*)

    def DEF(name: Name, tp: Type): DefTreeStart     = DEF(name) withType tp
    def DEF(name: Name): DefTreeStart               = new DefTreeStart(name)
    def DEF(sym: Symbol): DefSymStart               = new DefSymStart(sym)

    def VAL(name: Name, tp: Type): ValTreeStart     = VAL(name) withType tp
    def VAL(name: Name): ValTreeStart               = new ValTreeStart(name)
    def VAL(sym: Symbol): ValSymStart               = new ValSymStart(sym)

    def VAR(name: Name, tp: Type): ValTreeStart     = VAL(name, tp) withFlags Flags.MUTABLE
    def VAR(name: Name): ValTreeStart               = VAL(name) withFlags Flags.MUTABLE
    def VAR(sym: Symbol): ValSymStart               = VAL(sym) withFlags Flags.MUTABLE

    def LAZYVAL(name: Name, tp: Type): ValTreeStart = VAL(name, tp) withFlags Flags.LAZY
    def LAZYVAL(name: Name): ValTreeStart           = VAL(name) withFlags Flags.LAZY
    def LAZYVAL(sym: Symbol): ValSymStart           = VAL(sym) withFlags Flags.LAZY

    def AND(guards: Tree*) =
      if (guards.isEmpty) EmptyTree
      else guards reduceLeft gen.mkAnd

    def OR(guards: Tree*) =
      if (guards.isEmpty) EmptyTree
      else guards reduceLeft gen.mkOr

    def IF(tree: Tree)    = new IfStart(tree, EmptyTree)
    def TRY(tree: Tree)   = new TryStart(tree, Nil, EmptyTree)
    def BLOCK(xs: Tree*)  = Block(xs.init.toList, xs.last)
    def NOT(tree: Tree)   = Select(tree, Boolean_not)
    def SOME(xs: Tree*)   = Apply(SomeClass.companionSymbol, makeTupleTerm(xs.toList, true))

    /** Typed trees from symbols. */
    def THIS(sym: Symbol)             = gen.mkAttributedThis(sym)
    def ID(sym: Symbol)               = gen.mkAttributedIdent(sym)
    def REF(sym: Symbol)              = gen.mkAttributedRef(sym)
    def REF(pre: Type, sym: Symbol)   = gen.mkAttributedRef(pre, sym)

    def makeTupleTerm(trees: List[Tree], flattenUnary: Boolean): Tree = trees match {
      case Nil                        => UNIT
      case List(tree) if flattenUnary => tree
      case _                          => Apply(TupleClass(trees.length).companionModule, trees: _*)
    }
    def makeTupleType(trees: List[Tree], flattenUnary: Boolean): Tree = trees match {
      case Nil                        => gen.scalaUnitConstr
      case List(tree) if flattenUnary => tree
      case _                          => AppliedTypeTree(REF(TupleClass(trees.length)), trees)
    }

    /** Implicits - some of these should probably disappear **/
    implicit def mkTreeMethods(target: Tree): TreeMethods = new TreeMethods(target)
    implicit def mkTreeMethodsFromSymbol(target: Symbol): TreeMethods = new TreeMethods(Ident(target))
    implicit def mkSymbolMethodsFromSymbol(target: Symbol): SymbolMethods = new SymbolMethods(target)

    /** (foo DOT bar) might be simply a Select, but more likely it is to be immediately
     *  followed by an Apply.  We don't want to add an actual apply method to arbitrary
     *  trees, so SelectStart is created with an apply - and if apply is not the next
     *  thing called, the implicit from SelectStart -> Tree will provide the tree.
     */
    implicit def mkTreeFromSelectStart(ss: SelectStart): Select = ss.tree
    implicit def mkTreeMethodsFromSelectStart(ss: SelectStart): TreeMethods = mkTreeMethods(ss.tree)
  }
}
