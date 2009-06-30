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
    object LIT extends (Any => Literal) {
      def apply(x: Any)   = Literal(Constant(x))
      def unapply(x: Any) = x match {
        case Literal(Constant(value)) => Some(value)
        case _                        => None
      }
    }

    def ID(sym: Symbol) = Ident(sym) setType sym.tpe

    // You might think these could be vals, but empirically I have found that
    // at least in the case of UNIT the compiler breaks if you re-use trees.
    // However we need stable identifiers to have attractive pattern matching.
    // So it's inconsistent until I devise a better way.
    val TRUE          = LIT(true)
    val FALSE         = LIT(false)
    def NULL          = LIT(null)
    def UNIT          = LIT(())
    val ZERO          = LIT(0)

    object WILD {
      def apply(tpe: Type = null) =
        if (tpe == null) Ident(nme.WILDCARD)
        else Ident(nme.WILDCARD) setType tpe

      def unapply(other: Any) = other match {
        case Ident(nme.WILDCARD)  => true
        case _                    => false
      }
    }

    def fn(lhs: Tree, op:   Name, args: Tree*)  = Apply(Select(lhs, op), args.toList)
    def fn(lhs: Tree, op: Symbol, args: Tree*)  = Apply(Select(lhs, op), args.toList)

    class TreeMethods(target: Tree) {
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

      def ANY_NE  (other: Tree)     = fn(target, nme.ne, toAnyRef(other))
      def ANY_EQ  (other: Tree)     = fn(target, nme.eq, toAnyRef(other))
      def ANY_==  (other: Tree)     = fn(target, Any_==, other)
      def ANY_>=  (other: Tree)     = fn(target, nme.GE, other)
      def OBJ_!=  (other: Tree)     = fn(target, Object_ne, other)

      def INT_|   (other: Tree)     = fn(target, getMember(IntClass, nme.OR), other)
      def INT_&   (other: Tree)     = fn(target, getMember(IntClass, nme.AND), other)
      def INT_==  (other: Tree)     = fn(target, getMember(IntClass, nme.EQ), other)
      def INT_!=  (other: Tree)     = fn(target, getMember(IntClass, nme.NE), other)

      def BOOL_&& (other: Tree)     = fn(target, getMember(BooleanClass, nme.ZAND), other)
      def BOOL_|| (other: Tree)     = fn(target, getMember(BooleanClass, nme.ZOR), other)

      /** Apply, Select, Match **/
      def APPLY(params: Tree*)      = Apply(target, params.toList)
      def APPLY(params: List[Tree]) = Apply(target, params)
      def MATCH(cases: CaseDef*)    = Match(target, cases.toList)
      def DOT(member: Name)         = SelectStart(Select(target, member))
      def DOT(sym: Symbol)          = SelectStart(Select(target, sym))

      /** Assignment */
      def ===(rhs: Tree)            = Assign(target, rhs)

      /** for tree of sequence type, returns tree that drops first i elements */
      def DROP(count: Int): Tree =
        if (count == 0) target
        else (target DOT nme.drop)(LIT(count)) DOT nme.toSeq

      /** Casting & type tests -- working our way toward understanding exactly
       *  what differs between the different forms of IS and AS.
       */
      def AS(tpe: Type)       = TypeApply(Select(target, Any_asInstanceOf), List(TypeTree(tpe)))
      def AS_ANY(tpe: Type)   = gen.mkAsInstanceOf(target, tpe)
      def AS_ATTR(tpe: Type)  = gen.mkAttributedCast(target, tpe)

      def IS(tpe: Type)       = gen.mkIsInstanceOf(target, tpe, true)
      def IS_OBJ(tpe: Type)   = gen.mkIsInstanceOf(target, tpe, false)

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
    def DEFAULT: CaseStart          = new CaseStart(WILD(), EmptyTree)

    class NameMethods(target: Name) {
      def BIND(body: Tree) = Bind(target, body)
    }

    class SymbolMethods(target: Symbol) {
      def BIND(body: Tree) = Bind(target, body)

      // Option
      def IS_DEFINED() =
        if (target.tpe.typeSymbol == SomeClass) TRUE   // is Some[_]
        else NOT(ID(target) DOT nme.isEmpty)           // is Option[_]

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
    def NEW(tpe: Tree, args: Tree*)   = New(tpe, List(args.toList))
    def NEW(sym: Symbol, args: Tree*) =
      if (args.isEmpty) New(TypeTree(sym.tpe))
      else New(TypeTree(sym.tpe), List(args.toList))

    def VAL(sym: Symbol) = new ValStart(sym)
    def DEF(sym: Symbol) = new DefStart(sym)
    def AND(guards: Tree*) =
      if (guards.isEmpty) EmptyTree
      else guards reduceLeft gen.mkAnd

    def IF(tree: Tree)    = new IfStart(tree, EmptyTree)
    def TRY(tree: Tree)   = new TryStart(tree, Nil, EmptyTree)
    def REF(sym: Symbol)  = gen.mkAttributedRef(sym)
    def REF(pre: Type, sym: Symbol) = gen.mkAttributedRef(pre, sym)
    def BLOCK(xs: Tree*)  = Block(xs.init.toList, xs.last)
    def NOT(tree: Tree)   = Select(tree, getMember(BooleanClass, nme.UNARY_!))

    //
    // Unused, from the pattern matcher:
    // def SEQELEM(tpe: Type): Type = (tpe.widen baseType SeqClass) match {
    //   case NoType   => Predef.error("arg " + tpe + " not subtype of Seq[A]")
    //   case t        => t typeArgs 0
    // }

    /** Implicits - some of these should probably disappear **/
    implicit def mkTreeMethods(target: Tree): TreeMethods = new TreeMethods(target)
    implicit def mkTreeMethodsFromSymbol(target: Symbol): TreeMethods = new TreeMethods(Ident(target))
    implicit def mkTreeMethodsFromName(target: Name): TreeMethods = new TreeMethods(Ident(target))
    implicit def mkTreeMethodsFromString(target: String): TreeMethods = new TreeMethods(Ident(target))

    implicit def mkNameMethodsFromName(target: Name): NameMethods = new NameMethods(target)
    implicit def mkNameMethodsFromString(target: String): NameMethods = new NameMethods(target)

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