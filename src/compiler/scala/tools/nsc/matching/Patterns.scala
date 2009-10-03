/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * Author: Paul Phillips
 */

package scala.tools.nsc
package matching

import symtab.Flags

/**
 * Simple pattern types:
 *
 * 1 Variable               x
 * 3 Literal                56
 *
 * Types which must be decomposed into conditionals and simple types:
 *
 * 2 Typed                  x: Int
 * 4 Stable Identifier      Bob or `x`
 * 5 Constructor            Symbol("abc")
 * 6 Tuple                  (5, 5)
 * 7 Extractor              List(1, 2)
 * 8 Sequence               List(1, 2, _*)
 * 9 Infix                  5 :: xs
 * 10 Alternative           "foo" | "bar"
 * 11 XML                   --
 * 12 Regular Expression    --
 */

trait Patterns extends ast.TreeDSL {
  self: transform.ExplicitOuter =>

  import global.{ typer => _, _ }
  import definitions._
  import CODE._
  import treeInfo.{ unbind, isVarPattern }

  // Fresh patterns
  final def emptyPatterns(i: Int): List[Pattern] = List.fill(i)(NoPattern)

  // An empty pattern
  def NoPattern = WildcardPattern()

  // The constant null pattern
  def NullPattern = Pattern(NULL)

  // 8.1.1
  case class VariablePattern(tree: Ident) extends Pattern {
    override def irrefutableFor(tpe: Type) = true
    override def simplify(testVar: Symbol) = this.rebindToEqualsCheck()
  }

  // 8.1.1 (b)
  case class WildcardPattern() extends Pattern {
    val tree = EmptyTree
    override def irrefutableFor(tpe: Type) = true
  }

  // 8.1.2
  case class TypedPattern(tree: Typed) extends Pattern {
    private val Typed(expr, tpt) = tree

    override def irrefutableFor(tpe: Type) = tpe <:< tree.tpe
    override def simplify(testVar: Symbol) = Pattern(expr) match {
      case ExtractorPattern(ua) if testVar.tpe <:< tpt.tpe  => this rebindTo expr
      case _                                                => this
    }
  }

  // 8.1.3
  case class LiteralPattern(tree: Literal) extends Pattern { }

  // 8.1.4
  case class StableIdPattern(tree: Ident) extends Pattern {
    override def simplify(testVar: Symbol) = this.rebindToEqualsCheck()
  }

  // 8.1.4 (b)
  case class SelectPattern(tree: Select) extends Pattern {
    override def simplify(testVar: Symbol) = this.rebindToEqualsCheck()
  }

  // 8.1.5
  case class ConstructorPattern(tree: Apply) extends ApplyPattern {
    require(fn.isType && this.isCaseClass)

    override def subpatterns(pats: MatchMatrix#Patterns) =
      if (pats.isCaseHead) args map Pattern.apply
      else super.subpatterns(pats)

    override def simplify(testVar: Symbol) =
      if (args.isEmpty) this rebindToEmpty tree.tpe
      else this

    // XXX todo
    // override def irrefutableFor(tpe: Type) = false
  }
  // XXX temp
  case class ApplyValuePattern(tree: Apply) extends ApplyPattern {
    require(!fn.isType)

    override def simplify(testVar: Symbol) = {
      def examinePrefix(path: Tree) = (path, path.tpe) match {
        case (_, t: ThisType)     => singleType(t, sym)
        case (_: Apply, _)        => PseudoType(tree)
        case _                    => singleType(Pattern(path).mkSingleton, sym)
      }
      val singletonType =
        if (isModule) mkSingleton else fn match {
          case Select(path, _)  => examinePrefix(path)
          case x: Ident         => Pattern(x).equalsCheck
        }

      val typeToTest = mkEqualsRef(singletonType)
      val tt = Typed(WILD(typeToTest), TypeTree(singletonType)) setType typeToTest
      this rebindTo tt
    }
  }

  // 8.1.6
  case class TuplePattern(tree: Apply) extends ApplyPattern {
    // XXX todo
    // override def irrefutableFor(tpe: Type) = false
  }

  // 8.1.7
  case class ExtractorPattern(tree: UnApply) extends Pattern {
    private val UnApply(Apply(fn, _), args) = tree
    private val MethodType(List(arg, _*), _) = fn.tpe
    private def uaTyped = Typed(tree, TypeTree(arg.tpe)) setType arg.tpe

    // can fix #1697 here?
    override def simplify(testVar: Symbol) =
      if (testVar.tpe <:< arg.tpe) this
      else this rebindTo uaTyped
  }

  // 8.1.8 (unapplySeq calls)
  case class SequenceExtractorPattern(tree: UnApply) extends Pattern {
    private val UnApply(
      Apply(TypeApply(Select(_, nme.unapplySeq), List(tptArg)), _),
      List(ArrayValue(_, elems))
    ) = tree

    // @pre: is not right-ignoring (no star pattern) ; no exhaustivity check
    override def simplify(testVar: Symbol) = {
      testVar setFlag Flags.TRANS_FLAG
      this rebindTo normalizedListPattern(elems, tptArg.tpe)
    }
  }

  // 8.1.8 (b) (literal ArrayValues)
  case class SequencePattern(tree: ArrayValue) extends Pattern { }

  // // 8.1.8 (b)
  case class SequenceStarPattern(tree: ArrayValue) extends Pattern { }

  // 8.1.9
  // InfixPattern ... subsumed by Constructor/Extractor Patterns

  // 8.1.10
  case class AlternativePattern(tree: Alternative) extends Pattern {
    private val Alternative(subtrees) = tree
    // override def subpatterns(pats: MatchMatrix#Patterns) = subtrees map Pattern.apply
  }

  // 8.1.11
  // XMLPattern ... for now, subsumed by SequencePattern, but if we want
  //   to make it work right, it probably needs special handling.

  // XXX - temporary pattern until we have integrated every tree type.
  case class MiscPattern(tree: Tree) extends Pattern {
    log("Resorted to MiscPattern: %s/%s".format(tree, tree.getClass))
    override def simplify(testVar: Symbol) = tree match {
      case x: Ident => this.rebindToEqualsCheck()
      case _        => super.simplify(testVar)
    }
  }

  object Pattern {
    def isDefaultPattern(t: Tree)   = cond(unbind(t)) { case EmptyTree | WILD() => true }
    def isStar(t: Tree)             = cond(unbind(t)) { case Star(q) => isDefaultPattern(q) }
    def isRightIgnoring(t: Tree)    = cond(unbind(t)) { case ArrayValue(_, xs) if !xs.isEmpty => isStar(xs.last) }

    def apply(tree: Tree): Pattern = tree match {
      case x: Bind              => apply(unbind(tree)) withBoundTree x
      case EmptyTree | WILD()   => WildcardPattern()
      case x @ Alternative(ps)  => AlternativePattern(x)
      case x: Apply             => ApplyPattern(x)
      case x: Typed             => TypedPattern(x)
      case x: Literal           => LiteralPattern(x)
      case x: UnApply           => UnapplyPattern(x)
      case x: Ident             => if (isVarPattern(x)) VariablePattern(x) else StableIdPattern(x)
      case x: ArrayValue        => if (isRightIgnoring(x)) SequenceStarPattern(x) else SequencePattern(x)
      case x: Select            => SelectPattern(x)
      case x: Star              => MiscPattern(x) // XXX
      case _                    => abort("Unknown Tree reached pattern matcher: %s/%s".format(tree, tree.getClass))
    }
    def unapply(other: Any): Option[(Tree, List[Symbol])] = other match {
      case x: Tree    => unapply(Pattern(x))
      case x: Pattern => Some((x.tree, x.boundVariables))
      case _          => None
    }
  }

  object UnapplyPattern {
    private object UnapplySeq {
      private object TypeApp {
        def unapply(x: Any) = condOpt(x) {
          case TypeApply(sel @ Select(stor, nme.unapplySeq), List(tpe)) if stor.symbol eq ListModule => tpe
        }
      }
      def unapply(x: UnApply) = condOpt(x) {
        case UnApply(Apply(TypeApp(tptArg), _), List(ArrayValue(_, xs))) => (tptArg, xs)
      }
    }

    def apply(x: UnApply): Pattern = {
      x match {
        case UnapplySeq(_, _) => SequenceExtractorPattern(x)
        case _                => ExtractorPattern(x)
      }
    }
  }

  // right now a tree like x @ Apply(fn, Nil) where !fn.isType
  // is handled by creating a singleton type:
  //
  //    val stype = Types.singleType(x.tpe.prefix, x.symbol)
  //
  // and then passing that as a type argument to EqualsPatternClass:
  //
  //    val tpe = typeRef(NoPrefix, EqualsPatternClass, List(stype))
  //
  // then creating a Typed pattern and rebinding.
  //
  //    val newpat = Typed(EmptyTree, TypeTree(tpe)) setType tpe)
  //
  // This is also how Select(qual, name) is handled.
  object ApplyPattern {
    def apply(x: Apply): Pattern = {
      val Apply(fn, args) = x

      if (fn.isType) {
        if (isTupleType(fn.tpe)) TuplePattern(x)
        else ConstructorPattern(x)
      }
      else if (args.isEmpty) ApplyValuePattern(x)
      else abort("Strange apply: %s/%s".format(x))
    }
  }

  sealed abstract class ApplyPattern extends Pattern {
    protected lazy val Apply(fn, args) = tree
    def isConstructorPattern = fn.isType
  }
  // trait SimplePattern extends Pattern {
  //   def simplify(testVar: Symbol): Pattern = this
  // }
  sealed abstract class Pattern {
    val tree: Tree
    // The logic formerly in classifyPat, returns either a simplification
    // of this pattern or identity.
    def simplify(testVar: Symbol): Pattern = this
    def simplify(): Pattern = this simplify NoSymbol

    def subpatterns(pats: MatchMatrix#Patterns): List[Pattern] = pats.dummyPatterns

    // 8.1.13
    // A pattern p is irrefutable for type T if any of the following applies:
    //   1) p is a variable pattern
    //   2) p is a typed pattern x: T', and T <: T'
    //   3) p is a constructor pattern C(p1,...,pn), the type T is an instance of class C,
    //      the primary constructor of type T has argument types T1,...,Tn and and each
    //      pi is irrefutable for Ti.
    def irrefutableFor(tpe: Type) = false

    // XXX only a var for short-term experimentation.
    private var _boundTree: Bind = null
    def boundTree = if (_boundTree == null) tree else _boundTree
    def withBoundTree(x: Bind): this.type = {
      _boundTree = x
      this
    }
    lazy val boundVariables = strip(boundTree)

    private def wrapBindings(vs: List[Symbol], pat: Tree): Tree = vs match {
      case Nil      => pat
      case x :: xs  => Bind(x, wrapBindings(xs, pat)) setType pat.tpe
    }

    // If a tree has bindings, boundTree looks something like
    //   Bind(v3, Bind(v2, Bind(v1, tree)))
    // This takes the given tree and creates a new pattern
    //   using the same bindings.
    def rebindTo(t: Tree): Pattern =
      Pattern(wrapBindings(boundVariables, t))

    // Wrap this pattern's bindings around (_: Type)
    def rebindToType(tpe: Type): Pattern =
      rebindTo(Typed(WILD(tpe), TypeTree(tpe)) setType tpe)

    // Wrap them around _
    def rebindToEmpty(tpe: Type): Pattern =
      rebindTo(Typed(EmptyTree, TypeTree(tpe)) setType tpe)

    // Wrap them around a singleton type for an EqualsPattern check.
    def rebindToEqualsCheck(): Pattern =
      rebindToType(equalsCheck)

    def    sym  = tree.symbol
    def    tpe  = tree.tpe
    def prefix  = tpe.prefix
    def isEmpty = tree.isEmpty

    def isSymValid = (sym != null) && (sym != NoSymbol)
    def isModule = sym.isModule || tpe.termSymbol.isModule
    def isCaseClass = tpe.typeSymbol hasFlag Flags.CASE
    def isObject = isSymValid && prefix.isStable

    def setType(tpe: Type): this.type = {
      tree setType tpe
      this
    }

    def equalsCheck =
      if (sym.isValue) singleType(NoPrefix, sym)
      else mkSingleton

    def mkSingleton = tpe match {
      case st: SingleType => st
      case _              => singleType(prefix, sym)
    }

    final def isDefault           = cond(tree) { case EmptyTree | WILD() => true }
    final def isStar              = cond(tree) { case Star(q) => Pattern(q).isDefault }
    final def isAlternative       = cond(tree) { case Alternative(_) => true }
    final def isRightIgnoring     = cond(tree) { case ArrayValue(_, xs) if !xs.isEmpty => Pattern(xs.last).isStar }

    /** Helpers **/
    private def strip(t: Tree): List[Symbol] = t match {
      case b @ Bind(_, pat) => b.symbol :: strip(pat)
      case _                => Nil
    }

    /** Standard methods **/
    def copy(tree: Tree = this.tree): Pattern =
      if (_boundTree == null) Pattern(tree)
      else Pattern(tree) withBoundTree _boundTree

    // override def toString() = "Pattern(%s, %s)".format(tree, boundVariables)
    override def equals(other: Any) = other match {
      case x: Pattern => this.boundTree == x.boundTree
      case _          => super.equals(other)
    }
    override def hashCode() = boundTree.hashCode()
  }
}