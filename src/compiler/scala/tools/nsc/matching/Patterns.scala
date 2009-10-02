/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * Author: Paul Phillips
 */

package scala.tools.nsc
package matching

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

  // A fresh, empty pattern
  def NoPattern = WildcardPattern()

  // 8.1.1
  case class VariablePattern(tree: Ident) extends Pattern {
    override def irrefutableFor(tpe: Type) = true
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
  }

  // 8.1.3
  case class LiteralPattern(tree: Literal) extends Pattern { }

  // 8.1.4
  case class StableIdPattern(tree: Ident) extends Pattern { }

  // 8.1.4 (b)

  // 8.1.5
  case class ConstructorPattern(tree: Apply) extends ApplyPattern {
    // XXX todo
    // override def irrefutableFor(tpe: Type) = false
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

    def uaTyped = Typed(tree, TypeTree(arg.tpe)) setType arg.tpe
  }

  // 8.1.8
  case class SequencePattern(tree: ArrayValue) extends Pattern { }

  // 8.1.8 (b)
  case class SequenceStarPattern(tree: ArrayValue) extends Pattern {
    // def removeStar(xs: List[Pattern]): List[Pattern] =
    //   xs.init ::: List(Pattern(makeBind(xs.last.boundVariables, WILD(scrut.seqType))))
  }

  // 8.1.9
  // InfixPattern ... subsumed by Constructor/Extractor Patterns

  // 8.1.10
  case class AlternativePattern(tree: Alternative) extends Pattern {
    private val Alternative(subtrees) = tree
    lazy val subpatterns = subtrees map Pattern.apply
  }

  // 8.1.11
  // XMLPattern ... for now, subsumed by SequencePattern, but if we want
  //   to make it work right, it probably needs special handling.


  // XXX - temporary pattern until we have integrated every tree type.
  case class MiscPattern(tree: Tree) extends Pattern {
    // println("Resorted to MiscPattern: %s/%s".format(tree, tree.getClass))
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
      case x: UnApply           => ExtractorPattern(x)
      case x: Ident             => if (isVarPattern(x)) VariablePattern(x) else StableIdPattern(x)
      case x: ArrayValue        => if (isRightIgnoring(x)) SequenceStarPattern(x) else SequencePattern(x)
      case x: Select            => MiscPattern(x) // XXX
      case x: Star              => MiscPattern(x) // XXX
      case _                    => abort("Unknown Tree reached pattern matcher: %s/%s".format(tree, tree.getClass))
    }
    def unapply(other: Pattern): Option[Tree] = Some(other.tree)
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
      else if (args.isEmpty) fn match {
        case _  => ConstructorPattern(x)  // XXX
        // case x: Ident => StableIdPattern(x)
        // case x        => MiscPattern(x)
      }
      else abort("Strange apply: %s/%s".format(x))
    }
  }

  sealed abstract class ApplyPattern extends Pattern {
    protected lazy val Apply(fn, args) = tree

    def isConstructorPattern = fn.isType
  }

  sealed abstract class Pattern {
    val tree: Tree

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

    /** returns true if pattern tests an object */
    final def isObjectTest(head: Type) =
      isSymValid && prefix.isStable && (head =:= mkSingleton)

    /** Helpers **/
    private def strip(t: Tree): List[Symbol] = t match {
      case b @ Bind(_, pat) => b.symbol :: strip(pat)
      case _                => Nil
    }

    /** Standard methods **/
    def copy(tree: Tree = this.tree): Pattern =
      if (_boundTree == null) Pattern(tree)
      else Pattern(tree) withBoundTree _boundTree

    override def toString() = "Pattern(%s, %s)".format(tree, boundVariables)
    override def equals(other: Any) = other match {
      case x: Pattern => this.boundTree == x.boundTree
      case _          => super.equals(other)
    }
    override def hashCode() = boundTree.hashCode()
  }
}