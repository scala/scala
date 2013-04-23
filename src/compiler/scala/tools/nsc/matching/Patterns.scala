/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * Author: Paul Phillips
 */

package scala.tools.nsc
package matching

import symtab.Flags
import PartialFunction._

/** Patterns are wrappers for Trees with enhanced semantics.
 *
 *  @author Paul Phillips
 */

trait Patterns extends ast.TreeDSL {
  self: transform.ExplicitOuter =>

  import global.{ typer => _, _ }
  import definitions._
  import CODE._
  import Debug._
  import treeInfo.{ unbind, isStar, isVarPattern }

  type PatternMatch       = MatchMatrix#PatternMatch
  private type PatternVar = MatrixContext#PatternVar

  // Fresh patterns
  def emptyPatterns(i: Int): List[Pattern] = List.fill(i)(NoPattern)
  def emptyTrees(i: Int): List[Tree] = List.fill(i)(EmptyTree)

  // An empty pattern
  def NoPattern = WildcardPattern()

  // The constant null pattern
  def NullPattern = LiteralPattern(NULL)

  // The Nil pattern
  def NilPattern = Pattern(gen.mkNil)

  // 8.1.1
  case class VariablePattern(tree: Ident) extends NamePattern {
    lazy val Ident(name) = tree
    require(isVarPattern(tree) && name != nme.WILDCARD)
    override def covers(sym: Symbol) = true
    override def description = "%s".format(name)
  }

  // 8.1.1 (b)
  case class WildcardPattern() extends Pattern {
    def tree = EmptyTree
    override def covers(sym: Symbol) = true
    override def isDefault = true
    override def description = "_"
  }

  // 8.1.2
  case class TypedPattern(tree: Typed) extends Pattern {
    lazy val Typed(expr, tpt) = tree

    override def covers(sym: Symbol) = newMatchesPattern(sym, tpt.tpe)
    override def sufficientType = tpt.tpe
    override def subpatternsForVars: List[Pattern] = List(Pattern(expr))
    override def simplify(pv: PatternVar) = Pattern(expr) match {
      case ExtractorPattern(ua) if pv.sym.tpe <:< tpt.tpe => this rebindTo expr
      case _                                              => this
    }
    override def description = "%s: %s".format(Pattern(expr), tpt)
  }

  // 8.1.3
  case class LiteralPattern(tree: Literal) extends Pattern {
    lazy val Literal(const @ Constant(value)) = tree

    def isSwitchable = cond(const.tag) { case ByteTag | ShortTag | IntTag | CharTag => true }
    def intValue = const.intValue
    override def description = {
      val s = if (value == null) "null" else value.toString
      "Lit(%s)".format(s)
    }
  }

  // 8.1.4 (a)
  case class ApplyIdentPattern(tree: Apply) extends ApplyPattern with NamePattern {
    // XXX - see bug 3411 for code which violates this assumption
    // require (!isVarPattern(fn) && args.isEmpty)
    lazy val ident @ Ident(name) = fn

    override def sufficientType = Pattern(ident).equalsCheck
    override def simplify(pv: PatternVar) = this.rebindToObjectCheck()
    override def description = "Id(%s)".format(name)
  }
  // 8.1.4 (b)
  case class ApplySelectPattern(tree: Apply) extends ApplyPattern with SelectPattern {
    require (args.isEmpty)
    lazy val Apply(select: Select, _) = tree

    override lazy val sufficientType = qualifier.tpe match {
      case t: ThisType  => singleType(t, sym)   // this.X
      case _            =>
        qualifier match {
          case _: Apply => PseudoType(tree)
          case _        => singleType(Pattern(qualifier).necessaryType, sym)
        }
    }

    override def covers(sym: Symbol) = newMatchesPattern(sym, sufficientType)
    override def simplify(pv: PatternVar) = this.rebindToObjectCheck()
    override def description = backticked match {
      case Some(s)  => "this." + s
      case _        => "Sel(%s.%s)".format(Pattern(qualifier), name)
    }

  }
  // 8.1.4 (c)
  case class StableIdPattern(tree: Select) extends SelectPattern {
    def select = tree
    override def description = "St(%s)".format(printableSegments.mkString(" . "))
    private def printableSegments =
      pathSegments filter (x => !x.isEmpty && (x.toString != "$iw"))
  }
  // 8.1.4 (d)
  case class ObjectPattern(tree: Apply) extends ApplyPattern {  // NamePattern?
    require(!fn.isType && isModule)

    override def covers(sym: Symbol) = newMatchesPattern(sym, sufficientType)
    override def sufficientType = tpe.narrow
    override def simplify(pv: PatternVar) = this.rebindToObjectCheck()
    override def description = "Obj(%s)".format(fn)
  }
  // 8.1.4 (e)
  case class SimpleIdPattern(tree: Ident) extends NamePattern {
    val Ident(name) = tree
    override def covers(sym: Symbol) = newMatchesPattern(sym, tpe.narrow)
    override def description = "Id(%s)".format(name)
  }

  // 8.1.5
  case class ConstructorPattern(tree: Apply) extends ApplyPattern with NamePattern {
    require(fn.isType && this.isCaseClass, "tree: " + tree + " fn: " + fn)
    def name = tpe.typeSymbol.name
    def cleanName = tpe.typeSymbol.decodedName
    def hasPrefix = tpe.prefix.prefixString != ""
    def prefixedName =
      if (hasPrefix) "%s.%s".format(tpe.prefix.prefixString, cleanName)
      else cleanName

    private def isColonColon = cleanName == "::"

    override def subpatterns(pm: MatchMatrix#PatternMatch) =
      if (pm.head.isCaseClass) toPats(args)
      else super.subpatterns(pm)

    override def simplify(pv: PatternVar) =
      if (args.isEmpty) this rebindToEmpty tree.tpe
      else this

    override def covers(sym: Symbol) = {
      debugging("[constructor] Does " + this + " cover " + sym + " ? ") {
        sym.tpe.typeSymbol == this.tpe.typeSymbol
      }
    }
    override def description = {
      if (isColonColon) "%s :: %s".format(Pattern(args(0)), Pattern(args(1)))
      else "%s(%s)".format(name, toPats(args).mkString(", "))
    }
  }
  // 8.1.6
  case class TuplePattern(tree: Apply) extends ApplyPattern {
    override def description = "((%s))".format(args.size, toPats(args).mkString(", "))
  }

  // 8.1.7 / 8.1.8 (unapply and unapplySeq calls)
  case class ExtractorPattern(tree: UnApply) extends UnapplyPattern {
    private def uaTyped = Typed(tree, TypeTree(arg.tpe)) setType arg.tpe

    override def simplify(pv: PatternVar) = {
      if (pv.tpe <:< arg.tpe) this
      else this rebindTo uaTyped
    }
    override def description = "Unapply(%s => %s)".format(necessaryType, resTypesString)
  }

  // Special List handling.  It was like that when I got here.
  case class ListExtractorPattern(tree: UnApply, tpt: Tree, elems: List[Tree]) extends UnapplyPattern with SequenceLikePattern {
    // As yet I can't testify this is doing any good relative to using
    // tpt.tpe, but it doesn't seem to hurt either.
    private lazy val packedType = global.typer.computeType(tpt, tpt.tpe)
    private lazy val consRef    = appliedType(ConsClass, packedType)
    private lazy val listRef    = appliedType(ListClass, packedType)
    private lazy val seqRef     = appliedType(SeqClass, packedType)

    private def thisSeqRef = {
      val tc = (tree.tpe baseType SeqClass).typeConstructor
      if (tc.typeParams.size == 1) appliedType(tc, List(packedType))
      else seqRef
    }

    // Fold a list into a well-typed x :: y :: etc :: tree.
    private def listFolder(hd: Tree, tl: Tree): Tree = unbind(hd) match {
      case t @ Star(_) => moveBindings(hd, WILD(t.tpe))
      case _           =>
        val dummyMethod = NoSymbol.newTermSymbol(newTermName("matching$dummy"))
        val consType    = MethodType(dummyMethod newSyntheticValueParams List(packedType, listRef), consRef)

        Apply(TypeTree(consType), List(hd, tl)) setType consRef
    }
    private def foldedPatterns = elems.foldRight(gen.mkNil)((x, y) => listFolder(x, y))
    override def necessaryType = if (nonStarPatterns.nonEmpty) consRef else listRef

    override def simplify(pv: PatternVar) = {
      if (pv.tpe <:< necessaryType)
        Pattern(foldedPatterns)
      else
        this rebindTo (Typed(tree, TypeTree(necessaryType)) setType necessaryType)
    }
    override def description = "List(%s => %s)".format(packedType, resTypesString)
  }

  trait SequenceLikePattern extends Pattern {
    def elems: List[Tree]
    override def hasStar = elems.nonEmpty && isStar(elems.last)

    def elemPatterns    = toPats(elems)
    def nonStarElems    = if (hasStar) elems.init else elems
    def nonStarPatterns = toPats(nonStarElems)
    def nonStarLength   = nonStarElems.length
  }

  // 8.1.8 (b) (literal ArrayValues)
  case class SequencePattern(tree: ArrayValue) extends Pattern with SequenceLikePattern {
    lazy val ArrayValue(elemtpt, elems) = tree

    override def subpatternsForVars: List[Pattern] = elemPatterns
    override def description = "Seq(%s)".format(elemPatterns mkString ", ")
  }

  // 8.1.8 (c)
  case class StarPattern(tree: Star) extends Pattern {
    lazy val Star(elem) = tree
    override def description = "_*"
  }
  // XXX temporary?
  case class ThisPattern(tree: This) extends NamePattern {
    lazy val This(name) = tree
    override def description = "this"
  }

  // 8.1.9
  // InfixPattern ... subsumed by Constructor/Extractor Patterns

  // 8.1.10
  case class AlternativePattern(tree: Alternative) extends Pattern {
    private lazy val Alternative(subtrees) = tree
    private def alts = toPats(subtrees)
    override def description = "Alt(%s)".format(alts mkString " | ")
  }

  // 8.1.11
  // XMLPattern ... for now, subsumed by SequencePattern, but if we want
  //   to make it work right, it probably needs special handling.

  private def abortUnknownTree(tree: Tree) =
    abort("Unknown Tree reached pattern matcher: %s/%s".format(tree, tree.getClass))

  object Pattern {
    // a small tree -> pattern cache
    private val cache = perRunCaches.newMap[Tree, Pattern]()

    def apply(tree: Tree): Pattern = {
      if (cache contains tree)
        return cache(tree)

      val p = tree match {
        case x: Bind              => apply(unbind(tree)) setBound x
        case EmptyTree            => WildcardPattern()
        case Ident(nme.WILDCARD)  => WildcardPattern()
        case x @ Alternative(ps)  => AlternativePattern(x)
        case x: Apply             => ApplyPattern(x)
        case x: Typed             => TypedPattern(x)
        case x: Literal           => LiteralPattern(x)
        case x: UnApply           => UnapplyPattern(x)
        case x: Ident             => if (isVarPattern(x)) VariablePattern(x) else SimpleIdPattern(x)
        case x: ArrayValue        => SequencePattern(x)
        case x: Select            => StableIdPattern(x)
        case x: Star              => StarPattern(x)
        case x: This              => ThisPattern(x) // XXX ?
        case _                    => abortUnknownTree(tree)
      }
      cache(tree) = p

      // limiting the trace output
      p match {
        case WildcardPattern()  => p
        case _: LiteralPattern  => p
        case _                  => tracing("Pattern")(p)
      }
    }
    // matching on Pattern(...) always skips the bindings.
    def unapply(other: Any): Option[Tree] = other match {
      case x: Tree    => unapply(Pattern(x))
      case x: Pattern => Some(x.tree)
      case _          => None
    }
  }

  object UnapplyPattern {
    private object UnapplySeq {
      def unapply(x: UnApply) = x match {
        case UnApply(
        Apply(TypeApply(Select(qual, nme.unapplySeq), List(tpt)), _),
        List(ArrayValue(_, elems))) =>
          Some((qual.symbol, tpt, elems))
        case _ =>
          None
       }
    }

    def apply(x: UnApply): Pattern = x match {
      case UnapplySeq(ListModule, tpt, elems) =>
        ListExtractorPattern(x, tpt, elems)
      case _ =>
        ExtractorPattern(x)
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
      def isModule  = x.symbol.isModule || x.tpe.termSymbol.isModule

      if (fn.isType) {
        if (isTupleType(fn.tpe)) TuplePattern(x)
        else ConstructorPattern(x)
      }
      else if (args.isEmpty) {
        if (isModule) ObjectPattern(x)
        else fn match {
          case _: Ident   => ApplyIdentPattern(x)
          case _: Select  => ApplySelectPattern(x)
        }
      }
      else abortUnknownTree(x)
    }
  }

  /** Some intermediate pattern classes with shared structure **/

  sealed trait SelectPattern extends NamePattern {
    def select: Select
    lazy val Select(qualifier, name) = select
    def pathSegments = getPathSegments(tree)
    def backticked: Option[String] = qualifier match {
      case _: This if nme.isVariableName(name)  => Some("`%s`".format(name))
      case _                                => None
    }
    override def covers(sym: Symbol) = newMatchesPattern(sym, tree.tpe)
    protected def getPathSegments(t: Tree): List[Name] = t match {
      case Select(q, name)  => name :: getPathSegments(q)
      case Apply(f, Nil)    => getPathSegments(f)
      case _                => Nil
    }
  }

  sealed trait NamePattern extends Pattern {
    def name: Name
    override def sufficientType = tpe.narrow
    override def simplify(pv: PatternVar) = this.rebindToEqualsCheck()
    override def description = name.toString
  }

  sealed trait UnapplyPattern extends Pattern {
    lazy val UnApply(unfn, args) = tree
    lazy val Apply(fn, _) = unfn
    lazy val MethodType(List(arg, _*), _) = fn.tpe

    // Covers if the symbol matches the unapply method's argument type,
    // and the return type of the unapply is Some.
    override def covers(sym: Symbol) = newMatchesPattern(sym, arg.tpe)

    // TODO: for alwaysCovers:
    //   fn.tpe.finalResultType.typeSymbol == SomeClass

    override def necessaryType = arg.tpe
    override def subpatternsForVars = args match {
      case List(ArrayValue(elemtpe, elems)) => toPats(elems)
      case _                                => toPats(args)
    }

    def resTypes = analyzer.unapplyTypeList(unfn.pos, unfn.symbol, unfn.tpe, args)
    def resTypesString = resTypes match {
      case Nil  => "Boolean"
      case xs   => xs.mkString(", ")
    }
  }

  sealed trait ApplyPattern extends Pattern {
    lazy val Apply(fn, args) = tree
    override def subpatternsForVars: List[Pattern] = toPats(args)

    override def dummies =
      if (!this.isCaseClass) Nil
      else emptyPatterns(sufficientType.typeSymbol.caseFieldAccessors.size)

    def isConstructorPattern = fn.isType
    override def covers(sym: Symbol) = newMatchesPattern(sym, fn.tpe)
  }

  sealed abstract class Pattern extends PatternBindingLogic {
    def tree: Tree

    // returns either a simplification of this pattern or identity.
    def simplify(pv: PatternVar): Pattern = this

    // the right number of dummies for this pattern
    def dummies: List[Pattern] = Nil

    // Is this a default pattern (untyped "_" or an EmptyTree inserted by the matcher)
    def isDefault = false

    // what type must a scrutinee have to have any chance of matching this pattern?
    def necessaryType = tpe

    // what type could a scrutinee have which would automatically indicate a match?
    // (nullness and guards will still be checked.)
    def sufficientType = tpe

    // the subpatterns for this pattern (at the moment, that means constructor arguments)
    def subpatterns(pm: MatchMatrix#PatternMatch): List[Pattern] = pm.dummies

    // if this pattern should be considered to cover the given symbol
    def covers(sym: Symbol): Boolean = newMatchesPattern(sym, sufficientType)
    def newMatchesPattern(sym: Symbol, pattp: Type) = {
      debugging("[" + kindString + "] Does " + pattp + " cover " + sym + " ? ") {
        (sym.isModuleClass && (sym.tpe.typeSymbol eq pattp.typeSymbol)) ||
        (sym.tpe.baseTypeSeq exists (_ matchesPattern pattp))
      }
    }

    def    sym  = tree.symbol
    def    tpe  = tree.tpe
    def isEmpty = tree.isEmpty

    def isModule    = sym.isModule || tpe.termSymbol.isModule
    def isCaseClass = tpe.typeSymbol.isCase
    def isObject    = (sym != null) && (sym != NoSymbol) && tpe.prefix.isStable  // XXX not entire logic

    def hasStar = false

    def setType(tpe: Type): this.type = {
      tree setType tpe
      this
    }

    def equalsCheck =
      tracing("equalsCheck")(
        if (sym.isValue) singleType(NoPrefix, sym)
        else tpe.narrow
      )

    /** Standard methods **/
    override def equals(other: Any) = other match {
      case x: Pattern => this.boundTree == x.boundTree
      case _          => super.equals(other)
    }
    override def hashCode() = boundTree.hashCode()
    def description = super.toString

    final override def toString = description

    def toTypeString() = "%s <: x <: %s".format(necessaryType, sufficientType)
    def kindString = ""
  }

  /*** Extractors ***/

  object UnapplyParamType {
    def unapply(x: Tree): Option[Type] = condOpt(unbind(x)) {
      case UnApply(Apply(fn, _), _) => fn.tpe match {
        case m: MethodType => m.paramTypes.head
      }
    }
  }
}
