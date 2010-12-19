/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * Author: Paul Phillips
 */

package scala.tools.nsc
package matching

import symtab.Flags
import scala.reflect.NameTransformer.decode
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
  import treeInfo.{ unbind, isStar, isVarPattern, isVariableName }

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
    val Ident(name) = tree
    require(isVarPattern(tree) && name != nme.WILDCARD)

    override def irrefutableFor(tpe: Type) = true
    override def description = "%s".format(name)
  }

  // 8.1.1 (b)
  case class WildcardPattern() extends Pattern {
    val tree = EmptyTree
    override def irrefutableFor(tpe: Type) = true
    override def isDefault = true
    override def description = "_"
  }

  // 8.1.2
  case class TypedPattern(tree: Typed) extends Pattern {
    private val Typed(expr, tpt) = tree

    override def subpatternsForVars: List[Pattern] = List(Pattern(expr))

    override def irrefutableFor(tpe: Type) = tpe <:< tree.tpe
    override def simplify(pv: PatternVar) = Pattern(expr) match {
      case ExtractorPattern(ua) if pv.sym.tpe <:< tpt.tpe  => this rebindTo expr
      case _                                                => this
    }
    override def description = "Typ(%s: %s)".format(Pattern(expr), tpt)
  }

  // 8.1.3
  case class LiteralPattern(tree: Literal) extends Pattern {
    val Literal(const @ Constant(value)) = tree

    def isSwitchable = cond(const.tag) { case ByteTag | ShortTag | IntTag | CharTag => true }
    def intValue = const.intValue
    override def description = {
      val s = if (value == null) "null" else value.toString()
      "Lit(%s)".format(s)
    }
  }

  // 8.1.4 (a)
  case class ApplyIdentPattern(tree: Apply) extends ApplyPattern with NamePattern {
    // XXX - see bug 3411 for code which violates this assumption
    // require (!isVarPattern(fn) && args.isEmpty)
    val ident @ Ident(name) = fn

    override def sufficientType = Pattern(ident).equalsCheck
    override def simplify(pv: PatternVar) = this.rebindToObjectCheck()
    override def description = "Id(%s)".format(name)
  }
  // 8.1.4 (b)
  case class ApplySelectPattern(tree: Apply) extends ApplyPattern with SelectPattern {
    require (args.isEmpty)
    val Apply(select: Select, _) = tree

    override def sufficientType = mkSingletonFromQualifier
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

    override def sufficientType = tpe.narrow
    override def simplify(pv: PatternVar) = this.rebindToObjectCheck()
    override def description = "Obj(%s)".format(fn)
  }
  // 8.1.4 (e)
  case class SimpleIdPattern(tree: Ident) extends NamePattern {
    val Ident(name) = tree
    override def description = "Id(%s)".format(name)
  }

  // 8.1.5
  case class ConstructorPattern(tree: Apply) extends ApplyPattern with NamePattern {
    require(fn.isType && this.isCaseClass)
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

    override def description = {
      if (isColonColon) "%s :: %s".format(Pattern(args(0)), Pattern(args(1)))
      else "%s(%s)".format(name, toPats(args).mkString(", "))
    }

    // XXX todo
    // override def irrefutableFor(tpe: Type) = false
  }
  // 8.1.6
  case class TuplePattern(tree: Apply) extends ApplyPattern {
    // XXX todo
    // override def irrefutableFor(tpe: Type) = false
    override def description = "((%s))".format(args.size, toPats(args).mkString(", "))
  }

  // 8.1.7
  case class ExtractorPattern(tree: UnApply) extends UnapplyPattern {
    private val Apply(fn, _) = unfn
    private val MethodType(List(arg, _*), _) = fn.tpe
    private def uaTyped = Typed(tree, TypeTree(arg.tpe)) setType arg.tpe

    override def necessaryType = arg.tpe

    override def simplify(pv: PatternVar) =
      if (pv.sym.tpe <:< arg.tpe) this
      else this rebindTo uaTyped

    override def description = "UnApp(%s => %s)".format(necessaryType, resTypesString)
  }

  // 8.1.8 (unapplySeq calls)
  case class SequenceExtractorPattern(tree: UnApply) extends UnapplyPattern with SequenceLikePattern {

    lazy val UnApply(
      Apply(TypeApply(Select(_, nme.unapplySeq), List(tptArg)), _),
      List(ArrayValue(_, elems))
    ) = tree

    /** For folding a list into a well-typed x :: y :: etc :: tree. */
    private def listFolder = {
      val tpe = tptArg.tpe
      val MethodType(_, TypeRef(pre, sym, _)) = ConsClass.primaryConstructor.tpe
      val consRef                             = typeRef(pre, sym, List(tpe))
      val listRef                             = typeRef(pre, ListClass, List(tpe))

      def fold(x: Tree, xs: Tree) = unbind(x) match {
        case _: Star  => Pattern(x) rebindTo WILD(x.tpe) boundTree  // this is using boundVariables instead of deepBoundVariables
        case _        =>
          val dummyMethod = new TermSymbol(NoSymbol, NoPosition, "matching$dummy")
          val consType    = MethodType(dummyMethod newSyntheticValueParams List(tpe, listRef), consRef)

          Apply(TypeTree(consType), List(x, xs)) setType consRef
      }

      fold _
    }

    // @pre: is not right-ignoring (no star pattern) ; no exhaustivity check
    override def simplify(pv: PatternVar) = {
      pv.sym setFlag NO_EXHAUSTIVE
      this rebindTo elems.foldRight(gen.mkNil)(listFolder)
    }
    override def description = "UnSeq(%s => %s)".format(tptArg, resTypesString)
  }

  trait SequenceLikePattern extends Pattern {
    def elems: List[Tree]
    def elemPatterns = toPats(elems)

    def nonStarPatterns: List[Pattern] = if (hasStar) elemPatterns.init else elemPatterns
    def nonStarLength = nonStarPatterns.length
    def isAllDefaults = nonStarPatterns forall (_.isDefault)

    def isShorter(other: SequenceLikePattern) = nonStarLength < other.nonStarLength
    def isSameLength(other: SequenceLikePattern) = nonStarLength == other.nonStarLength
  }

  // 8.1.8 (b) (literal ArrayValues)
  case class SequencePattern(tree: ArrayValue) extends Pattern with SequenceLikePattern {
    lazy val ArrayValue(elemtpt, elems) = tree

    override def subpatternsForVars: List[Pattern] = elemPatterns
    override def description = "Seq(%s)".format(elemPatterns mkString ", ")
  }

  // 8.1.8 (c)
  case class StarPattern(tree: Star) extends Pattern {
    val Star(elem) = tree
    override def description = "_*"
  }
  // XXX temporary?
  case class ThisPattern(tree: This) extends NamePattern {
    val This(name) = tree
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
    private val cache = new collection.mutable.HashMap[Tree, Pattern]

    def unadorn(x: Tree): Tree = x match {
      case Typed(expr, _) => unadorn(expr)
      case Bind(_, x)     => unadorn(x)
      case _              => x
    }

    def isRightIgnoring(t: Tree) = cond(unadorn(t)) {
      case ArrayValue(_, xs) if !xs.isEmpty => isStar(unadorn(xs.last))
    }

    def apply(tree: Tree): Pattern = {
      if (cache contains tree)
        return cache(tree)

      val p = tree match {
        case x: Bind              => apply(unbind(tree)) withBoundTree x
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
      def isModule  = x.symbol.isModule || x.tpe.termSymbol.isModule
      def isTuple   = isTupleTypeOrSubtype(fn.tpe)

      if (fn.isType) {
        if (isTuple) TuplePattern(x)
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
      case _: This if isVariableName(name)  => Some("`%s`".format(name))
      case _                                => None
    }

    protected def getPathSegments(t: Tree): List[Name] = t match {
      case Select(q, name)  => name :: getPathSegments(q)
      case Apply(f, Nil)    => getPathSegments(f)
      case _                => Nil
    }
    protected def mkSingletonFromQualifier = {
      def pType = qualifier match {
        case _: Apply => PseudoType(tree)
        case _        => singleType(Pattern(qualifier).necessaryType, sym)
      }
      qualifier.tpe match {
        case t: ThisType  => singleType(t, sym) // this.X
        case _            => pType
      }
    }
  }

  sealed trait NamePattern extends Pattern {
    def name: Name
    override def sufficientType = tpe.narrow
    override def simplify(pv: PatternVar) = this.rebindToEqualsCheck()
    override def description = name.toString()
  }

  sealed trait UnapplyPattern extends Pattern {
    lazy val UnApply(unfn, args) = tree
    override def subpatternsForVars: List[Pattern] = toPats(args)

    def resTypes = analyzer.unapplyTypeList(unfn.symbol, unfn.tpe)
    def resTypesString = resTypes match {
      case Nil  => "Boolean"
      case xs   => xs.mkString(", ")
    }

    private def isSameFunction(f1: Tree, f2: Tree) =
      (f1.symbol == f2.symbol) && (f1 equalsStructure f2)

    // XXX args
    def isSameUnapply(other: UnapplyPattern) =
      isSameFunction(unfn, other.unfn)
  }

  sealed trait ApplyPattern extends Pattern {
    protected lazy val Apply(fn, args) = tree
    override def subpatternsForVars: List[Pattern] = toPats(args)

    override def dummies =
      if (!this.isCaseClass) Nil
      else emptyPatterns(sufficientType.typeSymbol.caseFieldAccessors.size)

    def isConstructorPattern = fn.isType
  }

  sealed abstract class Pattern extends PatternBindingLogic {
    val tree: Tree

    // returns either a simplification of this pattern or identity.
    def simplify(pv: PatternVar): Pattern = this
    def simplify(): Pattern = this simplify null

    // the right number of dummies for this pattern
    def dummies: List[Pattern] = Nil

    // 8.1.13
    // A pattern p is irrefutable for type T if any of the following applies:
    //   1) p is a variable pattern
    //   2) p is a typed pattern x: T', and T <: T'
    //   3) p is a constructor pattern C(p1,...,pn), the type T is an instance of class C,
    //      the primary constructor of type T has argument types T1,...,Tn and and each
    //      pi is irrefutable for Ti.
    def irrefutableFor(tpe: Type) = false

    // does this pattern completely cover that pattern (i.e. latter cannot be matched)
    def completelyCovers(second: Pattern) = false

    // Is this a default pattern (untyped "_" or an EmptyTree inserted by the matcher)
    def isDefault = false

    // what type must a scrutinee have to have any chance of matching this pattern?
    def necessaryType = tpe

    // what type could a scrutinee have which would automatically indicate a match?
    // (nullness and guards will still be checked.)
    def sufficientType = tpe

    // XXX have to determine if this can be made useful beyond an extractor barrier.
    // Default sufficient type might be NothingClass.tpe, tpe.narrow, ...

    // the subpatterns for this pattern (at the moment, that means constructor arguments)
    def subpatterns(pm: MatchMatrix#PatternMatch): List[Pattern] = pm.dummies

    def    sym  = tree.symbol
    def    tpe  = tree.tpe
    def prefix  = tpe.prefix
    def isEmpty = tree.isEmpty

    def isSymValid = (sym != null) && (sym != NoSymbol)
    def isModule = sym.isModule || tpe.termSymbol.isModule
    def isCaseClass = tpe.typeSymbol.isCase
    def isObject = isSymValid && prefix.isStable  // XXX not entire logic

    def unadorn(t: Tree): Tree = Pattern unadorn t

    private def isStar(x: Tree) = cond(unadorn(x)) { case Star(_) => true }
    private def endsStar(xs: List[Tree]) = xs.nonEmpty && isStar(xs.last)

    def isStarSequence = isSequence && hasStar
    def isSequence = cond(unadorn(tree)) {
      case ArrayValue(_, _) => true
    }
    def hasStar = cond(unadorn(tree)) {
      case ArrayValue(_, xs) if endsStar(xs)  => true
    }

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
    def description = super.toString()
    def bindingsDescription =
      if (boundTree.isEmpty) ""
      else (boundVariables map (_.name)).mkString("", ", ", " @ ")

    final override def toString() = {
      if (boundVariables.isEmpty) description
      else "%s%s".format(bindingsDescription, description)
    }
    def toTypeString() = "%s <: x <: %s".format(necessaryType, sufficientType)
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