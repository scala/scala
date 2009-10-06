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
  def emptyPatterns(i: Int): List[Pattern] = List.fill(i)(NoPattern)
  def emptyTrees(i: Int): List[Tree] = List.fill(i)(EmptyTree)

  // An empty pattern
  def NoPattern = WildcardPattern()

  // The constant null pattern
  def NullPattern = LiteralPattern(NULL)

  // 8.1.1
  case class VariablePattern(tree: Ident) extends Pattern {
    require(tree != Ident(nme.WILDCARD))

    override def irrefutableFor(tpe: Type) = true
    override def simplify(testVar: Symbol) = this.rebindToEqualsCheck()

    // override def matchingType = mkSingleton ??? XXX
  }

  // 8.1.1 (b)
  case class WildcardPattern() extends Pattern {
    val tree = EmptyTree
    override def irrefutableFor(tpe: Type) = true
    override def isDefault = true
    override def toString() = "_"
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
  case class LiteralPattern(tree: Literal) extends Pattern {
    val Literal(const @ Constant(value)) = tree

    def isSwitchable = cond(const.tag) { case ByteTag | ShortTag | IntTag | CharTag => true }
    def intValue = const.intValue
    override def toString() = if (value == null) "null" else value.toString()
  }

  // 8.1.4
  case class StableIdPattern(tree: Select) extends IdentifierPattern {
    val Select(qualifier, name) = tree

    def pathSegments = getPathSegments(tree)
    private def getPathSegments(t: Tree): List[Name] = t match {
      case Select(q, name)  => name :: getPathSegments(q)
      case Apply(f, Nil)    => getPathSegments(f)
      case _                => Nil
    }

    override def toString() = "StableId(%s)".format(pathSegments.mkString(" . "))
  }

  // 8.1.4 (b)
  case class SelectPattern(tree: Select) extends IdentifierPattern {
    val Select(qualifier, name) = tree

    override def toString() = "Select(%s, %s)".format(qualifier, name)
  }

  trait IdentifierPattern extends Pattern {
    val name: Name
    override def simplify(testVar: Symbol) = this.rebindToEqualsCheck()
    override def matchingType = mkSingleton
  }

  // 8.1.5
  case class ConstructorPattern(tree: Apply) extends ApplyPattern {
    require(fn.isType && this.isCaseClass)

    override def subpatterns(pm: MatchMatrix#PatternMatch) =
      if (pm.isCaseHead) args map Pattern.apply
      else super.subpatterns(pm)

    override def simplify(testVar: Symbol) =
      if (args.isEmpty) this rebindToEmpty tree.tpe
      else this

    override def toString() = "Constructor(%s)".format(toPats(args).mkString(", "))

    // XXX todo
    // override def irrefutableFor(tpe: Type) = false
  }
  // XXX temp
  case class ApplyValuePattern(tree: Apply) extends ApplyPattern {
    require(!fn.isType)
    //
    // val Apply(Select(qualifier, name), Nil) = tree
    //
    // Examples where the above does not match:
    // files/pos/t0710.scala  Child()
    // files/run/lazy-exprs.scala   Z1()
    // files/run/patmatnew.scala Child()

    override def simplify(testVar: Symbol) = {
      def examinePrefix(path: Tree) = (path, path.tpe) match {
        case (_, t: ThisType)     => singleType(t, sym) // this.X
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

    override def matchingType = arg.tpe

    // lazy val ua @ UnApply(app, args) = head.tree
    // lazy val Apply(fxn, _ :: trailingArgs) = app

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
  case class SequencePattern(tree: ArrayValue) extends Pattern {
    lazy val ArrayValue(elemtpt, elems) = tree
    lazy val elemPatterns = toPats(elems)
    lazy val nonStarPatterns = if (hasStar) elemPatterns.init else elemPatterns

    def hasStar = isRightIgnoring(tree)
    def nonStarLength = nonStarPatterns.length
    def isAllDefaults = nonStarPatterns forall (_.isDefault)

    def rebindStar(seqType: Type): List[Pattern] = {
      require(hasStar)
      nonStarPatterns ::: List(elemPatterns.last rebindToType seqType)
    }

    /** True if 'next' must be checked even if 'first' failed to match after passing its length test
      * (the conditional supplied by getPrecondition.) This is an optimization to avoid checking sequences
      * which cannot match due to a length incompatibility.
      */

    override def completelyCovers(second: Pattern): Boolean = {
      if (second.isDefault) return false

      second match {
        case x: SequencePattern  =>
          val (len1, len2)    = (nonStarLength, x.nonStarLength)
          val (star1, star2)  = (this.hasStar, x.hasStar)

          // this still needs rewriting.
          val res =
          ( star1 &&  star2 && len2  < len1                     ) ||  // Seq(a,b,c,_*) followed by Seq(a,b,_*) because of (a,b)
          ( star1 && !star2 && len2  < len1 &&    isAllDefaults ) ||  // Seq(a,b,c,_*) followed by Seq(a,b) because of (a,b)
          // ( star1 &&           len2  < len1                     ) ||
          (!star1 &&  star2                                     ) ||
          (!star1 && !star2 && len2 >= len1                     )

          !res
        case _ =>
          // shouldn't happen...
          false
      }
    }
  }

  // 8.1.8 (b)
  // temporarily subsumed by SequencePattern
  // case class SequenceStarPattern(tree: ArrayValue) extends Pattern { }

  // abstract trait ArrayValuePattern extends Pattern {
  //   val tree: ArrayValue
  //   lazy val av @ ArrayValue(elemTpt, elems) = tree
  //   lazy val elemPatterns = toPats(elems)
  //   def nonStarElems = if (isRightIgnoring) elems.init else elems
  // }

  // 8.1.8 (c)
  case class StarPattern(tree: Star) extends Pattern {
    val Star(elem) = tree
  }

  // 8.1.9
  // InfixPattern ... subsumed by Constructor/Extractor Patterns

  // 8.1.10
  case class AlternativePattern(tree: Alternative) extends Pattern {
    private val Alternative(subtrees) = tree
    // override def subpatterns(pats: PatternMatch) = subtrees map Pattern.apply
  }

  // 8.1.11
  // XMLPattern ... for now, subsumed by SequencePattern, but if we want
  //   to make it work right, it probably needs special handling.


  object Pattern {
    def apply(tree: Tree): Pattern = {
      val p = tree match {
        case x: Bind              => apply(unbind(tree)) withBoundTree x
        case EmptyTree            => WildcardPattern()
        case Ident(nme.WILDCARD)  => WildcardPattern()
        case x @ Alternative(ps)  => AlternativePattern(x)
        case x: Apply             => ApplyPattern(x)
        case x: Typed             => TypedPattern(x)
        case x: Literal           => LiteralPattern(x)
        case x: UnApply           => UnapplyPattern(x)
        case x: Ident             => VariablePattern(x)
        // case x: Ident             => if (isVarPattern(x)) VariablePattern(x) else StableIdPattern(x)
        // case x: ArrayValue        => if (isRightIgnoring(x)) SequenceStarPattern(x) else SequencePattern(x)
        case x: ArrayValue        => SequencePattern(x)
        case x: Select            => SelectPattern(x)
        case x: Star              => StarPattern(x)
        case _                    => abort("Unknown Tree reached pattern matcher: %s/%s".format(tree, tree.getClass))
      }

      p match {
        case WildcardPattern()  => p
        case _: LiteralPattern  => p
        case _                  => traceAndReturn("[Pattern] ", p)
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

      // case x @ Apply(sel: Select, Nil)  => StableIdPattern(sel)

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
  sealed abstract class Pattern extends PatternBindingLogic {
    val tree: Tree
    // The logic formerly in classifyPat, returns either a simplification
    // of this pattern or identity.
    def simplify(testVar: Symbol): Pattern = this
    def simplify(): Pattern = this simplify NoSymbol

    def subpatterns(pm: MatchMatrix#PatternMatch): List[Pattern] = pm.dummies

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

    def    sym  = tree.symbol
    def    tpe  = tree.tpe
    def prefix  = tpe.prefix
    def isEmpty = tree.isEmpty

    // this is used when this pattern is the foremost under consideration
    def matchingType = tpe

    def isSymValid = (sym != null) && (sym != NoSymbol)
    def isModule = sym.isModule || tpe.termSymbol.isModule
    def isCaseClass = tpe.typeSymbol hasFlag Flags.CASE
    def isObject = isSymValid && prefix.isStable  // XXX not entire logic

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

    final def isAlternative       = cond(tree) { case Alternative(_) => true }

    /** Standard methods **/
    def copy(tree: Tree = this.tree): Pattern =
      if (boundTree eq tree) Pattern(tree)
      else Pattern(tree) withBoundTree boundTree.asInstanceOf[Bind]

    // override def toString() = "Pattern(%s, %s)".format(tree, boundVariables)
    override def equals(other: Any) = other match {
      case x: Pattern => this.boundTree == x.boundTree
      case _          => super.equals(other)
    }
    override def hashCode() = boundTree.hashCode()
  }

  trait PatternBindingLogic {
    self: Pattern =>

    // XXX only a var for short-term experimentation.
    private var _boundTree: Bind = null
    def boundTree = if (_boundTree == null) tree else _boundTree
    def withBoundTree(x: Bind): this.type = {
      _boundTree = x
      this
    }
    lazy val boundVariables = strip(boundTree)

    def definedVars = definedVarsInternal(boundTree)
    private def definedVarsInternal(x: Tree): List[Symbol] = {
      def vars(x: Tree): List[Symbol] = x match {
        case Apply(_, args)     => args flatMap vars
        case b @ Bind(_, p)     => b.symbol :: vars(p)
        case Typed(p, _)        => vars(p)              // otherwise x @ (_:T)
        case UnApply(_, args)   => args flatMap vars
        case ArrayValue(_, xs)  => xs flatMap vars
        case x                  => Nil
      }
      vars(x) reverse
    }

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

    /** Helpers **/
    private def strip(t: Tree): List[Symbol] = t match {
      case b @ Bind(_, pat) => b.symbol :: strip(pat)
      case _                => Nil
    }
  }

  /*** Extractors ***/

  object UnapplyParamType {
    def unapply(x: Tree): Option[Type] = condOpt(unbind(x)) {
      case UnApply(Apply(fn, _), _) => fn.tpe match {
        case m: MethodType => m.paramTypes.head
      }
    }
  }

  object SeqStarSubPatterns {
    def removeStar(xs: List[Tree], seqType: Type): List[Pattern] = {
      val ps = toPats(xs)
      ps.init ::: List(ps.last rebindToType seqType)
    }

    def unapply(x: Pattern)(implicit min: Int, seqType: Type): Option[List[Pattern]] = x.tree match {
      case av @ ArrayValue(_, xs) =>
        if      (!isRightIgnoring(av) && xs.length   == min) Some(toPats(xs ::: List(gen.mkNil, EmptyTree)))    // Seq(p1,...,pN)
        else if ( isRightIgnoring(av) && xs.length-1 == min) Some(removeStar(xs, seqType) ::: List(NoPattern))  // Seq(p1,...,pN,_*)
        else if ( isRightIgnoring(av) && xs.length-1  < min) Some(emptyPatterns(min + 1) ::: List(x))           // Seq(p1..,pJ,_*)   J < N
        else None
      case _ =>
        if (x.isDefault) Some(emptyPatterns(min + 1 + 1)) else None
    }
  }
}