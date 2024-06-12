/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

//todo: rewrite or disallow new T where T is a mixin (currently: <init> not a member of T)
//todo: use inherited type info also for vars and values
//todo: disallow C#D in superclass
//todo: treat :::= correctly

package scala
package tools.nsc
package typechecker

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable, mutable.{LinkedHashMap, ListBuffer}
import scala.language.implicitConversions
import scala.reflect.internal.util.{ReusableInstance, Statistics, TriState}
import scala.reflect.internal.TypesStats
import scala.tools.nsc.Reporting.WarningCategory
import symtab.Flags._

/** This trait provides methods to find various kinds of implicits.
 *
 *  @author  Martin Odersky
 */
trait Implicits extends splain.SplainData {
  self: Analyzer =>

  import global._
  import definitions._
  import statistics._
  import typingStack.printTyping
  import typeDebug._
  import scala.util.matching.Regex.Match

  // standard usage
  def inferImplicitFor(pt: Type, tree: Tree, context: Context, reportAmbiguous: Boolean = true): SearchResult =
    inferImplicit(tree, pt, reportAmbiguous, isView = false, context, saveAmbiguousDivergent = true, tree.pos)

  // used by typer to find an implicit coercion
  def inferImplicitView(from: Type, to: Type, tree: Tree, context: Context, reportAmbiguous: Boolean, saveAmbiguousDivergent: Boolean) =
    inferImplicit(tree, Function1(from, to), reportAmbiguous, isView = true, context, saveAmbiguousDivergent, tree.pos)

  // used for manifests, typetags, checking language features, scaladoc
  def inferImplicitByType(pt: Type, context: Context, pos: Position = NoPosition): SearchResult =
    inferImplicit(EmptyTree, pt, reportAmbiguous = true, isView = false, context, saveAmbiguousDivergent = true, pos)

  def inferImplicitByTypeSilent(pt: Type, context: Context, pos: Position = NoPosition): SearchResult =
    inferImplicit(EmptyTree, pt, reportAmbiguous = false, isView = false, context, saveAmbiguousDivergent = false, pos)

  @deprecated("Unused in scalac", "2.12.0-M4")
  def inferImplicit(tree: Tree, pt: Type, reportAmbiguous: Boolean, isView: Boolean, context: Context): SearchResult =
    inferImplicit(tree, pt, reportAmbiguous, isView, context, saveAmbiguousDivergent = true, tree.pos)

  @deprecated("Unused in scalac", "2.12.0-M4")
  def inferImplicit(tree: Tree, pt: Type, reportAmbiguous: Boolean, isView: Boolean, context: Context, saveAmbiguousDivergent: Boolean): SearchResult =
    inferImplicit(tree, pt, reportAmbiguous, isView, context, saveAmbiguousDivergent, tree.pos)

  /** Search for an implicit value. Consider using one of the convenience methods above. This one has many boolean levers.
   *
   * See the comment on `result` at the end of class `ImplicitSearch` for more info how the search is conducted.
   *
   *  @param tree                    The tree for which the implicit needs to be inserted.
   *                                 (the inference might instantiate some of the undetermined
   *                                 type parameters of that tree.
   *  @param pt                      The expected type of the implicit.
   *  @param reportAmbiguous         Should ambiguous implicit errors be reported?
   *                                 False iff we search for a view to find out
   *                                 whether one type is coercible to another.
   *  @param isView                  We are looking for a view
   *  @param context                 The current context
   *  @param saveAmbiguousDivergent  False if any divergent/ambiguous errors should be ignored after
   *                                 implicits search,
   *                                 true if they should be reported (used in further typechecking).
   *  @param pos                     Position that should be used for tracing and error reporting
   *                                 (useful when we infer synthetic stuff and pass EmptyTree in the `tree` argument)
   *                                 If it's set to NoPosition, then position-based services will use `tree.pos`
   *  @return                        A search result
   */
  def inferImplicit(tree: Tree, pt: Type, reportAmbiguous: Boolean, isView: Boolean, context: Context, saveAmbiguousDivergent: Boolean, pos: Position): SearchResult = {
    currentRun.profiler.beforeImplicitSearch(pt)
    try {
      inferImplicit1(tree, pt, reportAmbiguous, isView, context, saveAmbiguousDivergent, pos)
    } finally {
      currentRun.profiler.afterImplicitSearch(pt)
    }
  }

  private def inferImplicit1(tree: Tree, pt: Type, reportAmbiguous: Boolean, isView: Boolean, context: Context, saveAmbiguousDivergent: Boolean, pos: Position): SearchResult = {
    // Note that the isInvalidConversionTarget seems to make a lot more sense right here, before all the
    // work is performed, than at the point where it presently exists.
    val shouldPrint     = printTypings && !context.undetparams.isEmpty
    val findMemberStart = if (settings.areStatisticsEnabled) statistics.startCounter(findMemberImpl) else null
    val subtypeStart    = if (settings.areStatisticsEnabled) statistics.startCounter(subtypeImpl) else null
    val start           = if (settings.areStatisticsEnabled) statistics.startTimer(implicitNanos) else null
    if (shouldPrint)
      typingStack.printTyping(tree, s"typing implicit: $tree ${context.undetparamsString}")
    val implicitSearchContext = context.makeImplicit(reportAmbiguous)
    ImplicitErrors.startSearch(pt)
    val dpt = if (isView) pt else dropByName(pt)
    val isByName = dpt ne pt
    val search = new ImplicitSearch(tree, dpt, isView, implicitSearchContext, pos, isByName)
    pluginsNotifyImplicitSearch(search)
    val result = search.bestImplicit
    pluginsNotifyImplicitSearchResult(result)
    ImplicitErrors.finishSearch(result.isSuccess, pt)

    if (result.isFailure && saveAmbiguousDivergent && implicitSearchContext.reporter.hasErrors)
      implicitSearchContext.reporter.propagateImplicitTypeErrorsTo(context.reporter)

    // scala/bug#7944 undetermined type parameters that result from inference within typedImplicit land in
    //         `implicitSearchContext.undetparams`, *not* in `context.undetparams`
    //         Here, we copy them up to parent context (analogously to the way the errors are copied above),
    //         and then filter out any which *were* inferred and are part of the substitutor in the implicit search result.
    context.undetparams = ((context.undetparams ++ result.undetparams) filterNot result.subst.from.contains).distinct

    if (settings.areStatisticsEnabled) statistics.stopTimer(implicitNanos, start)
    if (settings.areStatisticsEnabled) statistics.stopCounter(findMemberImpl, findMemberStart)
    if (settings.areStatisticsEnabled) statistics.stopCounter(subtypeImpl, subtypeStart)

    if (result.isSuccess) {
      val rts = {
        val infoSym = if (result.implicitInfo != null) result.implicitInfo.sym else NoSymbol
        infoSym.orElse {
          val rts0 = result.tree.symbol
          if (rts0 != null) rts0 else NoSymbol
        }
      }
      if (settings.lintImplicitRecursion) {
        val s = if (rts.isAccessor) rts.accessed else if (rts.isModule) rts.moduleClass else rts
        if (s != NoSymbol && context.owner.hasTransOwner(s))
          context.warning(result.tree.pos, s"Implicit resolves to enclosing $rts", WarningCategory.WFlagSelfImplicit)
      }
      if (result.inPackagePrefix && currentRun.isScala3) {
        val msg =
          s"""Implicit $rts was found in a package prefix of the required type, which is not part of the implicit scope in Scala 3 (or with -Xsource-features:package-prefix-implicits).
             |For migration, add `import ${rts.fullNameString}`.""".stripMargin
        context.warning(result.tree.pos, msg, WarningCategory.Scala3Migration)
      }
    }
    implicitSearchContext.emitImplicitDictionary(result)
  }

  /** A friendly wrapper over inferImplicit to be used in macro contexts and toolboxes.
   */
  def inferImplicit(tree: Tree, pt: Type, isView: Boolean, context: Context, silent: Boolean, withMacrosDisabled: Boolean, pos: Position, onError: (Position, String) => Unit): Tree = {
    val result = context.withMacros(enabled = !withMacrosDisabled) {
      inferImplicit(tree, pt, reportAmbiguous = true, isView = isView, context, saveAmbiguousDivergent = !silent, pos)
    }

    if (result.isFailure && !silent) {
      val err = context.reporter.firstError
      val errPos = err.map(_.errPos).getOrElse(pos)
      val errMsg = err.map(_.errMsg).getOrElse("implicit search has failed. to find out the reason, turn on -Vimplicits")
      onError(errPos, errMsg)
    }
    result.tree
  }

  /** Find all views from type `tp` (in which `tpars` are free)
   *
   * Note that the trees in the search results in the returned list share the same type variables.
   * Ignore their constr field! The list of type constraints returned along with each tree specifies the constraints that
   * must be met by the corresponding type parameter in `tpars` (for the returned implicit view to be valid).
   *
   * @param tp      from-type for the implicit conversion
   * @param context search implicits here
   * @param tpars   symbols that should be considered free type variables
   *                (implicit search should not try to solve them, just track their constraints)
   */
  def allViewsFrom(tp: Type, context: Context, tpars: List[Symbol]): List[(SearchResult, List[TypeConstraint])] = {
    // my untouchable typevars are better than yours (they can't be constrained by them)
    val tvars = tpars map (TypeVar untouchable _)
    val tpSubsted = tp.subst(tpars, tvars)

    val search = new ImplicitSearch(EmptyTree, functionType(List(tpSubsted), AnyTpe), true, context.makeImplicit(reportAmbiguousErrors = false), isByNamePt = false)

    search.allImplicitsPoly(tvars)
  }

  private final val sizeLimit = 50000
  private type Infos = List[ImplicitInfo]
  private type Infoss = List[List[ImplicitInfo]]
  private type InfoMap = LinkedHashMap[Symbol, List[ImplicitInfo]] // A map from class symbols to their associated implicits
  private val implicitsCache = new LinkedHashMap[Type, Infoss]
  private val infoMapCache = new LinkedHashMap[Symbol, InfoMap]
  private val improvesCache = perRunCaches.newMap[(ImplicitInfo, ImplicitInfo), Boolean]()
  private val implicitSearchId = { var id = 1 ; () => try id finally id += 1 }

  private val shadowerUseOldImplementation = java.lang.Boolean.getBoolean("scalac.implicit.shadow.old")
  def resetImplicits(): Unit = {
    implicitsCache.clear()
    infoMapCache.clear()
    improvesCache.clear()
  }

  /** The result of an implicit search
   *  @param  tree    The tree representing the implicit
   *  @param  subst   A substituter that represents the undetermined type parameters
   *                  that were instantiated by the winning implicit.
   *  @param undetparams undetermined type parameters
   */
  class SearchResult(val tree: Tree, val subst: TreeTypeSubstituter, val undetparams: List[Symbol], val inPackagePrefix: Boolean = false, val implicitInfo: ImplicitInfo = null) {
    override def toString = s"SearchResult($tree, ${if (subst.isEmpty) "" else subst}, $inPackagePrefix)"

    def isFailure          = false
    def isAmbiguousFailure = false
    def isDivergent        = false
    final def isSuccess    = !isFailure
  }

  lazy val SearchFailure = new SearchResult(EmptyTree, EmptyTreeTypeSubstituter, Nil) {
    override def isFailure = true
  }

  lazy val DivergentSearchFailure = new SearchResult(EmptyTree, EmptyTreeTypeSubstituter, Nil) {
    override def isFailure   = true
    override def isDivergent = true
  }

  lazy val AmbiguousSearchFailure = new SearchResult(EmptyTree, EmptyTreeTypeSubstituter, Nil) {
    override def isFailure          = true
    override def isAmbiguousFailure = true
  }

  /** A class that records an available implicit
   *  @param   name   The name of the implicit
   *  @param   pre    The prefix type of the implicit
   *  @param   sym    The symbol of the implicit
   */
  class ImplicitInfo(val name: Name, val pre: Type, val sym: Symbol, val inPackagePrefix: Boolean = false, val importInfo: ImportInfo = null, val importSelector: ImportSelector = null) {
    private[this] var tpeCache: Type = null
    private[this] var depolyCache: Type = null
    private[this] var isErroneousCache: TriState = TriState.Unknown

    /** Computes member type of implicit from prefix `pre` (cached). */
    final def tpe: Type = {
      if (tpeCache eq null) tpeCache = pre.memberType(sym)
      tpeCache
    }

    /* Map a polytype to one in which all type parameters and argument-dependent types are replaced by wildcards.
     * Consider `implicit def b(implicit x: A): x.T = error("")`. We need to approximate de Bruijn index types
     * when checking whether `b` is a valid implicit, as we haven't even searched a value for the implicit arg `x`,
     * so we have to approximate (otherwise it is excluded a priori).
     */
    final def depoly: Type = {
      if (depolyCache eq null) {
        depolyCache = tpe match {
          case PolyType(tparams, restpe) => deriveTypeWithWildcards(tparams)(ApproximateDependentMap(restpe))
          case _                         => ApproximateDependentMap(tpe)
        }
      }
      depolyCache
    }

    def dependsOnPrefix: Boolean = pre match {
      case SingleType(pre0, _) => tpe.exists(_ =:= pre0)
      case _ => false
    }

    def isSearchedPrefix: Boolean = name == null && sym == NoSymbol

    def isCyclicOrErroneous: Boolean =
      if(sym.hasFlag(LOCKED)) true
      else {
        if(!isErroneousCache.isKnown)
          isErroneousCache = computeErroneous
        isErroneousCache.booleanValue
      }

    private[this] final def computeErroneous =
      try containsError(tpe)
      catch { case _: CyclicReference => true }

    final var useCountArg: Int = 0
    final var useCountView: Int = 0
    final def useCount(isView: Boolean): Int = if (isView) useCountView else useCountArg

    /** Does type `tp` contain an Error type as parameter or result?
     */
    private final def containsError(tp: Type): Boolean = tp match {
      case PolyType(tparams, restpe) =>
        containsError(restpe)
      case NullaryMethodType(restpe) =>
        containsError(restpe)
      case mt @ MethodType(_, restpe) =>
        // OPT avoiding calling `mt.paramTypes` which creates a new list.
        mt.params.exists(_.tpe.isError) || containsError(restpe)
      case _ =>
        tp.isError
    }

    final def isStablePrefix = pre.isStable

    override def equals(other: Any) = other match {
      case that: ImplicitInfo =>
          this.name == that.name &&
          this.pre =:= that.pre &&
          this.sym == that.sym
      case _ => false
    }
    override def hashCode = {
      import scala.util.hashing.MurmurHash3._
      finalizeHash(mix(mix(productSeed, name.##), sym.##), 2)
    }
    override def toString = s"$name: ${ if (tpeCache eq null) "?" else tpe.toString }"
  }

  /** A class which is used to track pending implicits to prevent infinite implicit searches.
   */
  case class OpenImplicit(info: ImplicitInfo, pt: Type, tree: Tree) {
    // JZ: should be a case class parameter, but I have reason to believe macros/plugins peer into OpenImplicit
    // so I'm avoiding a signature change
    def isView: Boolean = _isView
    private def isView_=(value: Boolean): Unit = _isView = value

    private[this] var _isView: Boolean = false

    def isByName: Boolean = _isByName
    private def isByName_=(value: Boolean): Unit = _isByName = value
    private[this] var _isByName: Boolean = false
  }
  object OpenImplicit {
    def apply(info: ImplicitInfo, pt: Type, tree: Tree, isView: Boolean, isByName: Boolean): OpenImplicit = {
      val result = new OpenImplicit(info, pt, tree)
      result.isView = isView
      result.isByName = isByName
      result
    }
  }

  /** A sentinel indicating no implicit was found */
  val NoImplicitInfo = new ImplicitInfo(null, NoType, NoSymbol) {
    // equals used to be implemented in ImplicitInfo with an `if(this eq NoImplicitInfo)`
    // overriding the equals here seems cleaner and benchmarks show no difference in performance
    override def equals(other: Any) = other match { case that: AnyRef => that eq this  case _ => false }
    override def hashCode = 1
  }

  def SearchedPrefixImplicitInfo(pre: Type) = new ImplicitInfo(null, pre, NoSymbol)

  /** A constructor for types ?{ def/type name: tp }, used in infer view to member
   *  searches.
   */
  def memberWildcardType(name: Name, tp: Type) = {
    val result = refinedType(List(WildcardType), NoSymbol)
    val owner = result.typeSymbol
    (name match {
      case x: TermName => owner.newMethod(x)
      case x: TypeName => owner.newAbstractType(x)
    }).setInfoAndEnter(tp)

    result
  }

  // TODO: use ProtoType for HasMember/HasMethodMatching

  /** An extractor for types of the form ? { name: ? }
   */
  object HasMember {
    private val hasMemberCache = perRunCaches.newMap[Name, Type]()
    def apply(name: Name): Type = hasMemberCache.getOrElseUpdate(name, memberWildcardType(name, WildcardType))
  }

  /** An extractor for types of the form ? { name: (? >: argtpe <: Any*)restp }
   */
  object HasMethodMatching {
    val dummyMethod = NoSymbol.newTermSymbol(TermName("typer$dummy")) setInfo NullaryMethodType(AnyTpe)

    def templateArgType(argtpe: Type) = new BoundedWildcardType(TypeBounds.lower(argtpe))

    def apply(name: Name, argtpes: List[Type], restpe: Type): Type = {
      val mtpe = MethodType(dummyMethod.newSyntheticValueParams(argtpes map templateArgType), restpe)
      memberWildcardType(name, mtpe)
    }
    def unapply(pt: Type): Option[(Name, List[Type], Type)] = pt match {
      case RefinedType(List(WildcardType), decls) =>
        decls.toList match {
          case List(sym) =>
            sym.tpe match {
              case MethodType(params, restpe)
              if (params forall (_.tpe.isInstanceOf[BoundedWildcardType])) =>
                Some((sym.name, params map (_.tpe.lowerBound), restpe))
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }
  }

  /** An extractor for unary function types arg => res
   */
  object Function1 {
    val Sym = FunctionClass(1)
    val Pre = Sym.typeConstructor.prefix

    def apply(from: Type, to: Type) = TypeRef(Pre, Sym, List(from, to))

    // It is tempting to think that this should be inspecting "tp baseType Sym"
    // rather than tp. See test case run/t8280 and the commit message which
    // accompanies it for explanation why that isn't done.
    def unapply(tp: Type) = tp match {
      case TypeRef(_, Sym, arg1 :: arg2 :: _) => Some((arg1, arg2))
      case _                                  => None
    }
  }

  /** A class that sets up an implicit search. For more info, see comments for `inferImplicit`.
   *  @param tree             The tree for which the implicit needs to be inserted.
   *  @param pt               The original expected type of the implicit.
   *  @param isView           We are looking for a view
   *  @param context0         The context used for the implicit search
   *  @param pos0             Position that is preferable for use in tracing and error reporting
   *                          (useful when we infer synthetic stuff and pass EmptyTree in the `tree` argument)
   *                          If it's set to NoPosition, then position-based services will use `tree.pos`
   */
  class ImplicitSearch(val tree: Tree, val pt: Type, val isView: Boolean, val context0: Context, val pos0: Position = NoPosition, val isByNamePt: Boolean = false) extends Typer(context0) with ImplicitsContextErrors {
    val searchId = implicitSearchId()
    private def typingLog(what: String, msg: => String) = {
      if (printingOk(tree))
        typingStack.printTyping(s"[search #$searchId] $what $msg")
    }

    import infer._
    if (settings.areStatisticsEnabled) statistics.incCounter(implicitSearchCount)

    /** The type parameters to instantiate */
    val undetParams = if (isView) Nil else context.outer.undetparams
    val wildPt = approximate(pt)
    private[this] def functionArityOf(tp: Type): Int = {
      val dealiased = tp.dealiasWiden
      if (isFunctionTypeDirect(dealiased)) dealiased.typeArgs.length - 1 else -1
    }
    private[this] val cachedPtFunctionArity: Int = functionArityOf(pt)
    final def functionArity(tp: Type): Int = if ((tp eq pt) || (tp eq wildPt)) cachedPtFunctionArity else functionArityOf(tp)

    private val stableRunDefsForImport = currentRun.runDefinitions
    import stableRunDefsForImport._

    def undet_s = if (undetParams.isEmpty) "" else undetParams.mkString(" inferring ", ", ", "")
    def tree_s = typeDebug ptTree tree
    def ctx_s = fullSiteString(context)
    typingLog("start", s"`$tree_s`$undet_s, searching for adaptation to pt=$pt $ctx_s")

    def pos = if (pos0 != NoPosition) pos0 else tree.pos

    @inline final def failure(what: Any, reason: => String, pos: Position = this.pos): SearchResult = {
      if (settings.debug.value)
        reporter.echo(pos, s"$what is not a valid implicit value for $pt because:\n$reason")
      SearchFailure
    }
    /** Is implicit info `info1` better than implicit info `info2`?
     */
    def improves(info1: ImplicitInfo, info2: ImplicitInfo) = {
      if (settings.areStatisticsEnabled) statistics.incCounter(improvesCount)
      (info2 == NoImplicitInfo) ||
      (info1 != NoImplicitInfo) && {
        if (info1.sym.isStatic && info2.sym.isStatic) {
          improvesCache get ((info1, info2)) match {
            case Some(b) => if (settings.areStatisticsEnabled) statistics.incCounter(improvesCachedCount); b
            case None =>
              val result = isStrictlyMoreSpecific(info1.tpe, info2.tpe, info1.sym, info2.sym)
              improvesCache((info1, info2)) = result
              result
          }
        } else isStrictlyMoreSpecific(info1.tpe, info2.tpe, info1.sym, info2.sym)
      }
    }
    def isPlausiblyCompatible(tp: Type, pt: Type) = checkCompatibility(fast = true, tp, pt)
    def normSubType(tp: Type, pt: Type) = checkCompatibility(fast = false, tp, pt)

    /** Does stripped core type `dtor` dominate the stripped core type `dted`?
     *  This is the case if both types are the same wrt `=:=`, or if they overlap
     *  and the complexity of `dtor` is higher than the complexity of `dted`.
     *  The _stripped core_ of a type is the type where
     *   - all refinements and annotations are dropped,
     *   - all universal and existential quantification is eliminated
     *     by replacing variables by their upper bounds,
     *   - all remaining free type parameters in the type are replaced by WildcardType.
     *  The _complexity_ of a stripped core type corresponds roughly to the number of
     *  nodes in its ast, except that singleton types are widened before taking the complexity.
     *  Two types overlap if they have the same type symbol, or
     *  if one or both are intersection types with a pair of overlapping parent types.
     */
    private def dominates(dtor: Type, dted: Type): Boolean = {
      @annotation.tailrec def sumComplexity(acc: Int, xs: List[Type]): Int = xs match {
        case h :: t => sumComplexity(acc + complexity(h), t)
        case _      => acc
      }

      def complexity(tp: Type): Int = tp.dealias match {
        case NoPrefix                => 0
        case SingleType(_, sym)      => if (sym.hasPackageFlag) 0 else complexity(tp.dealiasWiden)
        case ThisType(sym)           => if (sym.hasPackageFlag) 0 else 1
        case TypeRef(pre, _, args)   => 1 + complexity(pre) + sumComplexity(0, args)
        case RefinedType(parents, _) => 1 + sumComplexity(0, parents)
        case _                       => 1
      }

      def overlaps(tp1: Type, tp2: Type): Boolean = (tp1, tp2) match {
        case (RefinedType(parents, _), _) => parents exists (overlaps(_, tp2))
        case (_, RefinedType(parents, _)) => parents exists (overlaps(tp1, _))
        case _                            => tp1.typeSymbol == tp2.typeSymbol
      }

      overlaps(dtor, dted) && {
        complexity(dtor) compareTo complexity(dted) match {
          case 0 => dtor =:= dted
          case cmp => cmp > 0
        }
      }
    }

    private def core(tp: Type): Type = tp.dealiasWiden match {
      case RefinedType(parents, _)            => intersectionType(parents map core, tp.typeSymbol.owner)
      case AnnotatedType(_, underlying)       => core(underlying)
      case ExistentialType(tparams, result)   => core(result).subst(tparams, tparams map (t => core(t.info.upperBound)))
      case PolyType(tparams, result)          => core(result).subst(tparams, tparams map (t => core(t.info.upperBound)))
      case TypeRef(pre, sym, args)            =>
        val coreArgs = args.mapConserve(core)
        if (coreArgs eq args) tp
        else typeRef(pre, sym, coreArgs)
      case _                                  => tp
    }

    private def stripped(tp: Type): Type = {
      // `t.typeSymbol` returns the symbol of the normalized type. If that normalized type
      // is a `PolyType`, the symbol of the result type is collected. This is precisely
      // what we require for scala/bug#5318.
      val syms = for (t <- tp; if t.typeSymbol.isTypeParameter) yield t.typeSymbol
      deriveTypeWithWildcards(syms.distinct)(tp)
    }

    private object AllSymbols extends TypeCollector(Set[Symbol](NoSymbol)) {
      def apply(tp: Type): Unit = tp match {
        case SingleType(pre, sym) =>
          result += tp.typeSymbol
          result += sym
          apply(pre)
          apply(tp.dealiasWiden)
        case ThisType(sym) =>
          result += sym
        case TypeRef(pre, sym, args) =>
          result += sym
          apply(pre)
          args.foreach(apply)
        case RefinedType(parents, _) =>
          parents.foreach(apply)
        case _ =>
          tp.foldOver(this)
      }
    }

    /** The expected type with all undetermined type parameters replaced with wildcards. */
    def approximate(tp: Type) = deriveTypeWithWildcards(undetParams)(tp)

    /** Try to construct a typed tree from given implicit info with given
     *  expected type.
     *  Detect infinite search trees for implicits.
     *
     *  @param info              The given implicit info describing the implicit definition
     *  @param isLocalToCallsite Is the implicit in the local scope of the call site?
     *  @pre `info.tpe` does not contain an error
     */
    private def typedImplicit(info: ImplicitInfo, ptChecked: Boolean, isLocalToCallsite: Boolean): SearchResult = {
      // scala/bug#7167 let implicit macros decide what amounts for a divergent implicit search
      // imagine a macro writer which wants to synthesize a complex implicit Complex[T] by making recursive calls to Complex[U] for its parts
      // e.g. we have `class Foo(val bar: Bar)` and `class Bar(val x: Int)`
      // then it's quite reasonable for the macro writer to synthesize Complex[Foo] by calling `inferImplicitValue(typeOf[Complex[Bar])`
      // however if we didn't insert the `info.sym.isMacro` check here, then under some circumstances
      // (e.g. as described here https://groups.google.com/group/scala-internals/browse_thread/thread/545462b377b0ac0a)
      // `dominates` might decide that `Bar` dominates `Foo` and therefore a recursive implicit search should be prohibited
      // now when we yield control of divergent expansions to the macro writer, what happens next?
      // in the worst case, if the macro writer is careless, we'll get a StackOverflowException from repeated macro calls
      // otherwise, the macro writer could check `c.openMacros` and `c.openImplicits` and do `c.abort` when expansions are deemed to be divergent
      // upon receiving `c.abort` the typechecker will decide that the corresponding implicit search has failed
      // which will fail the entire stack of implicit searches, producing a nice error message provided by the programmer
      val existsDominatedImplicit: Boolean =
        if (tree == EmptyTree) false
        else {
          lazy val ptStripped = stripped(core(pt))
          lazy val ptStrippedSyms = AllSymbols.collect(ptStripped)

          // Are all the symbols of the stripped core of dominating pt contained in the stripped core of tp?
          def coversDominatingPt(tp: Type): Boolean = {
            val tpStripped = stripped(core(tp))
            dominates(ptStripped, tpStripped) && ptStrippedSyms == AllSymbols.collect(tpStripped)
          }

          @tailrec
          def loop(ois: List[OpenImplicit], belowByName: Boolean): Boolean = ois match {
            case Nil => false
            case (hd @ OpenImplicit(info1, tp, tree1)) :: tl =>
              val possiblyDominated = !info1.sym.isMacro && tree1.symbol == tree.symbol
              if (possiblyDominated && belowByName && tp =:= pt) false
              else if (possiblyDominated && coversDominatingPt(tp)) true
              else loop(tl, hd.isByName || belowByName)
          }

          loop(context.openImplicits, this.isByNamePt)
        }

      if(existsDominatedImplicit) {
        //println("Pending implicit "+pending+" dominates "+pt+"/"+undetParams) //@MDEBUG
        DivergentSearchFailure
      } else {
        val ref = context.refByNameImplicit(pt)
        if(ref != EmptyTree)
          new SearchResult(ref, EmptyTreeTypeSubstituter, Nil, inPackagePrefix = info.inPackagePrefix)
        else {
          @tailrec
          def loop(ois: List[OpenImplicit], isByName: Boolean): Option[OpenImplicit] =
            ois match {
              case hd :: tl if (isByName || hd.isByName) && hd.pt <:< pt => Some(hd)
              case hd :: tl => loop(tl, isByName || hd.isByName)
              case _ => None
            }

          val recursiveImplicit: Option[OpenImplicit] = loop(context.openImplicits, isByNamePt)

          recursiveImplicit match {
            case Some(rec) =>
              val ref = atPos(pos.focus)(context.linkByNameImplicit(rec.pt))
              new SearchResult(ref, EmptyTreeTypeSubstituter, Nil, inPackagePrefix = info.inPackagePrefix)
            case None =>
              try {
                context.openImplicits = OpenImplicit(info, pt, tree, isView, isByNamePt) :: context.openImplicits
                //println("  "*context.openImplicits.length+"typed implicit "+info+" for "+pt) //@MDEBUG
                val result = typedImplicit0(info, ptChecked, isLocalToCallsite)
                if (result.isDivergent) {
                  //println("DivergentImplicit for pt:"+ pt +", open implicits:"+context.openImplicits) //@MDEBUG
                  if (context.openImplicits.tail.isEmpty && !pt.isErroneous)
                    DivergingImplicitExpansionError(tree, pt, info.sym)(context)
                  result
                } else context.defineByNameImplicit(pt, result)
              } finally {
                context.openImplicits = context.openImplicits.tail
              }
          }
        }
      }
    }

    /** Does type `tp` match expected type `pt`
     *  This is the case if either `pt` is a unary function type with a
     *  HasMethodMatching type as result, and `tp` is a unary function
     *  or method type whose result type has a method whose name and type
     *  correspond to the HasMethodMatching type,
     *  or otherwise if `tp` is compatible with `pt`.
     *  This method is performance critical: 5-8% of typechecking time.
     */
    private def matchesPt(tp: Type, pt: Type, undet: List[Symbol]): Boolean = {
      val start = if (settings.areStatisticsEnabled) statistics.startTimer(matchesPtNanos) else null
      val result = normSubType(tp, pt) || isView && {
        pt match {
          case Function1(arg1, arg2) => matchesPtView(tp, arg1, arg2, undet)
          case _                     => false
        }
      }
      if (settings.areStatisticsEnabled) statistics.stopTimer(matchesPtNanos, start)
      result
    }
    private def matchesPt(info: ImplicitInfo): Boolean = (
      info.isStablePrefix && matchesPt(info.depoly, wildPt, Nil)
    )

    private def matchesPtView(tp: Type, ptarg: Type, ptres: Type, undet: List[Symbol]): Boolean = tp match {
      case MethodType(p :: _, restpe) if p.isImplicit => matchesPtView(restpe, ptarg, ptres, undet)
      case MethodType(p :: Nil, restpe)               => matchesArgRes(p.tpe, restpe, ptarg, ptres, undet)
      case ExistentialType(_, qtpe)                   => matchesPtView(methodToExpressionTp(qtpe), ptarg, ptres, undet)
      case Function1(arg1, res1)                      => matchesArgRes(arg1, res1, ptarg, ptres, undet)
      case _                                          => false
    }

    private def matchesArgRes(tparg: Type, tpres: Type, ptarg: Type, ptres: Type, undet: List[Symbol]): Boolean =
     (ptarg weak_<:< tparg) && {
       ptres match {
         case HasMethodMatching(name, argtpes, restpe) =>
           (tpres.member(name) filter (m =>
             isApplicableSafe(undet, m.tpe, argtpes, restpe))) != NoSymbol
         case _ =>
           tpres <:< ptres
       }
     }

    private def matchesPtInst(info: ImplicitInfo): Boolean = {
      if (settings.areStatisticsEnabled) statistics.incCounter(matchesPtInstCalls)
        info.tpe match {
          case PolyType(tparams, restpe) =>
            try {
              val allUndetparams = (undetParams ++ tparams).distinct
              val tvars = allUndetparams map freshVar
              val tp = ApproximateDependentMap(restpe)
              val tpInstantiated = tp.instantiateTypeParams(allUndetparams, tvars)
              if(!matchesPt(tpInstantiated, wildPt, allUndetparams)) {
                if (settings.areStatisticsEnabled) statistics.incCounter(matchesPtInstMismatch1)
                false
              } else {
                // we can't usefully prune views any further because we would need to type an application
                // of the view to the term as is done in the computation of itree2 in typedImplicit1.
                tvars.foreach(_.constr.stopWideningIfPrecluded())
                val targs = solvedTypes(tvars, allUndetparams, varianceInType(wildPt), upper = false, lubDepth(tpInstantiated :: wildPt :: Nil))
                val adjusted = adjustTypeArgs(allUndetparams, tvars, targs)
                val tpSubst = deriveTypeWithWildcards(adjusted.undetParams)(tp.instantiateTypeParams(adjusted.okParams, adjusted.okArgs))
                if(!matchesPt(tpSubst, wildPt, adjusted.undetParams)) {
                  if (settings.areStatisticsEnabled) statistics.incCounter(matchesPtInstMismatch2)
                  false
                } else true
              }
          } catch {
            case _: NoInstance => false
          }
        case _ => true
      }
    }

    /** Capturing the overlap between isPlausiblyCompatible and normSubType.
     *  This is a faithful translation of the code which was there, but it
     *  seems likely the methods are intended to be even more similar than
     *  they are: perhaps someone more familiar with the intentional distinctions
     *  can examine the now much smaller concrete implementations below.
     */
    private def checkCompatibility(fast: Boolean, tp0: Type, pt0: Type): Boolean = {
      @tailrec def loop(tp: Type, pt: Type): Boolean = tp match {
        case mt @ MethodType(params, restpe) =>
          if (mt.isImplicit)
            loop(restpe, pt)
          else pt match {
            case tr @ TypeRef(pre, sym, args) =>
              if (sym.isAliasType) loop(tp, pt.dealias)
              else if (sym.isAbstractType) loop(tp, pt.lowerBound)
              else {
                val ptFunctionArity = functionArity(pt)
                ptFunctionArity > 0 && hasLength(params, ptFunctionArity) && {
                  var ps = params
                  var as = args
                  if (fast) {
                    while (!(ps.isEmpty || as.isEmpty)) {
                      if (!isPlausiblySubType(as.head, ps.head.tpe))
                        return false
                      ps = ps.tail
                      as = as.tail
                    }
                  } else {
                    while (!(ps.isEmpty || as.isEmpty)) {
                      if (!(as.head <:< ps.head.tpe))
                        return false
                      ps = ps.tail
                      as = as.tail
                    }
                  }
                  ps.isEmpty && !as.isEmpty && as.tail.isEmpty && loop(restpe, as.head)
                }
              }

            case _            => if (fast) false else tp <:< pt
          }
        case NullaryMethodType(restpe)  => loop(restpe, pt)
        case PolyType(_, restpe)        => loop(restpe, pt)
        case ExistentialType(_, qtpe)   => if (fast) loop(qtpe, pt) else methodToExpressionTp(tp) <:< pt // is !fast case needed??
        case _                          => (if (fast) isPlausiblySubType(tp, pt) else tp <:< pt) && {
          pt match {
            case RefinedType(_, syms) if !syms.isEmpty =>
              syms.reverseIterator.exists(x => context.isAccessible(tp.nonPrivateMember(x.name), tp))
            case _ =>
              true
          }
        }
      }
      loop(tp0, pt0)
    }

    @annotation.unused
    private def isImpossibleSubType(tp1: Type, tp2: Type): Boolean = !isPlausiblySubType(tp1, tp2)
    private def isPlausiblySubType(tp1: Type, tp2: Type): Boolean =
      tp1.dealiasWiden match {
        // We only know enough to rule out a subtype relationship if the left hand side is a class.
        case tr1@TypeRef(_, sym1, args1) if sym1.isClass =>
          val tp2Wide =
            tp2.dealiasWiden.upperBound match {
              case et: ExistentialType => et.underlying // OPT meant as cheap approximation of skolemizeExistential?
              case tp                  => tp
            }
          tp2Wide match {
            case TypeRef(_, sym2, args2) if sym2 ne SingletonClass =>
              // The order of these two checks can be material for performance (scala/bug#8478)
              def isSubArg(tparam: Symbol, t1: Type, t2: Type) =
                (!tparam.isContravariant || isPlausiblySubType(t2, t1)) &&
                (!tparam.isCovariant || isPlausiblySubType(t1, t2))

              if ((sym1 eq sym2) && (args1 ne Nil)) corresponds3(sym1.typeParams, args1, args2)(isSubArg)
              else (sym1 eq ByNameParamClass) == (sym2 eq ByNameParamClass) && (!sym2.isClass || (sym1 isWeakSubClass sym2))
            case RefinedType(parents, decls)                       =>
              // OPT avoid full call to .member
              decls.isEmpty || {
                // Do any of the base classes of the class on the left declare the first member in the refinement on the right?
                // (We randomly pick the first member as a good candidate for eliminating this subtyping pair.)
                val firstDeclName = decls.head.name
                tr1.baseClasses.exists(_.info.decls.lookupEntry(firstDeclName) != null)
              }

            case _ => true
          }
        case _ => true
      }

    private def typedImplicit0(info: ImplicitInfo, ptChecked: Boolean, isLocalToCallsite: Boolean): SearchResult = {
      if (settings.areStatisticsEnabled) statistics.incCounter(plausiblyCompatibleImplicits)
      val ok = ptChecked || matchesPt(info) && {
        def word = if (isLocalToCallsite) "local " else ""
        typingLog("match", s"$word$info")
        true
      }
      if (ok) typedImplicit1(info, isLocalToCallsite) else SearchFailure
    }

    private def typedImplicit1(info: ImplicitInfo, isLocalToCallsite: Boolean): SearchResult = {
      if (settings.areStatisticsEnabled) statistics.incCounter(matchingImplicits)

      // workaround for deficient context provided by ModelFactoryImplicitSupport#makeImplicitConstraints
      val isScaladoc = context.tree == EmptyTree

      val itree0 = atPos(pos.focus) {
        if (isLocalToCallsite && !isScaladoc) {
          // scala/bug#4270 scala/bug#5376 Always use an unattributed Ident for implicits in the local scope,
          // rather than an attributed Select, to detect shadowing.
          Ident(info.name)
        } else {
          assert(info.pre != NoPrefix, info)
          // scala/bug#2405 Not info.name, which might be an aliased import
          val implicitMemberName = info.sym.name
          Select(gen.mkAttributedQualifier(info.pre), implicitMemberName)
        }
      }
      val itree1 = if (isBlackbox(info.sym)) suppressMacroExpansion(itree0) else itree0
      typingLog("considering", typeDebug.ptTree(itree1))

      @inline def fail(reason: => String): SearchResult = failure(itree0, reason)
      def fallback = typed1(itree1, EXPRmode, wildPt)
      try {
        // try to infer implicit parameters immediately in order to:
        //   1) guide type inference for implicit views
        //   2) discard ineligible views right away instead of risking spurious ambiguous implicits
        //
        // this is an improvement of the state of the art that brings consistency to implicit resolution rules
        // (and also helps fundep materialization to be applicable to implicit views)
        //
        // there's one caveat though. we need to turn this behavior off for scaladoc
        // because scaladoc usually doesn't know the entire story
        // and is just interested in views that are potentially applicable
        // for instance, if we have `class C[T]` and `implicit def conv[T: Numeric](c: C[T]) = ???`
        // then Scaladoc will give us something of type `C[T]`, and it would like to know
        // that `conv` is potentially available under such and such conditions
        val itree2 = if (!isView) fallback else pt match {
          case Function1(arg1, arg2) =>
            val applied = typed1(
              atPos(itree0.pos)(Apply(itree1, Ident(nme.argument).setType(approximate(arg1)) :: Nil)),
              EXPRmode,
              approximate(arg2)
            )
            if (isImplicitMethodType(applied.tpe) && !isScaladoc) applyImplicitArgs(applied) else applied
          case _ => fallback
        }
        context.reporter.firstError match { // using match rather than foreach to avoid non local return.
          case Some(err) =>
            log("implicit adapt failed: " + err.errMsg)
            return fail(err.errMsg)
          case None      =>
        }

        if (settings.areStatisticsEnabled) statistics.incCounter(typedImplicits)

        val itree3 = if (isView) treeInfo.dissectApplied(itree2).callee
                     else adapt(itree2, EXPRmode, wildPt)

        typingStack.showAdapt(itree0, itree3, pt, context)

        def hasMatchingSymbol(tree: Tree): Boolean = (tree.symbol == info.sym) || {
          tree match {
            case Apply(fun, _)          => hasMatchingSymbol(fun)
            case TypeApply(fun, _)      => hasMatchingSymbol(fun)
            case Select(pre, nme.apply) => pre.symbol == info.sym
            case _                      => false
          }
        }

        if (context.reporter.hasErrors)
          fail("hasMatchingSymbol reported error: " + context.reporter.firstError.get.errMsg)
        else if (itree3.isErroneous)
          fail("error typechecking implicit candidate")
        else if (isLocalToCallsite && !hasMatchingSymbol(itree2))
          fail("candidate implicit %s is shadowed by %s".format(
            info.sym.fullLocationString, itree2.symbol.fullLocationString))
        else {
          val tvars = undetParams map freshVar
          val ptInstantiated = pt.instantiateTypeParams(undetParams, tvars)

          if (matchesPt(itree3.tpe, ptInstantiated, undetParams)) {
            if (tvars.nonEmpty)
              typingLog("solve", ptLine("tvars" -> tvars, "tvars.constr" -> tvars.map(_.constr)))

            val targs = solvedTypes(tvars, undetParams, varianceInType(pt), upper = false, lubDepth(itree3.tpe :: pt :: Nil))

            // #2421: check that we correctly instantiated type parameters outside of the implicit tree:
            checkBounds(itree3, NoPrefix, NoSymbol, undetParams, targs, "inferred ")

            // In case we stepped on a macro along the way, the macro was expanded during the call to adapt. Along the way,
            // any type parameters that were instantiated were NOT yet checked for bounds, so we need to repeat the above
            // bounds check on the expandee tree
            itree3.attachments.get[MacroExpansionAttachment].foreach {
              case MacroExpansionAttachment(exp @ TypeApply(fun, targs), _) =>
                val targTpes     = mapList(targs)(_.tpe)
                val withinBounds = checkBounds(exp, NoPrefix, NoSymbol, fun.symbol.typeParams, targTpes, "inferred ")
                if (!withinBounds) splainPushNonconformantBonds(pt, tree, targTpes, undetParams, None)
              case _ =>
            }

            context.reporter.firstError match {
              case Some(err) =>
                return fail("type parameters weren't correctly instantiated outside of the implicit tree: " + err.errMsg)
              case None      =>
            }

            // filter out failures from type inference, don't want to remove them from undetParams!
            // we must be conservative in leaving type params in undetparams
            // prototype == WildcardType: want to remove all inferred Nothings
            val adjusted = adjustTypeArgs(undetParams, tvars, targs)
            import adjusted.{okParams, okArgs}
            enhanceBounds(okParams, okArgs, undetParams)

            val subst: TreeTypeSubstituter =
              if (okParams.isEmpty) EmptyTreeTypeSubstituter
              else {
                val subst = new TreeTypeSubstituter(okParams, okArgs)
                subst traverse itree3
                notifyUndetparamsInferred(okParams, okArgs)
                subst
              }

            // #2421b: since type inference (which may have been
            // performed during implicit search) does not check whether
            // inferred arguments meet the bounds of the corresponding
            // parameter (see note in solvedTypes), must check again
            // here:
            // TODO: I would prefer to just call typed instead of
            // duplicating the code here, but this is probably a
            // hotspot (and you can't just call typed, need to force
            // re-typecheck)
            //
            // This is just called for the side effect of error detection,
            // see scala/bug#6966 to see what goes wrong if we use the result of this
            // as the SearchResult.
            itree3 match {
              case TypeApply(fun, args)           => typedTypeApply(itree3, EXPRmode, fun, args)
              case Apply(TypeApply(fun, args), _) => typedTypeApply(itree3, EXPRmode, fun, args) // t2421c
              case t                              => t
            }

            context.reporter.firstError match {
              case Some(err) =>
                splainPushImplicitSearchFailure(itree3, pt, err)
                fail("typing TypeApply reported errors for the implicit tree: " + err.errMsg)
              case None      =>
                val result = new SearchResult(unsuppressMacroExpansion(itree3), subst, context.undetparams, inPackagePrefix = info.inPackagePrefix)
                if (settings.areStatisticsEnabled) statistics.incCounter(foundImplicits)
                typingLog("success", s"inferred value of type $ptInstantiated is $result")
                result
            }
          }
          else fail("incompatible: %s does not match expected type %s".format(itree3.tpe, ptInstantiated))
        }
      }
      catch {
        case ex: TypeError =>
          fail(ex.getMessage())
      }
    }

    /** Should implicit definition symbol `sym` be considered for applicability testing?
     *  This is the case if one of the following holds:
     *   - the symbol's type is initialized
     *   - the symbol comes from a classfile
     *   - the symbol comes from a different sourcefile than the current one
     *   - the symbol and the accessed symbol's definitions come before, and do not contain the closest enclosing definition, // see #3373
     *   - the symbol's definition is a val, var, or def with an explicit result type
     *  The aim of this method is to prevent premature cyclic reference errors
     *  by computing the types of only those implicits for which one of these
     *  conditions is true.
     */
    def isValid(sym: Symbol) = {
      def hasExplicitResultType(sym: Symbol) = {
        def hasExplicitRT(tree: Tree) = tree match {
          case x: ValOrDefDef => !x.tpt.isEmpty
          case _              => false
        }
        sym.rawInfo match {
          case tc: TypeCompleter => hasExplicitRT(tc.tree)
          case PolyType(_, tc: TypeCompleter) => hasExplicitRT(tc.tree)
          case _ => true
        }
      }
      def comesBefore(sym: Symbol, owner: Symbol) = {
        val ownerPos = owner.pos.pointOrElse(Int.MaxValue)
        sym.pos.pointOrElse(0) < ownerPos && (
          if (sym.hasAccessorFlag) {
            val symAcc = sym.accessed // #3373
            symAcc.pos.pointOrElse(0) < ownerPos &&
            !(owner.ownerChain exists (o => (o eq sym) || (o eq symAcc))) // probably faster to iterate only once, don't feel like duplicating hasTransOwner for this case
          } else !(owner hasTransOwner sym)) // faster than owner.ownerChain contains sym
      }

      sym.isInitialized || {
        val sourceFile = sym.sourceFile
        sourceFile == null ||
        (sourceFile ne context.unit.source.file) ||
        hasExplicitResultType(sym) ||
        comesBefore(sym, context.owner)
      }
    }

    /** Prune ImplicitInfos down to either all the eligible ones or the best one.
     *
     *  @param  iss                list of list of infos
     *  @param  isLocalToCallsite  if true, `iss` represents in-scope implicits, which must respect the normal rules of
     *                             shadowing. The head of the list `iss` must represent implicits from the closest
     *                             enclosing scope, and so on.
     */
    class ImplicitComputation(iss: Infoss, isLocalToCallsite: Boolean) {
      private var best: SearchResult = SearchFailure

      private def isIneligible(info: ImplicitInfo) = (
           info.isCyclicOrErroneous
        || isView && ((info.sym eq Predef_conforms) || (info.sym eq SubType_refl)) // as implicit conversions, Predef.$conforms and <:<.refl are no-op, so exclude them
        || (!context.macrosEnabled && info.sym.isTermMacro)
      )

      /** True if a given ImplicitInfo (already known isValid) is eligible.
       */
      @nowarn("cat=lint-inaccessible")
      def survives(info: ImplicitInfo, shadower: Shadower) = (
           !isIneligible(info)                      // cyclic, erroneous, shadowed, or specially excluded
        && isPlausiblyCompatible(info.tpe, wildPt)  // optimization to avoid matchesPt
        && !shadower.isShadowed(info.name)          // OPT rare, only check for plausible candidates
        && matchesPt(info)                          // stable and matches expected type
      )
      /** The implicits that are not valid because they come later in the source and
       *  lack an explicit result type. Used for error diagnostics only.
       */
      val invalidImplicits = new ListBuffer[Symbol]

      /** Tests for validity and updates invalidImplicits by side effect when false.
       */
      private def checkValid(sym: Symbol) = isValid(sym) || { invalidImplicits += sym ; false }

      /** Preventing a divergent implicit from terminating implicit search,
       *  so that if there is a best candidate it can still be selected.
       */
      object DivergentImplicitRecovery {
        private var divergentError: Option[DivergentImplicitTypeError] = None

        private def saveDivergent(err: DivergentImplicitTypeError): Unit = {
          if (divergentError.isEmpty) divergentError = Some(err)
        }

        def issueSavedDivergentError(): Unit = {
          divergentError foreach (err => context.issue(err))
        }

        def apply(search: SearchResult, i: ImplicitInfo, errors: Seq[AbsTypeError]): SearchResult = {
          // A divergent error from a nested implicit search will be found in `errors`. Stash that
          // aside to be re-issued if this implicit search fails.
          errors.collectFirst { case err: DivergentImplicitTypeError => err } foreach saveDivergent

          if (search.isDivergent && divergentError.isEmpty) {
            // Divergence triggered by `i` at this level of the implicit search. We haven't
            // seen divergence so far, we won't issue this error just yet, and instead temporarily
            // treat `i` as a failed candidate.
            saveDivergent(DivergentImplicitTypeError(tree, pt, i.sym))
            log(s"discarding divergent implicit ${i.sym} during implicit search")
            SearchFailure
          } else {
            if (search.isFailure) {
              // Discard the divergentError we saved (if any), as well as all errors that are not of type DivergentImplicitTypeError
              // We don't want errors that occur while checking the implicit info
              // to influence the check of further infos, but we should retain divergent implicit errors
              // (except for the one we already squirreled away)
              context.reporter.retainDivergentErrorsExcept(divergentError.getOrElse(null))
            }
            search
          }
        }
      }

      /** Sorted list of eligible implicits.
       */
      private def eligibleOld = Shadower.using(isLocalToCallsite) { shadower =>
        iss flatMap { is =>
          val result = is filter (info => checkValid(info.sym) && survives(info, shadower))
          shadower addInfos is
          result
        }
      }

      /** Sorted list of eligible implicits.
       */
      private def eligibleNew = {
        final case class Candidate(info: ImplicitInfo, level: Int)
        var matches: java.util.ArrayList[Candidate] = null
        var matchesNames: java.util.HashSet[Name] = null

        var maxCandidateLevel = 0

        {
          var i = 0
          // Collect candidates, the level at which each was found and build a set of their names
          var iss = this.iss
          while (!iss.isEmpty) {
            var is = iss.head
            while (!is.isEmpty) {
              val info = is.head
              if (checkValid(info.sym) && survives(info, NoShadower)) {
                if (matches == null) {
                  matches = new java.util.ArrayList(16)
                  matchesNames = new java.util.HashSet(16)
                }
                matches.add(Candidate(info, i))
                matchesNames.add(info.name)
                maxCandidateLevel = i
              }
              is = is.tail
            }
            iss = iss.tail
            i += 1
          }
        }

        if (matches == null)
          Nil // OPT common case: no candidates
        else {
          if (isLocalToCallsite) {
            // A second pass to filter out results that are shadowed by implicits in inner scopes.
            var i = 0
            var removed = false
            var iss = this.iss
            while (!iss.isEmpty && i < maxCandidateLevel) {
              var is = iss.head
              while (!is.isEmpty) {
                val info = is.head
                if (matchesNames.contains(info.name)) {
                  var j = 0
                  val numMatches = matches.size()
                  while (j < numMatches) {
                    val matchInfo = matches.get(j)
                    if (matchInfo != null && matchInfo.info.name == info.name && matchInfo.level > i) {
                      // Shadowed. For now set to null, so as not to mess up the indexing our current loop.
                      matches.set(j, null)
                      removed = true
                    }
                    j += 1
                  }
                }
                is = is.tail
              }
              iss = iss.tail
              i += 1
            }
            if (removed) matches.removeIf(_ == null) // remove for real now.
          }
          val result = new ListBuffer[ImplicitInfo]
          matches.forEach(x => result += x.info)
          result.toList
        }
      }

      val eligible: List[ImplicitInfo] = if (shadowerUseOldImplementation) eligibleOld else eligibleNew
      if (eligible.nonEmpty)
        printTyping(tree, s"${eligible.size} eligible for pt=$pt at ${fullSiteString(context)}")

      /** Faster implicit search.  Overall idea:
       *   - prune aggressively
       *   - find the most likely one
       *   - if it matches, forget about all others it improves upon
       */

      // the pt can have embedded unification type variables, BoundedWildcardTypes or Nothings
      // which can't be solved for. Rather than attempt to patch things up later we just skip
      // those cases altogether.
      lazy val wildPtNotInstantiable =
        wildPt.exists { case _: BoundedWildcardType | _: TypeVar => true ; case tp => tp.isNothing }

      @tailrec private def rankImplicits(pending: Infos, acc: List[(SearchResult, ImplicitInfo)]): List[(SearchResult, ImplicitInfo)] = pending match {
        case Nil                          => acc
        case firstPending :: otherPending =>
          def firstPendingImproves(alt: ImplicitInfo) =
            firstPending == alt || (
              try improves(firstPending, alt)
              catch {
                case e: CyclicReference =>
                  devWarning(s"Discarding $firstPending during implicit search due to cyclic reference.")
                  true
              }
            )

          val mark = undoLog.log
          val savedInfos = undetParams.map(_.info)
          val typedFirstPending = {
            try {
              if (isView || wildPtNotInstantiable || matchesPtInst(firstPending)) {
                val res = typedImplicit(firstPending, ptChecked = true, isLocalToCallsite)
                if (res.isFailure) res
                else
                  new SearchResult(res.tree, res.subst, res.undetparams, res.inPackagePrefix, implicitInfo = firstPending)
              }
              else SearchFailure
            } finally {
              foreach2(undetParams, savedInfos){ (up, si) => up.setInfo(si) }
            }
          }
          if (typedFirstPending.isFailure)
            undoLog.undoTo(mark) // Don't accumulate constraints from typechecking or type error message creation for failed candidates

          // Pass the errors to `DivergentImplicitRecovery` so that it can note
          // the first `DivergentImplicitTypeError` that is being propagated
          // from a nested implicit search; this one will be
          // re-issued if this level of the search fails.
          DivergentImplicitRecovery(typedFirstPending, firstPending, context.reporter.errors) match {
            case sr if sr.isDivergent => Nil
            case sr if sr.isFailure   => rankImplicits(otherPending, acc)
            case newBest              =>
              best = newBest // firstPending is our new best, since we already pruned last time around:
              val pendingImprovingBest = undoLog undo {
                otherPending filterNot firstPendingImproves
              }

              if (pt.typeSymbol.hasAnnotation(definitions.LanguageFeatureAnnot)) (newBest, firstPending):: Nil
              else rankImplicits(pendingImprovingBest, (newBest, firstPending) :: acc)
          }
      }

      /** Returns all eligible ImplicitInfos and their SearchResults in a map.
       */
      def findAll() = linkedMapFrom(eligible)(x => try typedImplicit(x, ptChecked = false, isLocalToCallsite) finally context.reporter.clearAll())

      /** Returns the SearchResult of the best match.
       */
      def findBest(): SearchResult = {
        // After calling rankImplicits, the least frequent matching one is first and
        // earlier elems may improve on later ones, but not the other way.
        // So if there is any element not improved upon by the first it is an error.
        rankImplicits(eligible, Nil) match {
          case Nil            => ()
          case (chosenResult, chosenInfo) :: rest =>
            rest find { case (_, alt) => !improves(chosenInfo, alt) } match {
              case Some((competingResult, competingInfo))  =>
                AmbiguousImplicitError(chosenInfo, chosenResult.tree, competingInfo, competingResult.tree, "both", "and", "")(isView, pt, tree)(context)
                return AmbiguousSearchFailure // Stop the search once ambiguity is encountered, see t4457_2.scala
              case _                =>
                if (isView) chosenInfo.useCountView += 1
                else chosenInfo.useCountArg += 1
            }
        }

        if (best.isFailure) {
          // If there is no winner, and we witnessed and recorded a divergence error,
          // our recovery attempt has failed, so we must now issue it.
          DivergentImplicitRecovery.issueSavedDivergentError()

          if (invalidImplicits.nonEmpty)
            setAddendum(pos, () =>
              s"\n Note: implicit ${invalidImplicits.head} is not applicable here because it comes after the application point and it lacks an explicit result type.${if (invalidImplicits.head.isModule) " An object can be written as a lazy val with an explicit type." else ""}"
            )
        }
        else if (best.implicitInfo != null && best.implicitInfo.importInfo != null)
          best.implicitInfo.importInfo.recordUsage(best.implicitInfo.importSelector, best.tree.symbol)

        best
      }
    }

    /** Computes from a list of lists of implicit infos a map which takes
     *  infos which are applicable for given expected type `pt` to their attributed trees.
     *
     *  @param iss               The given list of lists of implicit infos
     *  @param isLocalToCallsite Is implicit definition visible without prefix?
     *                           If this is the case then symbols in preceding lists shadow
     *                           symbols of the same name in succeeding lists.
     *  @return                  map from infos to search results
     */
    def applicableInfos(iss: Infoss, isLocalToCallsite: Boolean): mutable.LinkedHashMap[ImplicitInfo, SearchResult] = {
      val start       = if (settings.areStatisticsEnabled) statistics.startCounter(subtypeAppInfos) else null
      val computation = new ImplicitComputation(iss, isLocalToCallsite) { }
      val applicable  = computation.findAll()

      if (settings.areStatisticsEnabled) statistics.stopCounter(subtypeAppInfos, start)
      applicable
    }

    /** Search list of implicit info lists for one matching prototype `pt`.
     *  If found return a search result with a tree from found implicit info
     *  which is typed with expected type `pt`. Otherwise return SearchFailure.
     *
     *  @param implicitInfoss    The given list of lists of implicit infos
     *  @param isLocalToCallsite Is implicit definition visible without prefix?
     *                           If this is the case then symbols in preceding lists shadow
     *                           symbols of the same name in succeeding lists.
     */
    def searchImplicit(implicitInfoss: Infoss, isLocalToCallsite: Boolean): SearchResult =
      if (implicitInfoss.forall(_.isEmpty)) SearchFailure
      else new ImplicitComputation(implicitInfoss, isLocalToCallsite).findBest()

    /** Produce an implicit info map, i.e. a map from the class symbols C of all parts of this type to
     *  the implicit infos in the companion objects of these class symbols C.
     * The parts of a type is the smallest set of types that contains
     *    - the type itself
     *    - the parts of its immediate components (prefix and argument)
     *    - the parts of its base types
     *    - for alias types and abstract types, we take instead the parts
     *      of their upper bounds.
     *  @return For those parts that refer to classes with companion objects that
     *  can be accessed with unambiguous stable prefixes that are not existentially
     *  bound, the implicits infos which are members of these companion objects.
     */
    private def companionImplicitMap(tp: Type): InfoMap = {

      /* Populate implicit info map by traversing all parts of type `tp`.
       * Parameters as for `getParts`.
       */
      def getClassParts(tp: Type)(implicit infoMap: InfoMap, seen: mutable.HashSet[Type], pending: Set[Symbol]) = tp match {
        case TypeRef(pre, sym, _) =>
          val symInfos = infoMap.getOrElse(sym, Nil)
          if(!symInfos.exists(pre =:= _.pre.prefix)) {
            if (symInfos.exists(_.isSearchedPrefix))
              infoMap(sym) = SearchedPrefixImplicitInfo(pre) :: symInfos
            else if (pre.isStable && !pre.typeSymbol.isExistentiallyBound) {
              val (pre1, inPackagePrefix) =
                if (sym.isPackageClass) (sym.packageObject.typeOfThis, true)
                else (singleType(pre, companionSymbolOf(sym, context)), false)
              val preInfos = {
                if (currentRun.sourceFeatures.packagePrefixImplicits && inPackagePrefix) Iterator.empty
                else pre1.implicitMembers.iterator.map(mem => new ImplicitInfo(mem.name, pre1, mem, inPackagePrefix = inPackagePrefix))
              }
              val mergedInfos = if (symInfos.isEmpty) preInfos else {
                if (shouldLogAtThisPhase && symInfos.exists(!_.dependsOnPrefix)) log {
                  val nonDepInfos = symInfos.iterator.filterNot(_.dependsOnPrefix).mkString("(", ", ", ")")
                  val prefix = symInfos.head.pre.prefix
                  s"Implicit members $nonDepInfos of $pre#$sym which are also visible via another prefix: $prefix"
                }

                (symInfos.iterator ++ preInfos).filter(_.dependsOnPrefix)
              }

              if (mergedInfos.hasNext)
                infoMap(sym) = mergedInfos.toList
              else
                infoMap(sym) = List(SearchedPrefixImplicitInfo(pre))
            }
            // Only strip annotations on the infrequent path
            val bts = (if (symInfos.isEmpty) tp else tp.map(_.withoutAnnotations)).baseTypeSeq
            var i = 1
            while (i < bts.length) {
              getParts(bts(i))
              i += 1
            }
            getParts(pre)
          }
        case x => throw new MatchError(x)
      }

      /* Populate implicit info map by traversing all parts of type `tp`.
       * This method is performance critical.
       * @param tp   The type for which we want to traverse parts
       * @param infoMap  The infoMap in which implicit infos corresponding to parts are stored
       * @param seen     The types that were already visited previously when collecting parts for the given infoMap
       * @param pending  The set of static symbols for which we are currently trying to collect their parts
       *                 in order to cache them in infoMapCache
       */
      def getParts(tp: Type)(implicit infoMap: InfoMap, seen: mutable.HashSet[Type], pending: Set[Symbol]): Unit = {
        if (seen add tp) tp match {
          case TypeRef(pre, sym, args) =>
            if (sym.isClass && !sym.isRoot) {
              if (sym.isStatic && !pending.contains(sym))
                infoMap ++= infoMapCache.getOrElseUpdate(sym, {
                  val result = new InfoMap
                  getClassParts(sym.tpeHK)(result, new mutable.HashSet, pending + sym)
                  result
                })
              else
                getClassParts(tp)
              args.foreach(getParts)
            } else if (sym.isAliasType) {
              getParts(tp.normalize) // scala/bug#7180 Normalize needed to expand HK type refs
            } else if (sym.isAbstractType) {
              // SLS 2.12, section 7.2:
              //  - if `T` is an abstract type, the parts of its upper bound;
              getParts(tp.upperBound)
              //  - if `T` is a parameterized type `S[T1,…,Tn]`, the union of the parts of `S` and `T1,…,Tn`
              args.foreach(getParts)
              //  - if `T` is a type projection `S#U`, the parts of `S` as well as `T` itself;
              getParts(pre)
            }
          case ThisType(_) =>
            getParts(tp.widen)
          case _: SingletonType =>
            getParts(tp.widen)
          case HasMethodMatching(_, argtpes, restpe) =>
            for (tp <- argtpes) getParts(tp)
            getParts(restpe)
          case RefinedType(ps, _) =>
            for (p <- ps) getParts(p)
          case AnnotatedType(_, t) =>
            getParts(t)
          case ExistentialType(_, t) =>
            getParts(t)
          case PolyType(_, t) =>
            getParts(t)
          // not needed, a view's expected type is normalized in typer by normalizeProtoForView:
          // case proto: OverloadedArgProto => getParts(proto.underlying)
          case _ =>
        }
      }

      val infoMap = new InfoMap
      getParts(tp)(infoMap, new mutable.HashSet(), Set())
      val emptyInfos = mutable.ArrayBuffer[Symbol]()
      infoMap.foreachEntry { (k, v) =>
        if (v.exists(_.isSearchedPrefix))
          emptyInfos.addOne(k)
      }
      emptyInfos.foreach(infoMap.remove)
      if (infoMap.nonEmpty)
        printTyping(tree, "" + infoMap.size + " implicits in companion scope")

      infoMap
    }

    /** The implicits made available by type `pt`.
     *  These are all implicits found in companion objects of classes C
     *  such that some part of `tp` has C as one of its superclasses.
     */
    private def implicitsOfExpectedType: Infoss = {
      if (settings.areStatisticsEnabled) statistics.incCounter(implicitCacheAccs)
      implicitsCache get pt match {
        case Some(implicitInfoss) =>
          if (settings.areStatisticsEnabled) statistics.incCounter(implicitCacheHits)
          implicitInfoss
        case None =>
          val start = if (settings.areStatisticsEnabled) statistics.startTimer(subtypeETNanos) else null
          //        val implicitInfoss = companionImplicits(pt)
          val implicitInfoss1 = companionImplicitMap(pt).valuesIterator.toList
          //        val is1 = implicitInfoss.flatten.toSet
          //        val is2 = implicitInfoss1.flatten.toSet
          //        for (i <- is1)
          //          if (!(is2 contains i)) println("!!! implicit infos of "+pt+" differ, new does not contain "+i+",\nold: "+implicitInfoss+",\nnew: "+implicitInfoss1)
          //        for (i <- is2)
          //          if (!(is1 contains i)) println("!!! implicit infos of "+pt+" differ, old does not contain "+i+",\nold: "+implicitInfoss+",\nnew: "+implicitInfoss1)
          if (settings.areStatisticsEnabled) statistics.stopTimer(subtypeETNanos, start)
          implicitsCache(pt) = implicitInfoss1
          if (implicitsCache.size >= sizeLimit)
            implicitsCache -= implicitsCache.keysIterator.next()
          implicitInfoss1
      }
    }

    /** Creates a tree will produce a tag of the requested flavor.
      * An EmptyTree is returned if materialization fails.
      */
    private def tagOfType(pre: Type, tp: Type, tagClass: Symbol): SearchResult = {
      def success(arg: Tree) = {
        def isMacroException(msg: String): Boolean =
          // [Eugene] very unreliable, ask Hubert about a better way
          msg contains "exception during macro expansion"

        def processMacroExpansionError(pos: Position, msg: String): SearchResult = {
          // giving up and reporting all macro exceptions regardless of their source
          // this might lead to an avalanche of errors if one of your implicit macros misbehaves
          if (isMacroException(msg)) context.error(pos, msg)
          failure(arg, "failed to typecheck the materialized tag: %n%s".format(msg), pos)
        }

        try {
          val tree1 = typedPos(pos.focus)(arg)
          context.reporter.firstError match {
            case Some(err) => processMacroExpansionError(err.errPos, err.errMsg)
            case None      => new SearchResult(tree1, EmptyTreeTypeSubstituter, Nil)
          }
        } catch {
          case ex: TypeError =>
            processMacroExpansionError(ex.pos, ex.msg)
        }
      }

      val prefix = (
        // ClassTags are not path-dependent, so their materializer doesn't care about prefixes
        if (tagClass eq ClassTagClass) EmptyTree
        else pre match {
          case SingleType(prePre, preSym) =>
            gen.mkAttributedRef(prePre, preSym) setType pre
          // necessary only to compile typetags used inside the Universe cake
          case ThisType(thisSym) =>
            gen.mkAttributedThis(thisSym)
          case _ =>
            // if `pre` is not a PDT, e.g. if someone wrote
            //   implicitly[scala.reflect.macros.blackbox.Context#TypeTag[Int]]
            // then we need to fail, because we don't know the prefix to use during type reification
            // upd. we also need to fail silently, because this is a very common situation
            // e.g. quite often we're searching for BaseUniverse#TypeTag, e.g. for a type tag in any universe
            // so that if we find one, we could convert it to whatever universe we need by the means of the `in` method
            // if no tag is found in scope, we end up here, where we ask someone to materialize the tag for us
            // however, since the original search was about a tag with no particular prefix, we cannot proceed
            // this situation happens very often, so emitting an error message here (even if only for -Vimplicits) would be too much
            //return failure(tp, "tag error: unsupported prefix type %s (%s)".format(pre, pre.kind))
            return SearchFailure
        }
      )
      // todo. migrate hardcoded materialization in Implicits to corresponding implicit macros
      val materializer = atPos(pos.focus)(gen.mkMethodCall(TagMaterializers(tagClass), List(tp), if (prefix != EmptyTree) List(prefix) else List()))
      if (settings.debug.value) reporter.echo(pos, "materializing requested %s.%s[%s] using %s".format(pre, tagClass.name, tp, materializer))
      if (context.macrosEnabled) success(materializer)
      // don't call `failure` here. if macros are disabled, we just fail silently
      // otherwise -Vimplicits/-Vdebug will spam the long with zillions of "macros are disabled"
      // this is ugly but temporary, since all this code will be removed once I fix implicit macros
      else SearchFailure
    }

    /** Creates a tree that calls the relevant factory method in object
      * scala.reflect.Manifest for type 'tp'. An EmptyTree is returned if
      * no manifest is found. todo: make this instantiate take type params as well?
      */
    private def manifestOfType(tp: Type, flavor: Symbol): SearchResult = {
      val full = flavor == FullManifestClass
      val opt = flavor == OptManifestClass

      /* Creates a tree that calls the factory method called constructor in object scala.reflect.Manifest */
      def manifestFactoryCall(constructor: String, tparg: Type, args: Tree*): Tree =
        if (args contains EmptyTree) EmptyTree
        else typedPos(tree.pos.focus) {
          val mani = gen.mkManifestFactoryCall(full, constructor, tparg, args.toList)
          if (settings.isDebug) println("generated manifest: "+mani) // DEBUG
          mani
        }

      /* Creates a tree representing one of the singleton manifests.*/
      def findSingletonManifest(name: String) = typedPos(tree.pos.focus) {
        Select(gen.mkAttributedRef(FullManifestModule), name)
      }

      /* Re-wraps a type in a manifest before calling inferImplicit on the result */
      def findManifest(tp: Type, manifestClass: Symbol = if (full) FullManifestClass else PartialManifestClass) =
        inferImplicitFor(appliedType(manifestClass, tp :: Nil), tree, context).tree

      def findSubManifest(tp: Type) = findManifest(tp, if (full) FullManifestClass else OptManifestClass)
      def mot(tp0: Type, from: List[Symbol], to: List[Type]): SearchResult = {
        implicit def wrapResult(tree: Tree): SearchResult =
          if (tree == EmptyTree) SearchFailure else new SearchResult(tree, if (from.isEmpty) EmptyTreeTypeSubstituter else new TreeTypeSubstituter(from, to), Nil)

        val tp1 = tp0.dealias
        tp1 match {
          case ThisType(_) | SingleType(_, _) =>
            // can't generate a reference to a value that's abstracted over by an existential
            if (containsExistential(tp1)) EmptyTree
            else manifestFactoryCall("singleType", tp, gen.mkAttributedQualifier(tp1))
          case ConstantType(value) =>
            manifestOfType(tp1.deconst, FullManifestClass)
          case TypeRef(pre, sym, args) =>
            if (isPrimitiveValueClass(sym) || isPhantomClass(sym)) {
              findSingletonManifest(sym.name.toString)
            } else if (sym == ObjectClass || sym == AnyRefClass) {
              findSingletonManifest("Object")
            } else if (sym == RepeatedParamClass || sym == ByNameParamClass) {
              EmptyTree
            } else if (sym == ArrayClass && args.length == 1) {
              manifestFactoryCall("arrayType", args.head, findManifest(args.head))
            } else if (sym.isClass) {
              val classarg0 = gen.mkClassOf(tp1)
              val classarg = tp.dealias match {
                case _: ExistentialType => gen.mkCast(classarg0, ClassType(tp))
                case _                  => classarg0
              }
              val suffix = classarg :: (args map findSubManifest)
              manifestFactoryCall(
                "classType", tp,
                (if ((pre eq NoPrefix) || pre.typeSymbol.isStaticOwner) suffix
                 else findSubManifest(pre) :: suffix): _*)
            } else if (sym.isExistentiallyBound && full) {
              manifestFactoryCall("wildcardType", tp,
                                  findManifest(tp.lowerBound), findManifest(tp.upperBound))
            }
            // looking for a manifest of a type parameter that hasn't been inferred by now,
            // can't do much, but let's not fail
            else if (undetParams contains sym) {
              // #3859: need to include the mapping from sym -> NothingTpe in the SearchResult
              mot(NothingTpe, sym :: from, NothingTpe :: to)
            } else {
              // a manifest should have been found by normal searchImplicit
              EmptyTree
            }
          case RefinedType(parents, decls) => // !!! not yet: if !full || decls.isEmpty =>
            // refinement is not generated yet
            if (hasLength(parents, 1)) findManifest(parents.head)
            else if (full) manifestFactoryCall("intersectionType", tp, parents map findSubManifest: _*)
            else mot(erasure.intersectionDominator(parents), from, to)
          case ExistentialType(tparams, result) =>
            mot(tp1.skolemizeExistential, from, to)
          case _ =>
            EmptyTree
          }
      }

      if (full) {
        val tagInScope = resolveTypeTag(pos, NoType, tp, concrete = true, allowMaterialization = false)
        if (tagInScope.isEmpty) mot(tp, Nil, Nil)
        else {
          if (ReflectRuntimeUniverse == NoSymbol) {
            // TODO: write a test for this (the next error message is already checked by neg/interop_typetags_without_classtags_arenot_manifests.scala)
            // TODO: this was using context.error, and implicit search always runs in silent mode, thus it was actually throwing a TypeError
            // with the new strategy-based reporting, a BufferingReporter buffers instead of throwing
            // it would be good to rework this logic to fit into the regular context.error mechanism
            throw new TypeError(pos,
              sm"""to create a manifest here, it is necessary to interoperate with the type tag `$tagInScope` in scope.
                  |however typetag -> manifest conversion requires Scala reflection, which is not present on the classpath.
                  |to proceed put scala-reflect.jar on your compilation classpath and recompile.""")
          }
          if (resolveClassTag(pos, tp, allowMaterialization = true) == EmptyTree) {
            throw new TypeError(pos,
              sm"""to create a manifest here, it is necessary to interoperate with the type tag `$tagInScope` in scope.
                  |however typetag -> manifest conversion requires a class tag for the corresponding type to be present.
                  |to proceed add a class tag to the type `$tp` (e.g. by introducing a context bound) and recompile.""")
          }
          val cm = typed(Ident(ReflectRuntimeCurrentMirror))
          val internal = gen.mkAttributedSelect(gen.mkAttributedRef(ReflectRuntimeUniverse), UniverseInternal)
          val interop = gen.mkMethodCall(Select(internal, nme.typeTagToManifest), List(tp), List(cm, tagInScope))
          wrapResult(interop)
        }
      } else {
        mot(tp, Nil, Nil) match {
          case SearchFailure if opt => wrapResult(gen.mkAttributedRef(NoManifest))
          case result               => result
        }
      }
    }

    /** Creates a tree that will produce a ValueOf instance for the requested type.
      * An EmptyTree is returned if materialization fails.
      */
    private def valueOfType(tp: Type): SearchResult = {
      def success(t: Tree) = wrapResult(Apply(Select(New(gen.scalaDot(tpnme.ValueOf)), nme.CONSTRUCTOR), List(t)))

      tp.dealias match {
        case ConstantType(c: Constant) => success(Literal(c))
        case SingleType(p, v) => success(gen.mkAttributedRef(p, v) setType tp)
        case ThisType(sym) => success(gen.mkAttributedThis(sym) setType tp)
        case UnitTpe => success(Literal(Constant(())))
        case TypeRef(pre, sym, Nil) if sym.isModuleClass => success(gen.mkAttributedRef(pre, sym.sourceModule) setType tp)
        case _ => SearchFailure
      }
    }

    def wrapResult(tree: Tree): SearchResult =
      if (tree == EmptyTree) SearchFailure else new SearchResult(atPos(pos.focus)(tree), EmptyTreeTypeSubstituter, Nil)

    /** Materializes implicits of predefined types (currently, manifests and tags).
     *  Will be replaced by implicit macros once we fix them.
     */
    private def materializeImplicit(pt: Type): SearchResult =
      pt match {
        case TypeRef(_, sym, _) if sym.isAbstractType =>
          materializeImplicit(pt.dealias.lowerBound) // #3977: use pt.dealias, not pt (if pt is a type alias, pt.lowerBound == pt)
        case pt @ TypeRef(pre, sym, arg :: Nil) =>
          // TODO: is there any way in which an OverloadedArgProto could sneak into one of these special expected types for implicit search?
          // As the outer expected type, it's normalized in typer by normalizeProtoForView
          sym match {
            case sym if ManifestSymbols(sym) => manifestOfType(arg, sym)
            case sym if TagSymbols(sym) => tagOfType(pre, arg, sym)
            case ValueOfClass => valueOfType(arg)
            // as of late ClassManifest is an alias of ClassTag
            // hence we need to take extra care when performing dealiasing
            // because it might destroy the flavor of the manifest requested by the user
            // when the user wants ClassManifest[T], we should invoke `manifestOfType` not `tagOfType`
            // hence we don't do `pt.dealias` as we did before, but rather do `pt.betaReduce`
            // unlike `dealias`, `betaReduce` performs at most one step of dealiasing
            // while dealias pops all aliases in a single invocation
            case sym if sym.isAliasType => materializeImplicit(pt.betaReduce)
            case _ => SearchFailure
          }
        case _ =>
          SearchFailure
      }

    /** The result of the implicit search:
     *  First search implicits visible in current context.
     *  If that fails, search implicits in expected type `pt`.
     *
     *  todo. the following lines should be deleted after we migrate delegate tag materialization to implicit macros
     *  If that fails, and `pt` is an instance of a ClassTag, try to construct a class tag.
     *  If that fails, and `pt` is an instance of a TypeTag, try to construct a type tag.
     *  If that fails, and `pt` is an instance of a ClassManifest, try to construct a class manifest.
     *  If that fails, and `pt` is an instance of a Manifest, try to construct a manifest.
     *  If that fails, and `pt` is an instance of a OptManifest, try to construct a class manifest and return NoManifest if construction fails.
     *  If all fails return SearchFailure
     */
    def bestImplicit: SearchResult = {
      val stats = settings.areStatisticsEnabled
      val failstart = if (stats) statistics.startTimer(inscopeFailNanos) else null
      val succstart = if (stats) statistics.startTimer(inscopeSucceedNanos) else null

      var result = searchImplicit(context.implicitss, isLocalToCallsite = true)

      if (stats) {
        if (result.isFailure) statistics.stopTimer(inscopeFailNanos, failstart)
        else {
          statistics.stopTimer(inscopeSucceedNanos, succstart)
          statistics.incCounter(inscopeImplicitHits)
        }
      }

      if (result.isFailure) {
        val failstart = if (stats) statistics.startTimer(oftypeFailNanos) else null
        val succstart = if (stats) statistics.startTimer(oftypeSucceedNanos) else null

        // scala/bug#6667, never search companions after an ambiguous error in in-scope implicits
        val wasAmbiguous = result.isAmbiguousFailure

        // TODO: encapsulate
        val previousErrs = context.reporter.errors
        context.reporter.clearAllErrors()

        result = materializeImplicit(pt)

        // `materializeImplicit` does some preprocessing for `pt`
        // is it only meant for manifests/tags or we need to do the same for `implicitsOfExpectedType`?
        if (result.isFailure && !wasAmbiguous)
          result = searchImplicit(implicitsOfExpectedType, isLocalToCallsite = false)

        if (result.isFailure)
          context.reporter ++= previousErrs

        if (stats) {
          if (result.isFailure) statistics.stopTimer(oftypeFailNanos, failstart)
          else {
            statistics.stopTimer(oftypeSucceedNanos, succstart)
            statistics.incCounter(oftypeImplicitHits)
          }
        }
      }
      if (result.isSuccess && isView) {
        def maybeInvalidConversionError(msg: String): Boolean = {
          // We have to check context.ambiguousErrors even though we are calling "issueAmbiguousError"
          // which ostensibly does exactly that before issuing the error. Why? I have no idea. Test is pos/t7690.
          // AM: I would guess it's because ambiguous errors will be buffered in silent mode if they are not reported
          if (context.ambiguousErrors)
            context.issueAmbiguousError(AmbiguousImplicitTypeError(tree, msg))
          true
        }
        pt match {
          // scala/bug#10206 don't use subtyping to rule out AnyRef/AnyVal:
          //   - there are several valid structural types that are supertypes of AnyRef (e.g., created by HasMember);
          //     typeSymbol will do the trick (AnyRef is a type alias for Object), while ruling out these structural types
          //   - also don't want to accidentally constrain type vars through using <:<
          case Function1(in, out) =>
            val outSym = out.typeSymbol

            val fail =
              if (out.annotations.isEmpty && (outSym == ObjectClass || outSym == AnyValClass))
                maybeInvalidConversionError(s"the result type of an implicit conversion must be more specific than $out")
              else if (in.annotations.isEmpty && in.typeSymbol == NullClass)
                maybeInvalidConversionError("an expression of type Null is ineligible for implicit conversion")
              else false

            if (fail) result = SearchFailure

          case _ =>
        }
      }

      if (result.isFailure && settings.isDebug) // debuglog is not inlined for some reason
        log(s"no implicits found for ${pt} ${pt.typeSymbol.info.baseClasses} ${implicitsOfExpectedType}")

      result
    }

    def allImplicits: List[SearchResult] = {
      def search(iss: Infoss, isLocalToCallsite: Boolean) = applicableInfos(iss, isLocalToCallsite).values
      (
        search(context.implicitss, isLocalToCallsite = true) ++
        search(implicitsOfExpectedType, isLocalToCallsite = false)
      ).toList.filter(_.tree ne EmptyTree)
    }

    // find all implicits for some type that contains type variables
    // collect the constraints that result from typing each implicit
    def allImplicitsPoly(tvars: List[TypeVar]): List[(SearchResult, List[TypeConstraint])] = {
      def resetTVars() = tvars foreach { _.constr = new TypeConstraint }

      def eligibleInfos(iss: Infoss, isLocalToCallsite: Boolean) = {
        val eligible = new ImplicitComputation(iss, isLocalToCallsite).eligible
        eligible.toList.flatMap {
          (ii: ImplicitInfo) =>
          // each ImplicitInfo contributes a distinct set of constraints (generated indirectly by typedImplicit)
          // thus, start each type var off with a fresh for every typedImplicit
          resetTVars()
          // any previous errors should not affect us now
          context.reporter.clearAllErrors()
          val res = typedImplicit(ii, ptChecked = false, isLocalToCallsite)
          if (res.tree ne EmptyTree) List((res, tvars map (_.constr)))
          else Nil
        }
      }
      eligibleInfos(context.implicitss, isLocalToCallsite = true) ++
      eligibleInfos(implicitsOfExpectedType, isLocalToCallsite = false)
    }
  }

  class ImplicitAnnotationMsg(f: Symbol => Option[String], clazz: Symbol, annotationName: String) {
    def unapply(sym: Symbol): Option[Message] = f(sym) match {
      case Some(m) => Some(new Message(sym, m, annotationName))
      case None if sym.isAliasType =>
        // perform exactly one step of dealiasing
        // this is necessary because ClassManifests are now aliased to ClassTags
        // but we don't want to intimidate users by showing unrelated error messages
        unapply(sym.info.resultType.betaReduce.typeSymbolDirect)
      case _ => None
    }

    // check the message's syntax: should be a string literal that may contain occurrences of the string "${X}",
    // where `X` refers to a type parameter of `sym`
    def check(sym: Symbol): Option[String] =
      sym.getAnnotation(clazz).flatMap(_.stringArg(0) match {
        case Some(m) => new Message(sym, m, annotationName).validate
        case None => Some(s"Missing argument `msg` on $annotationName annotation.")
      })
  }

  object ImplicitNotFoundMsg extends ImplicitAnnotationMsg(_.implicitNotFoundMsg, ImplicitNotFoundClass, "implicitNotFound")

  object ImplicitAmbiguousMsg extends ImplicitAnnotationMsg(_.implicitAmbiguousMsg, ImplicitAmbiguousClass, "implicitAmbiguous")

  class Message(sym: Symbol, msg: String, annotationName: String) {
    import scala.util.matching.Regex.{quoteReplacement, Groups}
    // https://dcsobral.blogspot.com/2010/01/string-interpolation-in-scala-with.html
    private val Intersobralator = """\$\{\s*([^}\s]+)\s*\}""".r

    private def interpolate(text: String, vars: Map[String, String]) =
      Intersobralator.replaceAllIn(text, (_: Match) match {
        case Groups(v) => quoteReplacement(vars.getOrElse(v, ""))
          // #3915: need to quote replacement string since it may include $'s (such as the interpreter's $iw)
        case x         => throw new MatchError(x)
      })

    def referencedTypeParams: List[String] = Intersobralator.findAllMatchIn(msg).map(_.group(1)).distinct.toList

    private def symTypeParamNames: List[String] = sym.typeParams.map(_.decodedName)

    def lookupTypeParam(name: String): Symbol = {
      val n = newTypeName(name)
      var r: Symbol = NoSymbol
      var o = sym.owner
      while (r == NoSymbol && o != NoSymbol) {
        o.typeParams.find(_.name == n) match {
          case Some(p) => r = p
          case _ =>
            do { o = o.owner } while (!(o.isClass || o.isMethod || o == NoSymbol))
        }
      }
      r
    }

    private def typeArgsAtSym(paramTp: Type) = paramTp.baseType(sym).typeArgs

    def formatDefSiteMessage(paramTp: Type): String =
      formatDefSiteMessage(typeArgsAtSym(paramTp).map(_.toString))

    def formatDefSiteMessage(typeArgs: List[String]): String =
      interpolate(msg, Map(symTypeParamNames.zip(typeArgs): _*))

    def formatParameterMessage(fun: Tree): String = {
      val paramNames = referencedTypeParams
      val paramSyms = paramNames.map(lookupTypeParam).filterNot(_ == NoSymbol)
      val paramTypeRefs = paramSyms.map(_.typeConstructor.etaExpand) // make polytypes for type constructors -- we'll abbreviate them below
      val prefix = fun match {
        case treeInfo.Applied(Select(qual, _), _, _) => qual.tpe
        case _ => NoType
      }

      val argTypes1 = if (prefix == NoType) paramTypeRefs else paramTypeRefs.map(_.asSeenFrom(prefix, fun.symbol.owner))
      val argTypes2 = fun match {
        case treeInfo.Applied(_, targs, _) => argTypes1.map(_.instantiateTypeParams(fun.symbol.info.typeParams, targs.map(_.tpe)))
        case _ => argTypes1
      }

      val argTypes = argTypes2.map {
        case PolyType(tps, tr@TypeRef(_, _, tprefs)) =>
          if (tps.corresponds(tprefs)((p, r) => p == r.typeSymbol)) tr.typeConstructor.toString
          else {
            val freshTpars = tps.mapConserve { p =>
              if (p.unexpandedName == tpnme.WILDCARD) p.cloneSymbol.setName(newTypeName("?T" + tps.indexOf(p)))
              else p
            }
            freshTpars.map(_.name).mkString("[", ", ", "] -> ") + tr.instantiateTypeParams(tps, freshTpars.map(_.typeConstructor)).toString
          }
        case tp => tp.toString
      }
      interpolate(msg, Map(paramNames.zip(argTypes): _*))
    }

    def validate: Option[String] = {
      val refs  = referencedTypeParams
      val isMessageOnParameter = sym.isParameter
      val decls =
        if (isMessageOnParameter) referencedTypeParams.filterNot(p => lookupTypeParam(p) == NoSymbol)
        else symTypeParamNames.distinct

      refs.diff(decls) match {
        case s if s.isEmpty => None
        case unboundNames   =>
          val singular = unboundNames.size == 1
          val ess      = if (singular) "" else "s"
          val bee      = if (singular) "is" else "are"
          val where    = if (isMessageOnParameter) s"in scope" else s"defined by $sym"
          Some(s"The type parameter$ess ${unboundNames mkString ", "} referenced in the message of the @$annotationName annotation $bee not $where.")
      }
    }
  }

  private abstract class Shadower {
    def addInfos(infos: Infos): Unit
    def isShadowed(name: Name): Boolean
  }
  object Shadower {
    private[this] val localShadowerCache = ReusableInstance[LocalShadower](new LocalShadower, enabled = isCompilerUniverse)

    def using[T](local: Boolean)(f: Shadower => T): T =
      if (local) localShadowerCache.using { shadower =>
        shadower.clear()
        f(shadower)
      }
      else f(NoShadower)
  }

  /** Used for exclude implicits from outer scopes that are shadowed by same-named implicits */
  private final class LocalShadower extends Shadower {
    // OPT: using j.l.HashSet as that retains the internal array on clear(), which makes it worth caching.
    val shadowed = new java.util.HashSet[Name](512)
    def addInfos(infos: Infos): Unit = {
      infos.foreach(i => shadowed.add(i.name))
    }
    def isShadowed(name: Name) = shadowed.contains(name)
    def clear(): Unit = shadowed.clear()
  }
  /** Used for the implicits of expected type, when no shadowing checks are needed. */
  private object NoShadower extends Shadower {
    def addInfos(infos: Infos): Unit = {}
    def isShadowed(name: Name) = false
  }
}

trait ImplicitsStats {
  self: TypesStats with Statistics =>

  val subtypeImpl         = newSubCounter("  of which in implicit", subtypeCount)
  val findMemberImpl      = newSubCounter("  of which in implicit", findMemberCount)
  val subtypeAppInfos     = newSubCounter("  of which in app impl", subtypeCount)
  val implicitSearchCount = newCounter   ("#implicit searches", "typer")
  val plausiblyCompatibleImplicits
                          = newSubCounter("  #plausibly compatible", implicitSearchCount)
  val matchingImplicits   = newSubCounter("  #matching", implicitSearchCount)
  val typedImplicits      = newSubCounter("  #typed", implicitSearchCount)
  val foundImplicits      = newSubCounter("  #found", implicitSearchCount)
  val improvesCount       = newSubCounter("  #implicit improves tests", implicitSearchCount)
  val improvesCachedCount = newSubCounter("  #implicit improves cached ", implicitSearchCount)
  val inscopeImplicitHits = newSubCounter("  #implicit inscope hits", implicitSearchCount)
  val oftypeImplicitHits  = newSubCounter("  #implicit oftype hits ", implicitSearchCount)
  val implicitNanos       = newSubTimer  ("time spent in implicits", typerNanos)
  val inscopeSucceedNanos = newSubTimer  ("  successful in scope", typerNanos)
  val inscopeFailNanos    = newSubTimer  ("  failed in scope", typerNanos)
  val oftypeSucceedNanos  = newSubTimer  ("  successful of type", typerNanos)
  val oftypeFailNanos     = newSubTimer  ("  failed of type", typerNanos)
  val subtypeETNanos      = newSubTimer  ("  assembling parts", typerNanos)
  val matchesPtNanos      = newSubTimer  ("  matchesPT", typerNanos)
  val implicitCacheAccs   = newCounter   ("implicit cache accesses", "typer")
  val implicitCacheHits   = newSubCounter("implicit cache hits", implicitCacheAccs)

  val matchesPtInstCalls  = newCounter   ("implicits instantiated for pruning")
  val matchesPtInstMismatch1
                          = newSubCounter("  immediate mismatches", matchesPtInstCalls)
  val matchesPtInstMismatch2
                          = newSubCounter("  instantiated mismatches", matchesPtInstCalls)
}
