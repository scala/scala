/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

//todo: rewrite or disllow new T where T is a mixin (currently: <init> not a member of T)
//todo: use inherited type info also for vars and values
//todo: disallow C#D in superclass
//todo: treat :::= correctly

package scala.tools.nsc
package typechecker

import scala.annotation.tailrec
import scala.collection.{ mutable, immutable }
import mutable.{ LinkedHashMap, ListBuffer }
import scala.util.matching.Regex
import symtab.Flags._
import scala.reflect.internal.util.Statistics
import scala.language.implicitConversions

/** This trait provides methods to find various kinds of implicits.
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait Implicits extends SourceContextUtils {
  self: Analyzer =>

  import global._
  import definitions._
  import ImplicitsStats._
  import typeDebug.{ ptTree, ptBlock, ptLine }
  import global.typer.{ printTyping, deindentTyping, indentTyping, printInference }

  def inferImplicit(tree: Tree, pt: Type, reportAmbiguous: Boolean, isView: Boolean, context: Context): SearchResult =
    inferImplicit(tree, pt, reportAmbiguous, isView, context, true, tree.pos)

  def inferImplicit(tree: Tree, pt: Type, reportAmbiguous: Boolean, isView: Boolean, context: Context, saveAmbiguousDivergent: Boolean): SearchResult =
    inferImplicit(tree, pt, reportAmbiguous, isView, context, saveAmbiguousDivergent, tree.pos)

  /** Search for an implicit value. See the comment on `result` at the end of class `ImplicitSearch`
   *  for more info how the search is conducted.
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
   *  @param pos                     Position that is should be used for tracing and error reporting
   *                                 (useful when we infer synthetic stuff and pass EmptyTree in the `tree` argument)
   *                                 If it's set NoPosition, then position-based services will use `tree.pos`
   *  @return                        A search result
   */
  def inferImplicit(tree: Tree, pt: Type, reportAmbiguous: Boolean, isView: Boolean, context: Context, saveAmbiguousDivergent: Boolean, pos: Position): SearchResult = {
    printInference("[infer %s] %s with pt=%s in %s".format(
      if (isView) "view" else "implicit",
      tree, pt, context.owner.enclClass)
    )
    printTyping(
      ptBlock("infer implicit" + (if (isView) " view" else ""),
        "tree"        -> tree,
        "pt"          -> pt,
        "undetparams" -> context.outer.undetparams
      )
    )
    indentTyping()

    val rawTypeStart    = if (Statistics.canEnable) Statistics.startCounter(rawTypeImpl) else null
    val findMemberStart = if (Statistics.canEnable) Statistics.startCounter(findMemberImpl) else null
    val subtypeStart    = if (Statistics.canEnable) Statistics.startCounter(subtypeImpl) else null
    val start           = if (Statistics.canEnable) Statistics.startTimer(implicitNanos) else null
    if (printInfers && !tree.isEmpty && !context.undetparams.isEmpty)
      printTyping("typing implicit: %s %s".format(tree, context.undetparamsString))
    val implicitSearchContext = context.makeImplicit(reportAmbiguous)
    val result = new ImplicitSearch(tree, pt, isView, implicitSearchContext, pos).bestImplicit
    if ((result.isFailure || !settings.Xdivergence211.value) && saveAmbiguousDivergent && implicitSearchContext.hasErrors) {
      context.updateBuffer(implicitSearchContext.errBuffer.filter(err => err.kind == ErrorKinds.Ambiguous || err.kind == ErrorKinds.Divergent))
      debugwarn("update buffer: " + implicitSearchContext.errBuffer)
    }
    printInference("[infer implicit] inferred " + result)
    context.undetparams = context.undetparams filterNot result.subst.from.contains

    if (Statistics.canEnable) Statistics.stopTimer(implicitNanos, start)
    if (Statistics.canEnable) Statistics.stopCounter(rawTypeImpl, rawTypeStart)
    if (Statistics.canEnable) Statistics.stopCounter(findMemberImpl, findMemberStart)
    if (Statistics.canEnable) Statistics.stopCounter(subtypeImpl, subtypeStart)
    deindentTyping()
    printTyping("Implicit search yielded: "+ result)
    result
  }

  /** A friendly wrapper over inferImplicit to be used in macro contexts and toolboxes.
   */
  def inferImplicit(tree: Tree, pt: Type, isView: Boolean, context: Context, silent: Boolean, withMacrosDisabled: Boolean, pos: Position, onError: (Position, String) => Unit): Tree = {
    val wrapper1 = if (!withMacrosDisabled) (context.withMacrosEnabled[SearchResult] _) else (context.withMacrosDisabled[SearchResult] _)
    def wrapper(inference: => SearchResult) = wrapper1(inference)
    def fail(reason: Option[String]) = {
      if (!silent) {
        if (context.hasErrors) onError(context.errBuffer.head.errPos, context.errBuffer.head.errMsg)
        else onError(pos, reason getOrElse "implicit search has failed. to find out the reason, turn on -Xlog-implicits")
      }
      EmptyTree
    }
    try {
      wrapper(inferImplicit(tree, pt, reportAmbiguous = true, isView = isView, context = context, saveAmbiguousDivergent = !silent, pos = pos)) match {
        case failure if failure.tree.isEmpty => fail(None)
        case success => success.tree
      }
    } catch {
      case ex: DivergentImplicit =>
        if (settings.Xdivergence211.value)
          debugwarn("this shouldn't happen. DivergentImplicit exception has been thrown with -Xdivergence211 turned on: "+ex)
        fail(Some("divergent implicit expansion"))
    }
  }

  /** Find all views from type `tp` (in which `tpars` are free)
   *
   * Note that the trees in the search results in the returned list share the same type variables.
   * Ignore their constr field! The list of type constraints returned along with each tree specifies the constraints that
   * must be met by the corresponding type parameter in `tpars` (for the returned implicit view to be valid).
   *
   * @arg tp      from-type for the implicit conversion
   * @arg context search implicits here
   * @arg tpars   symbols that should be considered free type variables
   *              (implicit search should not try to solve them, just track their constraints)
   */
  def allViewsFrom(tp: Type, context: Context, tpars: List[Symbol]): List[(SearchResult, List[TypeConstraint])] = {
    // my untouchable typevars are better than yours (they can't be constrained by them)
    val tvars = tpars map (TypeVar untouchable _)
    val tpSubsted = tp.subst(tpars, tvars)

    val search = new ImplicitSearch(EmptyTree, functionType(List(tpSubsted), AnyClass.tpe), true, context.makeImplicit(false))

    search.allImplicitsPoly(tvars)
  }

  private final val sizeLimit = 50000
  private type Infos = List[ImplicitInfo]
  private type Infoss = List[List[ImplicitInfo]]
  private type InfoMap = LinkedHashMap[Symbol, List[ImplicitInfo]] // A map from class symbols to their associated implicits
  private val implicitsCache = new LinkedHashMap[Type, Infoss]
  private val infoMapCache = new LinkedHashMap[Symbol, InfoMap]
  private val improvesCache = perRunCaches.newMap[(ImplicitInfo, ImplicitInfo), Boolean]()

  def resetImplicits() {
    implicitsCache.clear()
    infoMapCache.clear()
    improvesCache.clear()
  }

  /* Map a polytype to one in which all type parameters and argument-dependent types are replaced by wildcards.
   * Consider `implicit def b(implicit x: A): x.T = error("")`. We need to approximate DebruijnIndex types
   * when checking whether `b` is a valid implicit, as we haven't even searched a value for the implicit arg `x`,
   * so we have to approximate (otherwise it is excluded a priori).
   */
  private def depoly(tp: Type): Type = tp match {
    case PolyType(tparams, restpe) => deriveTypeWithWildcards(tparams)(ApproximateDependentMap(restpe))
    case _                         => ApproximateDependentMap(tp)
  }

  /** The result of an implicit search
   *  @param  tree    The tree representing the implicit
   *  @param  subst   A substituter that represents the undetermined type parameters
   *                  that were instantiated by the winning implicit.
   */
  class SearchResult(val tree: Tree, val subst: TreeTypeSubstituter) {
    override def toString = "SearchResult(%s, %s)".format(tree,
      if (subst.isEmpty) "" else subst)

    def isFailure          = false
    def isAmbiguousFailure = false
    // only used when -Xdivergence211 is turned on
    def isDivergent        = false
    final def isSuccess    = !isFailure
  }

  lazy val SearchFailure = new SearchResult(EmptyTree, EmptyTreeTypeSubstituter) {
    override def isFailure = true
  }

  // only used when -Xdivergence211 is turned on
  lazy val DivergentSearchFailure = new SearchResult(EmptyTree, EmptyTreeTypeSubstituter) {
    override def isFailure   = true
    override def isDivergent = true
  }

  lazy val AmbiguousSearchFailure = new SearchResult(EmptyTree, EmptyTreeTypeSubstituter) {
    override def isFailure          = true
    override def isAmbiguousFailure = true
  }

  /** A class that records an available implicit
   *  @param   name   The name of the implicit
   *  @param   pre    The prefix type of the implicit
   *  @param   sym    The symbol of the implicit
   */
  class ImplicitInfo(val name: Name, val pre: Type, val sym: Symbol) {
    private var tpeCache: Type = null

    /** Computes member type of implicit from prefix `pre` (cached). */
    def tpe: Type = {
      if (tpeCache eq null) tpeCache = pre.memberType(sym)
      tpeCache
    }

    def isCyclicOrErroneous =
      try sym.hasFlag(LOCKED) || containsError(tpe)
      catch { case _: CyclicReference => true }

    var useCountArg: Int = 0
    var useCountView: Int = 0

    /** Does type `tp` contain an Error type as parameter or result?
     */
    private def containsError(tp: Type): Boolean = tp match {
      case PolyType(tparams, restpe) =>
        containsError(restpe)
      case NullaryMethodType(restpe) =>
        containsError(restpe)
      case mt @ MethodType(_, restpe) =>
        (mt.paramTypes exists typeIsError) || containsError(restpe)
      case _ =>
        tp.isError
    }

    /** Todo reconcile with definition of stability given in Types.scala */
    private def isStable(tp: Type): Boolean = tp match {
     case TypeRef(pre, sym, _) =>
       sym.isPackageClass ||
       sym.isModuleClass && isStable(pre) /*||
       sym.isAliasType && isStable(tp.normalize)*/
     case _ => tp.isStable
    }
    def isStablePrefix = isStable(pre)

    override def equals(other: Any) = other match {
      case that: ImplicitInfo =>
          this.name == that.name &&
          this.pre =:= that.pre &&
          this.sym == that.sym
      case _ => false
    }
    override def hashCode = name.## + pre.## + sym.##
    override def toString = name + ": " + tpe
  }

  /** A class which is used to track pending implicits to prevent infinite implicit searches.
   */
  case class OpenImplicit(info: ImplicitInfo, pt: Type, tree: Tree)

  /** A sentinel indicating no implicit was found */
  val NoImplicitInfo = new ImplicitInfo(null, NoType, NoSymbol) {
    // equals used to be implemented in ImplicitInfo with an `if(this eq NoImplicitInfo)`
    // overriding the equals here seems cleaner and benchmarks show no difference in performance
    override def equals(other: Any) = other match { case that: AnyRef => that eq this  case _ => false }
    override def hashCode = 1
  }

  /** A constructor for types ?{ def/type name: tp }, used in infer view to member
   *  searches.
   */
  def memberWildcardType(name: Name, tp: Type) = {
    val result = refinedType(List(WildcardType), NoSymbol)
    name match {
      case x: TermName => result.typeSymbol.newMethod(x) setInfoAndEnter tp
      case x: TypeName => result.typeSymbol.newAbstractType(x) setInfoAndEnter tp
    }
    result
  }

  /** An extractor for types of the form ? { name: ? }
   */
  object HasMember {
    private val hasMemberCache = perRunCaches.newMap[Name, Type]()
    def apply(name: Name): Type = hasMemberCache.getOrElseUpdate(name, memberWildcardType(name, WildcardType))
    def unapply(pt: Type): Option[Name] = pt match {
      case RefinedType(List(WildcardType), Scope(sym)) if sym.tpe == WildcardType => Some(sym.name)
      case _ => None
    }
  }

  /** An extractor for types of the form ? { name: (? >: argtpe <: Any*)restp }
   */
  object HasMethodMatching {
    val dummyMethod = NoSymbol.newTermSymbol(newTermName("typer$dummy"))
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
                Some((sym.name, params map (_.tpe.bounds.lo), restpe))
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
  class ImplicitSearch(tree: Tree, pt: Type, isView: Boolean, context0: Context, pos0: Position = NoPosition)
    extends Typer(context0) with ImplicitsContextErrors {
      printTyping(
        ptBlock("new ImplicitSearch",
          "tree"        -> tree,
          "pt"          -> pt,
          "isView"      -> isView,
          "context0"    -> context0,
          "undetparams" -> context.outer.undetparams
        )
      )
//    assert(tree.isEmpty || tree.pos.isDefined, tree)
    def pos = if (pos0 != NoPosition) pos0 else tree.pos

    def failure(what: Any, reason: String, pos: Position = this.pos): SearchResult = {
      if (settings.XlogImplicits.value)
        reporter.echo(pos, what+" is not a valid implicit value for "+pt+" because:\n"+reason)
      SearchFailure
    }

    import infer._
    /** Is implicit info `info1` better than implicit info `info2`?
     */
    def improves(info1: ImplicitInfo, info2: ImplicitInfo) = {
      if (Statistics.canEnable) Statistics.incCounter(improvesCount)
      (info2 == NoImplicitInfo) ||
      (info1 != NoImplicitInfo) && {
        if (info1.sym.isStatic && info2.sym.isStatic) {
          improvesCache get (info1, info2) match {
            case Some(b) => if (Statistics.canEnable) Statistics.incCounter(improvesCachedCount); b
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

    /** Does type `dtor` dominate type `dted`?
     *  This is the case if the stripped cores `dtor1` and `dted1` of both types are
     *  the same wrt `=:=`, or if they overlap and the complexity of `dtor1` is higher
     *  than the complexity of `dted1`.
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
      def core(tp: Type): Type = tp.normalize match {
        case RefinedType(parents, defs) => intersectionType(parents map core, tp.typeSymbol.owner)
        case AnnotatedType(annots, tp, selfsym) => core(tp)
        case ExistentialType(tparams, result) => core(result).subst(tparams, tparams map (t => core(t.info.bounds.hi)))
        case PolyType(tparams, result) => core(result).subst(tparams, tparams map (t => core(t.info.bounds.hi)))
        case _ => tp
      }
      def stripped(tp: Type): Type = {
        // `t.typeSymbol` returns the symbol of the normalized type. If that normalized type
        // is a `PolyType`, the symbol of the result type is collected. This is precisely
        // what we require for SI-5318.
        val syms = for (t <- tp; if t.typeSymbol.isTypeParameter) yield t.typeSymbol
        deriveTypeWithWildcards(syms.distinct)(tp)
      }
      def sum(xs: List[Int]) = (0 /: xs)(_ + _)
      def complexity(tp: Type): Int = tp.normalize match {
        case NoPrefix =>
          0
        case SingleType(pre, sym) =>
          if (sym.isPackage) 0 else complexity(tp.normalize.widen)
        case TypeRef(pre, sym, args) =>
          complexity(pre) + sum(args map complexity) + 1
        case RefinedType(parents, _) =>
          sum(parents map complexity) + 1
        case _ =>
          1
      }
      def overlaps(tp1: Type, tp2: Type): Boolean = (tp1, tp2) match {
        case (RefinedType(parents, _), _) => parents exists (overlaps(_, tp2))
        case (_, RefinedType(parents, _)) => parents exists (overlaps(tp1, _))
        case _ => tp1.typeSymbol == tp2.typeSymbol
      }
      val dtor1 = stripped(core(dtor))
      val dted1 = stripped(core(dted))
      overlaps(dtor1, dted1) && (dtor1 =:= dted1 || complexity(dtor1) > complexity(dted1))
    }

    if (Statistics.canEnable) Statistics.incCounter(implicitSearchCount)

    /** The type parameters to instantiate */
    val undetParams = if (isView) List() else context.outer.undetparams

    /** The expected type with all undetermined type parameters replaced with wildcards. */
    def approximate(tp: Type) = deriveTypeWithWildcards(undetParams)(tp)
    val wildPt = approximate(pt)

    /** Try to construct a typed tree from given implicit info with given
     *  expected type.
     *  Detect infinite search trees for implicits.
     *
     *  @param info    The given implicit info describing the implicit definition
     *  @param isLocal Is the implicit in the local scope of the call site?
     *  @pre           `info.tpe` does not contain an error
     */
    private def typedImplicit(info: ImplicitInfo, ptChecked: Boolean, isLocal: Boolean): SearchResult = {
      // SI-7167 let implicit macros decide what amounts for a divergent implicit search
      // imagine a macro writer which wants to synthesize a complex implicit Complex[T] by making recursive calls to Complex[U] for its parts
      // e.g. we have `class Foo(val bar: Bar)` and `class Bar(val x: Int)`
      // then it's quite reasonable for the macro writer to synthesize Complex[Foo] by calling `inferImplicitValue(typeOf[Complex[Bar])`
      // however if we didn't insert the `info.sym.isMacro` check here, then under some circumstances
      // (e.g. as described here http://groups.google.com/group/scala-internals/browse_thread/thread/545462b377b0ac0a)
      // `dominates` might decide that `Bar` dominates `Foo` and therefore a recursive implicit search should be prohibited
      // now when we yield control of divergent expansions to the macro writer, what happens next?
      // in the worst case, if the macro writer is careless, we'll get a StackOverflowException from repeated macro calls
      // otherwise, the macro writer could check `c.openMacros` and `c.openImplicits` and do `c.abort` when expansions are deemed to be divergent
      // upon receiving `c.abort` the typechecker will decide that the corresponding implicit search has failed
      // which will fail the entire stack of implicit searches, producing a nice error message provided by the programmer
      (context.openImplicits find { case OpenImplicit(info, tp, tree1) => !info.sym.isMacro && tree1.symbol == tree.symbol && dominates(pt, tp)}) match {
         case Some(pending) =>
           //println("Pending implicit "+pending+" dominates "+pt+"/"+undetParams) //@MDEBUG
           if (settings.Xdivergence211.value) DivergentSearchFailure
           else throw DivergentImplicit
         case None =>
           def pre211DivergenceLogic() = {
           try {
             context.openImplicits = OpenImplicit(info, pt, tree) :: context.openImplicits
             // println("  "*context.openImplicits.length+"typed implicit "+info+" for "+pt) //@MDEBUG
             typedImplicit0(info, ptChecked, isLocal)
           } catch {
             case ex: DivergentImplicit =>
               //println("DivergentImplicit for pt:"+ pt +", open implicits:"+context.openImplicits) //@MDEBUG
               if (context.openImplicits.tail.isEmpty) {
                 if (!pt.isErroneous && !info.sym.isMacro)
                   DivergingImplicitExpansionError(tree, pt, info.sym)(context)
                 SearchFailure
               } else {
                 throw DivergentImplicit
               }
           } finally {
             context.openImplicits = context.openImplicits.tail
           }
           }
           def post211DivergenceLogic() = {
             try {
               context.openImplicits = OpenImplicit(info, pt, tree) :: context.openImplicits
               // println("  "*context.openImplicits.length+"typed implicit "+info+" for "+pt) //@MDEBUG
               val result = typedImplicit0(info, ptChecked, isLocal)
               if (result.isDivergent) {
                 //println("DivergentImplicit for pt:"+ pt +", open implicits:"+context.openImplicits) //@MDEBUG
                 if (context.openImplicits.tail.isEmpty && !pt.isErroneous)
                   DivergingImplicitExpansionError(tree, pt, info.sym)(context)
               }
               result
             } finally {
               context.openImplicits = context.openImplicits.tail
             }
           }
           if (settings.Xdivergence211.value) post211DivergenceLogic()
           else pre211DivergenceLogic()
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
      val start = if (Statistics.canEnable) Statistics.startTimer(matchesPtNanos) else null
      val result = normSubType(tp, pt) || isView && {
        pt match {
          case TypeRef(_, Function1.Sym, arg1 :: arg2 :: Nil) =>
            matchesPtView(tp, arg1, arg2, undet)
          case _ =>
            false
        }
      }
      if (Statistics.canEnable) Statistics.stopTimer(matchesPtNanos, start)
      result
    }
    private def matchesPt(info: ImplicitInfo): Boolean = (
      info.isStablePrefix && matchesPt(depoly(info.tpe), wildPt, Nil)
    )

    private def matchesPtView(tp: Type, ptarg: Type, ptres: Type, undet: List[Symbol]): Boolean = tp match {
      case MethodType(p :: _, restpe) if p.isImplicit => matchesPtView(restpe, ptarg, ptres, undet)
      case MethodType(p :: Nil, restpe)               => matchesArgRes(p.tpe, restpe, ptarg, ptres, undet)
      case ExistentialType(_, qtpe)                   => matchesPtView(normalize(qtpe), ptarg, ptres, undet)
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
              else if (sym.isAbstractType) loop(tp, pt.bounds.lo)
              else {
                val len = args.length - 1
                hasLength(params, len) &&
                sym == FunctionClass(len) && {
                  var ps = params
                  var as = args
                  if (fast) {
                    while (ps.nonEmpty && as.nonEmpty) {
                      if (!isPlausiblySubType(as.head, ps.head.tpe))
                        return false
                      ps = ps.tail
                      as = as.tail
                    }
                  } else {
                    while (ps.nonEmpty && as.nonEmpty) {
                      if (!(as.head <:< ps.head.tpe))
                        return false
                      ps = ps.tail
                      as = as.tail
                    }
                  }
                  ps.isEmpty && as.nonEmpty && {
                    val lastArg = as.head
                    as.tail.isEmpty && loop(restpe, lastArg)
                  }
                }
              }

            case _            => if (fast) false else tp <:< pt
          }
        case NullaryMethodType(restpe)  => loop(restpe, pt)
        case PolyType(_, restpe)        => loop(restpe, pt)
        case ExistentialType(_, qtpe)   => if (fast) loop(qtpe, pt) else normalize(tp) <:< pt // is !fast case needed??
        case _                          => if (fast) isPlausiblySubType(tp, pt) else tp <:< pt
      }
      loop(tp0, pt0)
    }

    /** This expresses more cleanly in the negative: there's a linear path
     *  to a final true or false.
     */
    private def isPlausiblySubType(tp1: Type, tp2: Type) = !isImpossibleSubType(tp1, tp2)
    private def isImpossibleSubType(tp1: Type, tp2: Type) = tp1.dealiasWiden match {
      // We can only rule out a subtype relationship if the left hand
      // side is a class, else we may not know enough.
      case tr1 @ TypeRef(_, sym1, _) if sym1.isClass =>
        tp2.dealiasWiden match {
          case TypeRef(_, sym2, _)         => sym2.isClass && !(sym1 isWeakSubClass sym2)
          case RefinedType(parents, decls) => decls.nonEmpty && tr1.member(decls.head.name) == NoSymbol
          case _                           => false
        }
      case _ => false
    }

    private def typedImplicit0(info: ImplicitInfo, ptChecked: Boolean, isLocal: Boolean): SearchResult = {
      if (Statistics.canEnable) Statistics.incCounter(plausiblyCompatibleImplicits)
      printTyping (
        ptBlock("typedImplicit0",
          "info.name" -> info.name,
          "ptChecked" -> ptChecked,
          "pt"        -> wildPt,
          "orig"      -> ptBlock("info",
            "undetParams"           -> undetParams,
            "info.pre"              -> info.pre
          ).replaceAll("\\n", "\n  ")
        )
      )

      if (ptChecked || matchesPt(info))
        typedImplicit1(info, isLocal)
      else
        SearchFailure
    }

    private def typedImplicit1(info: ImplicitInfo, isLocal: Boolean): SearchResult = {
      if (Statistics.canEnable) Statistics.incCounter(matchingImplicits)

      val itree = atPos(pos.focus) {
        // workaround for deficient context provided by ModelFactoryImplicitSupport#makeImplicitConstraints
        val isScalaDoc = context.tree == EmptyTree

        if (isLocal && !isScalaDoc) {
          // SI-4270 SI-5376 Always use an unattributed Ident for implicits in the local scope,
          // rather than an attributed Select, to detect shadowing.
          Ident(info.name)
        } else {
          assert(info.pre != NoPrefix, info)
          // SI-2405 Not info.name, which might be an aliased import
          val implicitMemberName = info.sym.name
          Select(gen.mkAttributedQualifier(info.pre), implicitMemberName)
        }
      }
      printTyping("typedImplicit1 %s, pt=%s, from implicit %s:%s".format(
        typeDebug.ptTree(itree), wildPt, info.name, info.tpe)
      )

      def fail(reason: String): SearchResult = failure(itree, reason)
      try {
        val itree1 =
          if (isView) {
            val arg1 :: arg2 :: _ = pt.typeArgs
            typed1(
              atPos(itree.pos)(Apply(itree, List(Ident("<argument>") setType approximate(arg1)))),
              EXPRmode,
              approximate(arg2)
            )
          }
          else
            typed1(itree, EXPRmode, wildPt)

        if (context.hasErrors)
          return fail(context.errBuffer.head.errMsg)

        if (Statistics.canEnable) Statistics.incCounter(typedImplicits)

        printTyping("typed implicit %s:%s, pt=%s".format(itree1, itree1.tpe, wildPt))
        val itree2 = if (isView) (itree1: @unchecked) match { case Apply(fun, _) => fun }
                     else adapt(itree1, EXPRmode, wildPt)

        printTyping("adapted implicit %s:%s to %s".format(
          itree1.symbol, itree2.tpe, wildPt)
        )

        def hasMatchingSymbol(tree: Tree): Boolean = (tree.symbol == info.sym) || {
          tree match {
            case Apply(fun, _)          => hasMatchingSymbol(fun)
            case TypeApply(fun, _)      => hasMatchingSymbol(fun)
            case Select(pre, nme.apply) => pre.symbol == info.sym
            case _                      => false
          }
        }

        if (context.hasErrors)
          fail("hasMatchingSymbol reported error: " + context.errBuffer.head.errMsg)
        else if (isLocal && !hasMatchingSymbol(itree1))
          fail("candidate implicit %s is shadowed by %s".format(
            info.sym.fullLocationString, itree1.symbol.fullLocationString))
        else {
          val tvars = undetParams map freshVar
          def ptInstantiated = pt.instantiateTypeParams(undetParams, tvars)

          printInference("[search] considering %s (pt contains %s) trying %s against pt=%s".format(
            if (undetParams.isEmpty) "no tparams" else undetParams.map(_.name).mkString(", "),
            typeVarsInType(ptInstantiated) filterNot (_.isGround) match { case Nil => "no tvars" ; case tvs => tvs.mkString(", ") },
            itree2.tpe, pt
          ))

          if (matchesPt(itree2.tpe, ptInstantiated, undetParams)) {
            if (tvars.nonEmpty)
              printTyping(ptLine("" + info.sym, "tvars" -> tvars, "tvars.constr" -> tvars.map(_.constr)))

            val targs = solvedTypes(tvars, undetParams, undetParams map varianceInType(pt),
                                    false, lubDepth(List(itree2.tpe, pt)))

            // #2421: check that we correctly instantiated type parameters outside of the implicit tree:
            checkBounds(itree2, NoPrefix, NoSymbol, undetParams, targs, "inferred ")
            if (context.hasErrors)
              return fail("type parameters weren't correctly instantiated outside of the implicit tree: " + context.errBuffer.head.errMsg)

            // filter out failures from type inference, don't want to remove them from undetParams!
            // we must be conservative in leaving type params in undetparams
            // prototype == WildcardType: want to remove all inferred Nothings
            val AdjustedTypeArgs(okParams, okArgs) = adjustTypeArgs(undetParams, tvars, targs)

            val subst: TreeTypeSubstituter =
              if (okParams.isEmpty) EmptyTreeTypeSubstituter
              else {
                val subst = new TreeTypeSubstituter(okParams, okArgs)
                subst traverse itree2
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
            // TODO: the return tree is ignored.  This seems to make
            // no difference, but it's bad practice regardless.


            val checked = itree2 match {
              case TypeApply(fun, args)           => typedTypeApply(itree2, EXPRmode, fun, args)
              case Apply(TypeApply(fun, args), _) => typedTypeApply(itree2, EXPRmode, fun, args) // t2421c
              case t                              => t
            }

            if (context.hasErrors)
              fail("typing TypeApply reported errors for the implicit tree: " + context.errBuffer.head.errMsg)
            else {
              val result = new SearchResult(itree2, subst)
              if (Statistics.canEnable) Statistics.incCounter(foundImplicits)
              printInference("[success] found %s for pt %s".format(result, ptInstantiated))
              result
            }
          }
          else fail("incompatible: %s does not match expected type %s".format(itree2.tpe, ptInstantiated))
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

      sym.isInitialized ||
      sym.sourceFile == null ||
      (sym.sourceFile ne context.unit.source.file) ||
      hasExplicitResultType(sym) ||
      comesBefore(sym, context.owner)
    }

    /** Prune ImplicitInfos down to either all the eligible ones or the best one.
     *
     *  @param  iss       list of list of infos
     *  @param  isLocal   if true, `iss` represents in-scope implicits, which must respect the normal rules of
     *                    shadowing. The head of the list `iss` must represent implicits from the closest
     *                    enclosing scope, and so on.
     */
    class ImplicitComputation(iss: Infoss, isLocal: Boolean) {
      abstract class Shadower {
        def addInfos(infos: Infos)
        def isShadowed(name: Name): Boolean
      }
      private val shadower: Shadower = {
        /** Used for exclude implicits from outer scopes that are shadowed by same-named implicits */
        final class LocalShadower extends Shadower {
          val shadowed = util.HashSet[Name](512)
          def addInfos(infos: Infos) {
            shadowed addEntries infos.map(_.name)
          }
          def isShadowed(name: Name) = shadowed(name)
        }
        /** Used for the implicits of expected type, when no shadowing checks are needed. */
        object NoShadower extends Shadower {
          def addInfos(infos: Infos) {}
          def isShadowed(name: Name) = false
        }
        if (isLocal) new LocalShadower else NoShadower
      }

      private var best: SearchResult = SearchFailure

      private def isIneligible(info: ImplicitInfo) = (
           info.isCyclicOrErroneous
        || isView && isPredefMemberNamed(info.sym, nme.conforms)
        || shadower.isShadowed(info.name)
        || (!context.macrosEnabled && info.sym.isTermMacro)
      )

      /** True if a given ImplicitInfo (already known isValid) is eligible.
       */
      def survives(info: ImplicitInfo) = (
           !isIneligible(info)                      // cyclic, erroneous, shadowed, or specially excluded
        && isPlausiblyCompatible(info.tpe, wildPt)  // optimization to avoid matchesPt
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
       *
       *  The old way of handling divergence.
       *  Only enabled when -Xdivergence211 is turned off.
       */
      private var divergence = false
      private val divergenceHandler: PartialFunction[Throwable, SearchResult] = {
        var remaining = 1;
        { case x: DivergentImplicit if remaining > 0 =>
            remaining -= 1
            divergence = true
            log("discarding divergent implicit during implicit search")
            SearchFailure
        }
      }

      /** Preventing a divergent implicit from terminating implicit search,
       *  so that if there is a best candidate it can still be selected.
       *
       *  The new way of handling divergence.
       *  Only enabled when -Xdivergence211 is turned on.
       */
      object DivergentImplicitRecovery {
        // symbol of the implicit that caused the divergence.
        // Initially null, will be saved on first diverging expansion.
        private var implicitSym: Symbol    = _
        private var countdown: Int = 1

        def sym: Symbol = implicitSym
        def apply(search: SearchResult, i: ImplicitInfo): SearchResult =
          if (search.isDivergent && countdown > 0) {
            countdown -= 1
            implicitSym = i.sym
            log("discarding divergent implicit ${implicitSym} during implicit search")
            SearchFailure
          } else search
      }

      /** Sorted list of eligible implicits.
       */
      val eligible = {
        val matches = iss flatMap { is =>
          val result = is filter (info => checkValid(info.sym) && survives(info))
          shadower addInfos is
          result
        }

        // most frequent one first
        matches sortBy (x => if (isView) -x.useCountView else -x.useCountArg)
      }
      if (eligible.nonEmpty)
        printInference("[search%s] %s with pt=%s in %s, eligible:\n  %s".format(
          if (isView) " view" else "",
          tree, pt, context.owner.enclClass, eligible.mkString("\n  "))
        )

      /** Faster implicit search.  Overall idea:
       *   - prune aggressively
       *   - find the most likely one
       *   - if it matches, forget about all others it improves upon
       */
      @tailrec private def rankImplicits(pending: Infos, acc: Infos): Infos = pending match {
        case Nil      => acc
        case i :: is  =>
          def pre211tryImplicitInfo(i: ImplicitInfo) =
            try typedImplicit(i, ptChecked = true, isLocal)
            catch divergenceHandler

          def post211tryImplicitInfo(i: ImplicitInfo) =
            DivergentImplicitRecovery(typedImplicit(i, ptChecked = true, isLocal), i)

          {
            if (settings.Xdivergence211.value) post211tryImplicitInfo(i)
            else pre211tryImplicitInfo(i)
          } match {
            // only used if -Xdivergence211 is turned on
            case sr if sr.isDivergent =>
              Nil
            case sr if sr.isFailure =>
              // We don't want errors that occur during checking implicit info
              // to influence the check of further infos.
              context.condBufferFlush(_.kind != ErrorKinds.Divergent)
              rankImplicits(is, acc)
            case newBest        =>
              best = newBest
              val newPending = undoLog undo {
                is filterNot (alt => alt == i || {
                  try improves(i, alt)
                  catch {
                    case e: CyclicReference =>
                      if (printInfers) {
                        println(i+" discarded because cyclic reference occurred")
                        e.printStackTrace()
                      }
                      true
                  }
                })
              }
              rankImplicits(newPending, i :: acc)
          }
      }

      /** Returns all eligible ImplicitInfos and their SearchResults in a map.
       */
      def findAll() = mapFrom(eligible)(typedImplicit(_, ptChecked = false, isLocal))

      /** Returns the SearchResult of the best match.
       */
      def findBest(): SearchResult = {
        // After calling rankImplicits, the least frequent matching one is first and
        // earlier elems may improve on later ones, but not the other way.
        // So if there is any element not improved upon by the first it is an error.
        rankImplicits(eligible, Nil) match {
          case Nil            => ()
          case chosen :: rest =>
            rest find (alt => !improves(chosen, alt)) match {
              case Some(competing)  =>
                AmbiguousImplicitError(chosen, competing, "both", "and", "")(isView, pt, tree)(context)
                return AmbiguousSearchFailure // Stop the search once ambiguity is encountered, see t4457_2.scala
              case _                =>
                if (isView) chosen.useCountView += 1
                else chosen.useCountArg += 1
            }
        }

        if (best.isFailure) {
          /** If there is no winner, and we witnessed and caught divergence,
           *  now we can throw it for the error message.
           */
          if (divergence || DivergentImplicitRecovery.sym != null) {
            if (settings.Xdivergence211.value) DivergingImplicitExpansionError(tree, pt, DivergentImplicitRecovery.sym)(context)
            else throw DivergentImplicit
          }

          if (invalidImplicits.nonEmpty)
            setAddendum(pos, () =>
              s"\n Note: implicit ${invalidImplicits.head} is not applicable here because it comes after the application point and it lacks an explicit result type"
            )
        }

        best
      }
    }

    /** Computes from a list of lists of implicit infos a map which takes
     *  infos which are applicable for given expected type `pt` to their attributed trees.
     *
     *  @param iss            The given list of lists of implicit infos
     *  @param isLocal        Is implicit definition visible without prefix?
     *                        If this is the case then symbols in preceding lists shadow
     *                        symbols of the same name in succeeding lists.
     *  @return               map from infos to search results
     */
    def applicableInfos(iss: Infoss, isLocal: Boolean): Map[ImplicitInfo, SearchResult] = {
      val start       = if (Statistics.canEnable) Statistics.startCounter(subtypeAppInfos) else null
      val computation = new ImplicitComputation(iss, isLocal) { }
      val applicable  = computation.findAll()

      if (Statistics.canEnable) Statistics.stopCounter(subtypeAppInfos, start)
      applicable
    }

    /** Search list of implicit info lists for one matching prototype `pt`.
     *  If found return a search result with a tree from found implicit info
     *  which is typed with expected type `pt`. Otherwise return SearchFailure.
     *
     *  @param implicitInfoss The given list of lists of implicit infos
     *  @param isLocal        Is implicit definition visible without prefix?
     *                        If this is the case then symbols in preceding lists shadow
     *                        symbols of the same name in succeeding lists.
     */
    def searchImplicit(implicitInfoss: Infoss, isLocal: Boolean): SearchResult =
      if (implicitInfoss.forall(_.isEmpty)) SearchFailure
      else new ImplicitComputation(implicitInfoss, isLocal) findBest()

    /** Produce an implicict info map, i.e. a map from the class symbols C of all parts of this type to
     *  the implicit infos in the companion objects of these class symbols C.
     * The parts of a type is the smallest set of types that contains
     *    - the type itself
     *    - the parts of its immediate components (prefix and argument)
     *    - the parts of its base types
     *    - for alias types and abstract types, we take instead the parts
     *    - of their upper bounds.
     *  @return For those parts that refer to classes with companion objects that
     *  can be accessed with unambiguous stable prefixes that are not existentially
     *  bound, the implicits infos which are members of these companion objects.
     */
    private def companionImplicitMap(tp: Type): InfoMap = {

      /** Populate implicit info map by traversing all parts of type `tp`.
       *  Parameters as for `getParts`.
       */
      def getClassParts(tp: Type)(implicit infoMap: InfoMap, seen: mutable.Set[Type], pending: Set[Symbol]) = tp match {
        case TypeRef(pre, sym, args) =>
          infoMap get sym match {
            case Some(infos1) =>
              if (infos1.nonEmpty && !(pre =:= infos1.head.pre.prefix)) {
                log(s"Ignoring implicit members of $pre#$sym as it is also visible via another prefix: ${infos1.head.pre.prefix}")
                infoMap(sym) = List() // ambiguous prefix - ignore implicit members
              }
            case None =>
              if (pre.isStable && !pre.typeSymbol.isExistentiallyBound) {
                val companion = companionSymbolOf(sym, context)
                companion.moduleClass match {
                  case mc: ModuleClassSymbol =>
                    val infos =
                      for (im <- mc.implicitMembers.toList) yield new ImplicitInfo(im.name, singleType(pre, companion), im)
                    if (infos.nonEmpty)
                      infoMap += (sym -> infos)
                  case _ =>
                }
              }
              val bts = tp.baseTypeSeq
              var i = 1
              while (i < bts.length) {
                getParts(bts(i))
                i += 1
              }
              getParts(pre)
            }
      }

      /** Populate implicit info map by traversing all parts of type `tp`.
       *  This method is performance critical.
       *  @param tp   The type for which we want to traverse parts
       *  @param infoMap  The infoMap in which implicit infos corresponding to parts are stored
       *  @param seen     The types that were already visited previously when collecting parts for the given infoMap
       *  @param pending  The set of static symbols for which we are currently trying to collect their parts
       *                  in order to cache them in infoMapCache
       */
      def getParts(tp: Type)(implicit infoMap: InfoMap, seen: mutable.Set[Type], pending: Set[Symbol]) {
        if (seen(tp))
          return
        seen += tp
        tp match {
          case TypeRef(pre, sym, args) =>
            if (sym.isClass) {
              if (!((sym.name == tpnme.REFINE_CLASS_NAME) ||
                    (sym.name startsWith tpnme.ANON_CLASS_NAME) ||
                    (sym.name == tpnme.ROOT))) {
                if (sym.isStatic && !(pending contains sym))
                  infoMap ++= {
                    infoMapCache get sym match {
                      case Some(imap) => imap
                      case None =>
                        val result = new InfoMap
                        getClassParts(sym.tpe)(result, new mutable.HashSet(), pending + sym)
                        infoMapCache(sym) = result
                        result
                    }
                  }
                else
                  getClassParts(tp)
                args foreach (getParts(_))
              }
            } else if (sym.isAliasType) {
              getParts(tp.normalize) // SI-7180 Normalize needed to expand HK type refs
            } else if (sym.isAbstractType) {
              getParts(tp.bounds.hi)
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
          case AnnotatedType(_, t, _) =>
            getParts(t)
          case ExistentialType(_, t) =>
            getParts(t)
          case PolyType(_, t) =>
            getParts(t)
          case _ =>
        }
      }

      val infoMap = new InfoMap
      getParts(tp)(infoMap, new mutable.HashSet(), Set())
      printInference(
        ptBlock("companionImplicitMap " + tp, infoMap.toSeq.map({ case (k, v) => ("" + k, v.mkString(", ")) }): _*)
      )
      infoMap
    }

    /** The implicits made available by type `pt`.
     *  These are all implicits found in companion objects of classes C
     *  such that some part of `tp` has C as one of its superclasses.
     */
    private def implicitsOfExpectedType: Infoss = {
      if (Statistics.canEnable) Statistics.incCounter(implicitCacheAccs)
      implicitsCache get pt match {
        case Some(implicitInfoss) =>
          if (Statistics.canEnable) Statistics.incCounter(implicitCacheHits)
          implicitInfoss
        case None =>
          val start = if (Statistics.canEnable) Statistics.startTimer(subtypeETNanos) else null
          //        val implicitInfoss = companionImplicits(pt)
          val implicitInfoss1 = companionImplicitMap(pt).valuesIterator.toList
          //        val is1 = implicitInfoss.flatten.toSet
          //        val is2 = implicitInfoss1.flatten.toSet
          //        for (i <- is1)
          //          if (!(is2 contains i)) println("!!! implicit infos of "+pt+" differ, new does not contain "+i+",\nold: "+implicitInfoss+",\nnew: "+implicitInfoss1)
          //        for (i <- is2)
          //          if (!(is1 contains i)) println("!!! implicit infos of "+pt+" differ, old does not contain "+i+",\nold: "+implicitInfoss+",\nnew: "+implicitInfoss1)
          if (Statistics.canEnable) Statistics.stopTimer(subtypeETNanos, start)
          implicitsCache(pt) = implicitInfoss1
          if (implicitsCache.size >= sizeLimit)
            implicitsCache -= implicitsCache.keysIterator.next
          implicitInfoss1
      }
    }

    private def TagSymbols =  TagMaterializers.keySet
    private val TagMaterializers = Map[Symbol, Symbol](
      ClassTagClass    -> materializeClassTag,
      WeakTypeTagClass -> materializeWeakTypeTag,
      TypeTagClass     -> materializeTypeTag
    )

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
          if (context.hasErrors) processMacroExpansionError(context.errBuffer.head.errPos, context.errBuffer.head.errMsg)
          else new SearchResult(tree1, EmptyTreeTypeSubstituter)
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
            // if ``pre'' is not a PDT, e.g. if someone wrote
            //   implicitly[scala.reflect.macros.Context#TypeTag[Int]]
            // then we need to fail, because we don't know the prefix to use during type reification
            // upd. we also need to fail silently, because this is a very common situation
            // e.g. quite often we're searching for BaseUniverse#TypeTag, e.g. for a type tag in any universe
            // so that if we find one, we could convert it to whatever universe we need by the means of the `in` method
            // if no tag is found in scope, we end up here, where we ask someone to materialize the tag for us
            // however, since the original search was about a tag with no particular prefix, we cannot proceed
            // this situation happens very often, so emitting an error message here (even if only for -Xlog-implicits) would be too much
            //return failure(tp, "tag error: unsupported prefix type %s (%s)".format(pre, pre.kind))
            return SearchFailure
        }
      )
      // todo. migrate hardcoded materialization in Implicits to corresponding implicit macros
      var materializer = atPos(pos.focus)(gen.mkMethodCall(TagMaterializers(tagClass), List(tp), if (prefix != EmptyTree) List(prefix) else List()))
      if (settings.XlogImplicits.value) reporter.echo(pos, "materializing requested %s.%s[%s] using %s".format(pre, tagClass.name, tp, materializer))
      if (context.macrosEnabled) success(materializer)
      // don't call `failure` here. if macros are disabled, we just fail silently
      // otherwise -Xlog-implicits will spam the long with zillions of "macros are disabled"
      // this is ugly but temporary, since all this code will be removed once I fix implicit macros
      else SearchFailure
    }

    private val ManifestSymbols = Set[Symbol](PartialManifestClass, FullManifestClass, OptManifestClass)

    /** Creates a tree that calls the relevant factory method in object
      * scala.reflect.Manifest for type 'tp'. An EmptyTree is returned if
      * no manifest is found. todo: make this instantiate take type params as well?
      */
    private def manifestOfType(tp: Type, flavor: Symbol): SearchResult = {
      val full = flavor == FullManifestClass
      val opt = flavor == OptManifestClass

      /** Creates a tree that calls the factory method called constructor in object scala.reflect.Manifest */
      def manifestFactoryCall(constructor: String, tparg: Type, args: Tree*): Tree =
        if (args contains EmptyTree) EmptyTree
        else typedPos(tree.pos.focus) {
          val mani = gen.mkManifestFactoryCall(full, constructor, tparg, args.toList)
          if (settings.debug.value) println("generated manifest: "+mani) // DEBUG
          mani
        }

      /** Creates a tree representing one of the singleton manifests.*/
      def findSingletonManifest(name: String) = typedPos(tree.pos.focus) {
        Select(gen.mkAttributedRef(FullManifestModule), name)
      }

      /** Re-wraps a type in a manifest before calling inferImplicit on the result */
      def findManifest(tp: Type, manifestClass: Symbol = if (full) FullManifestClass else PartialManifestClass) =
        inferImplicit(tree, appliedType(manifestClass, tp), true, false, context).tree

      def findSubManifest(tp: Type) = findManifest(tp, if (full) FullManifestClass else OptManifestClass)
      def mot(tp0: Type, from: List[Symbol], to: List[Type]): SearchResult = {
        implicit def wrapResult(tree: Tree): SearchResult =
          if (tree == EmptyTree) SearchFailure else new SearchResult(tree, if (from.isEmpty) EmptyTreeTypeSubstituter else new TreeTypeSubstituter(from, to))

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
              val classarg = tp match {
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
                                  findManifest(tp.bounds.lo), findManifest(tp.bounds.hi))
            }
            // looking for a manifest of a type parameter that hasn't been inferred by now,
            // can't do much, but let's not fail
            else if (undetParams contains sym) {
              // #3859: need to include the mapping from sym -> NothingClass.tpe in the SearchResult
              mot(NothingClass.tpe, sym :: from, NothingClass.tpe :: to)
            } else {
              // a manifest should have been found by normal searchImplicit
              EmptyTree
            }
          case RefinedType(parents, decls) =>
            // refinement only generated if type has only one parent
            if (hasLength(parents, 1)) {
              val entries: List[Symbol] = (decls.toList filter { entry =>
                !entry.isConstructor && entry.allOverriddenSymbols.isEmpty && !entry.isPrivate
              })
              val names: List[String] = entries map { _.name.toString }
              val namesTrees: List[Tree] = names map { name => Literal(Constant(name)) }
              val namesTree: Tree = Apply(Select(gen.mkAttributedRef(ListModule), nme.apply), namesTrees)
              val maniTrees: List[Tree] = entries map { sym =>
                val tp = sym.tpe match {
                  case NullaryMethodType(tp) => tp
                  case tp => tp
                }
                findManifest(tp)
              }
              val maniTree: Tree = Apply(Select(gen.mkAttributedRef(ListModule), nme.apply), maniTrees)
              manifestFactoryCall("refinedType", tp, findManifest(parents.head), namesTree, maniTree)
            }
            else if (full) manifestFactoryCall("intersectionType", tp, parents map findSubManifest: _*)
            else mot(erasure.intersectionDominator(parents), from, to)
          case ExistentialType(tparams, result) =>
            mot(tp1.skolemizeExistential, from, to)
          case NullaryMethodType(result) => // TODO: necessary?
            mot(result, from, to)
          case _ =>
            EmptyTree
          }
      }

      if (full) {
        val tagInScope = resolveTypeTag(pos, NoType, tp, concrete = true, allowMaterialization = false)
        if (tagInScope.isEmpty) mot(tp, Nil, Nil)
        else {
          if (ReflectRuntimeUniverse == NoSymbol) {
            // todo. write a test for this
            context.error(pos,
              sm"""to create a manifest here, it is necessary to interoperate with the type tag `$tagInScope` in scope.
                  |however typetag -> manifest conversion requires Scala reflection, which is not present on the classpath.
                  |to proceed put scala-reflect.jar on your compilation classpath and recompile.""")
            return SearchFailure
          }
          if (resolveClassTag(pos, tp, allowMaterialization = true) == EmptyTree) {
            context.error(pos,
              sm"""to create a manifest here, it is necessary to interoperate with the type tag `$tagInScope` in scope.
                  |however typetag -> manifest conversion requires a class tag for the corresponding type to be present.
                  |to proceed add a class tag to the type `$tp` (e.g. by introducing a context bound) and recompile.""")
            return SearchFailure
          }
          val cm = typed(Ident(ReflectRuntimeCurrentMirror))
          val interop = gen.mkMethodCall(ReflectRuntimeUniverse, nme.typeTagToManifest, List(tp), List(cm, tagInScope))
          wrapResult(interop)
        }
      } else {
        mot(tp, Nil, Nil) match {
          case SearchFailure if opt => wrapResult(gen.mkAttributedRef(NoManifest))
          case result               => result
        }
      }
    }

    def wrapResult(tree: Tree): SearchResult =
      if (tree == EmptyTree) SearchFailure else new SearchResult(tree, EmptyTreeTypeSubstituter)

    /** Materializes implicits of predefined types (currently, manifests and tags).
     *  Will be replaced by implicit macros once we fix them.
     */
    private def materializeImplicit(pt: Type): SearchResult =
      pt match {
        case TypeRef(_, sym, _) if sym.isAbstractType =>
          materializeImplicit(pt.dealias.bounds.lo) // #3977: use pt.dealias, not pt (if pt is a type alias, pt.bounds.lo == pt)
        case pt @ TypeRef(pre, sym, arg :: Nil) =>
          sym match {
            case sym if ManifestSymbols(sym) => manifestOfType(arg, sym)
            case sym if TagSymbols(sym) => tagOfType(pre, arg, sym)
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
      val failstart = if (Statistics.canEnable) Statistics.startTimer(inscopeFailNanos) else null
      val succstart = if (Statistics.canEnable) Statistics.startTimer(inscopeSucceedNanos) else null

      var result = searchImplicit(context.implicitss, true)

      // invoke update on implicitly found SourceContext
      var updateSourceContext = true

      if (result.isFailure) {
        if (Statistics.canEnable) Statistics.stopTimer(inscopeFailNanos, failstart)
      } else {
        if (Statistics.canEnable) Statistics.stopTimer(inscopeSucceedNanos, succstart)
        if (Statistics.canEnable) Statistics.incCounter(inscopeImplicitHits)
      }
      if (result.isFailure) {
        val previousErrs = context.flushAndReturnBuffer()
        val failstart = if (Statistics.canEnable) Statistics.startTimer(oftypeFailNanos) else null
        val succstart = if (Statistics.canEnable) Statistics.startTimer(oftypeSucceedNanos) else null

        val wasAmbigious = result.isAmbiguousFailure // SI-6667, never search companions after an ambiguous error in in-scope implicits
        result = materializeImplicit(pt)

        // `materializeImplicit` does some preprocessing for `pt`
        // is it only meant for manifests/tags or we need to do the same for `implicitsOfExpectedType`?
        if (result.isFailure) result = searchImplicit(implicitsOfExpectedType, false)

        if (result.isFailure) {
          pt.dealias match {
            case TypeRef(_, SourceContextClass, _) =>
              // construct new SourceContext instance
              result = sourceInfo(this, context0, tree)
              // there is no existing SourceContext to chain with
              updateSourceContext = false
            case _ =>
          }
          context.updateBuffer(previousErrs)
          if (Statistics.canEnable) Statistics.stopTimer(oftypeFailNanos, failstart)
        } else {
          if (wasAmbigious && settings.lint.value)
            reporter.warning(tree.pos,
              "Search of in-scope implicits was ambiguous, and the implicit scope was searched. In Scala 2.11.0, this code will not compile. See SI-6667. \n" +
                previousErrs.map(_.errMsg).mkString("\n"))

          if (Statistics.canEnable) Statistics.stopTimer(oftypeSucceedNanos, succstart)
          if (Statistics.canEnable) Statistics.incCounter(oftypeImplicitHits)
        }
      }

      if (result.isFailure && settings.debug.value)
        log("no implicits found for "+pt+" "+pt.typeSymbol.info.baseClasses+" "+implicitsOfExpectedType)

      updatedWithSourceContext(this, tree, pt, context0, result, updateSourceContext)
    }

    def allImplicits: List[SearchResult] = {
      def search(iss: Infoss, isLocal: Boolean) = applicableInfos(iss, isLocal).values
      (search(context.implicitss, true) ++ search(implicitsOfExpectedType, false)).toList.filter(_.tree ne EmptyTree)
    }

    // find all implicits for some type that contains type variables
    // collect the constraints that result from typing each implicit
    def allImplicitsPoly(tvars: List[TypeVar]): List[(SearchResult, List[TypeConstraint])] = {
      def resetTVars() = tvars foreach { _.constr = new TypeConstraint }

      def eligibleInfos(iss: Infoss, isLocal: Boolean) = {
        val eligible = new ImplicitComputation(iss, isLocal).eligible
        eligible.toList.flatMap {
          (ii: ImplicitInfo) =>
        // each ImplicitInfo contributes a distinct set of constraints (generated indirectly by typedImplicit)
        // thus, start each type var off with a fresh for every typedImplicit
        resetTVars()
        // any previous errors should not affect us now
        context.flushBuffer()

            val res = typedImplicit(ii, ptChecked = false, isLocal)
        if (res.tree ne EmptyTree) List((res, tvars map (_.constr)))
        else Nil
      }
    }
      eligibleInfos(context.implicitss, isLocal = true) ++ eligibleInfos(implicitsOfExpectedType, isLocal = false)
  }
  }

  object ImplicitNotFoundMsg {
    def unapply(sym: Symbol): Option[(Message)] = sym.implicitNotFoundMsg match {
      case Some(m) => Some(new Message(sym, m))
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
      sym.getAnnotation(ImplicitNotFoundClass).flatMap(_.stringArg(0) match {
        case Some(m) => new Message(sym, m).validate
        case None => Some("Missing argument `msg` on implicitNotFound annotation.")
      })


    class Message(sym: Symbol, msg: String) {
      // http://dcsobral.blogspot.com/2010/01/string-interpolation-in-scala-with.html
      private def interpolate(text: String, vars: Map[String, String]) = {
        """\$\{([^}]+)\}""".r.replaceAllIn(text, (_: Regex.Match) match {
          case Regex.Groups(v) => java.util.regex.Matcher.quoteReplacement(vars.getOrElse(v, "")) // #3915: need to quote replacement string since it may include $'s (such as the interpreter's $iw)
        })}

      private lazy val typeParamNames: List[String] = sym.typeParams.map(_.decodedName)

      def format(paramName: Name, paramTp: Type): String = format(paramTp.typeArgs map (_.toString))
      def format(typeArgs: List[String]): String =
        interpolate(msg, Map((typeParamNames zip typeArgs): _*)) // TODO: give access to the name and type of the implicit argument, etc?

      def validate: Option[String] = {
        import scala.util.matching.Regex; import scala.collection.breakOut
        // is there a shorter way to avoid the intermediate toList?
        val refs = """\$\{([^}]+)\}""".r.findAllIn(msg).matchData.map(_ group 1).toSet
        val decls = typeParamNames.toSet

        (refs &~ decls) match {
          case s if s.isEmpty => None
          case unboundNames =>
            val singular = unboundNames.size == 1
            Some("The type parameter"+( if(singular) " " else "s " )+ unboundNames.mkString(", ")  +
                  " referenced in the message of the @implicitNotFound annotation "+( if(singular) "is" else "are" )+
                  " not defined by "+ sym +".")
        }
      }
    }
  }
}

object ImplicitsStats {

  import scala.reflect.internal.TypesStats._

  val rawTypeImpl         = Statistics.newSubCounter ("  of which in implicits", rawTypeCount)
  val subtypeImpl         = Statistics.newSubCounter("  of which in implicit", subtypeCount)
  val findMemberImpl      = Statistics.newSubCounter("  of which in implicit", findMemberCount)
  val subtypeAppInfos     = Statistics.newSubCounter("  of which in app impl", subtypeCount)
  val subtypeImprovCount  = Statistics.newSubCounter("  of which in improves", subtypeCount)
  val implicitSearchCount = Statistics.newCounter   ("#implicit searches", "typer")
  val triedImplicits      = Statistics.newSubCounter("  #tried", implicitSearchCount)
  val plausiblyCompatibleImplicits
                                  = Statistics.newSubCounter("  #plausibly compatible", implicitSearchCount)
  val matchingImplicits   = Statistics.newSubCounter("  #matching", implicitSearchCount)
  val typedImplicits      = Statistics.newSubCounter("  #typed", implicitSearchCount)
  val foundImplicits      = Statistics.newSubCounter("  #found", implicitSearchCount)
  val improvesCount       = Statistics.newSubCounter("implicit improves tests", implicitSearchCount)
  val improvesCachedCount = Statistics.newSubCounter("#implicit improves cached ", implicitSearchCount)
  val inscopeImplicitHits = Statistics.newSubCounter("#implicit inscope hits", implicitSearchCount)
  val oftypeImplicitHits  = Statistics.newSubCounter("#implicit oftype hits ", implicitSearchCount)
  val implicitNanos       = Statistics.newSubTimer  ("time spent in implicits", typerNanos)
  val inscopeSucceedNanos = Statistics.newSubTimer  ("  successful in scope", typerNanos)
  val inscopeFailNanos    = Statistics.newSubTimer  ("  failed in scope", typerNanos)
  val oftypeSucceedNanos  = Statistics.newSubTimer  ("  successful of type", typerNanos)
  val oftypeFailNanos     = Statistics.newSubTimer  ("  failed of type", typerNanos)
  val subtypeETNanos      = Statistics.newSubTimer  ("  assembling parts", typerNanos)
  val matchesPtNanos      = Statistics.newSubTimer  ("  matchesPT", typerNanos)
  val implicitCacheAccs   = Statistics.newCounter   ("implicit cache accesses", "typer")
  val implicitCacheHits   = Statistics.newSubCounter("implicit cache hits", implicitCacheAccs)
}

// only used when -Xdivergence211 is turned off
class DivergentImplicit extends Exception
object DivergentImplicit extends DivergentImplicit
