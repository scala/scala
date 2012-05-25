/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

//todo: rewrite or disllow new T where T is a mixin (currently: <init> not a member of T)
//todo: use inherited type info also for vars and values
//todo: disallow C#D in superclass
//todo: treat :::= correctly

package scala.tools.nsc
package typechecker

import annotation.tailrec
import scala.collection.{ mutable, immutable }
import mutable.{ LinkedHashMap, ListBuffer }
import scala.util.matching.Regex
import symtab.Flags._
import util.Statistics._

/** This trait provides methods to find various kinds of implicits.
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait Implicits {
  self: Analyzer =>

  import global._
  import definitions._
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

    val rawTypeStart    = startCounter(rawTypeImpl)
    val findMemberStart = startCounter(findMemberImpl)
    val subtypeStart    = startCounter(subtypeImpl)
    val start = startTimer(implicitNanos)
    if (printInfers && !tree.isEmpty && !context.undetparams.isEmpty)
      printTyping("typing implicit: %s %s".format(tree, context.undetparamsString))
    val implicitSearchContext = context.makeImplicit(reportAmbiguous)
    val result = new ImplicitSearch(tree, pt, isView, implicitSearchContext, pos).bestImplicit
    if (saveAmbiguousDivergent && implicitSearchContext.hasErrors) {
      context.updateBuffer(implicitSearchContext.errBuffer.filter(err => err.kind == ErrorKinds.Ambiguous || err.kind == ErrorKinds.Divergent))
      debugwarn("update buffer: " + implicitSearchContext.errBuffer)
    }
    printInference("[infer implicit] inferred " + result)
    context.undetparams = context.undetparams filterNot result.subst.from.contains

    stopTimer(implicitNanos, start)
    stopCounter(rawTypeImpl, rawTypeStart)
    stopCounter(findMemberImpl, findMemberStart)
    stopCounter(subtypeImpl, subtypeStart)
    deindentTyping()
    printTyping("Implicit search yielded: "+ result)
    result
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
  }

  lazy val SearchFailure = new SearchResult(EmptyTree, EmptyTreeTypeSubstituter)

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
      case MethodType(params, restpe) =>
        params.exists(_.tpe.isError) || containsError(restpe)
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
      incCounter(improvesCount)
      (info2 == NoImplicitInfo) ||
      (info1 != NoImplicitInfo) && {
        if (info1.sym.isStatic && info2.sym.isStatic) {
          improvesCache get (info1, info2) match {
            case Some(b) => incCounter(improvesCachedCount); b
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
     *  if one or both are intersection types with a pair of overlapiing parent types.
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

    incCounter(implicitSearchCount)

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
     *  @pre           `info.tpe` does not contain an error
     */
    private def typedImplicit(info: ImplicitInfo, ptChecked: Boolean): SearchResult = {
      (context.openImplicits find { case (tp, tree1) => tree1.symbol == tree.symbol && dominates(pt, tp)}) match {
         case Some(pending) =>
           //println("Pending implicit "+pending+" dominates "+pt+"/"+undetParams) //@MDEBUG
           throw DivergentImplicit
         case None =>
           try {
             context.openImplicits = (pt, tree) :: context.openImplicits
             // println("  "*context.openImplicits.length+"typed implicit "+info+" for "+pt) //@MDEBUG
             typedImplicit0(info, ptChecked)
           } catch {
             case ex: DivergentImplicit =>
               //println("DivergentImplicit for pt:"+ pt +", open implicits:"+context.openImplicits) //@MDEBUG
               if (context.openImplicits.tail.isEmpty) {
                 if (!(pt.isErroneous))
                   DivergingImplicitExpansionError(tree, pt, info.sym)(context)
                 SearchFailure
               } else {
                 throw DivergentImplicit
               }
           } finally {
             context.openImplicits = context.openImplicits.tail
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
      val start = startTimer(matchesPtNanos)
      val result = normSubType(tp, pt) || isView && {
        pt match {
          case TypeRef(_, Function1.Sym, args) =>
            matchesPtView(tp, args.head, args.tail.head, undet)
          case _ =>
            false
        }
      }
      stopTimer(matchesPtNanos, start)
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
              if (sym.isAliasType) loop(tp, pt.normalize)
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
    private def isImpossibleSubType(tp1: Type, tp2: Type) = tp1.normalize.widen match {
      case tr1 @ TypeRef(_, sym1, _) =>
        // We can only rule out a subtype relationship if the left hand
        // side is a class, else we may not know enough.
        sym1.isClass && (tp2.normalize.widen match {
          case TypeRef(_, sym2, _) =>
             sym2.isClass && !(sym1 isWeakSubClass sym2)
          case RefinedType(parents, decls) =>
            decls.nonEmpty &&
            tr1.member(decls.head.name) == NoSymbol
          case _ => false
        })
      case _ => false
    }

    private def typedImplicit0(info: ImplicitInfo, ptChecked: Boolean): SearchResult = {
      incCounter(plausiblyCompatibleImplicits)
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
        typedImplicit1(info)
      else
        SearchFailure
    }

    private def typedImplicit1(info: ImplicitInfo): SearchResult = {
      incCounter(matchingImplicits)

      val itree = atPos(pos.focus) {
        if (info.pre == NoPrefix) Ident(info.name)
        else {
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
          return fail("typed implicit %s has errors".format(info.sym.fullLocationString))

        incCounter(typedImplicits)

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
          fail("hasMatchingSymbol reported threw error(s)")
        else if (!hasMatchingSymbol(itree1))
          fail("candidate implicit %s is shadowed by other implicit %s".format(
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
              return fail("type parameters weren't correctly instantiated outside of the implicit tree")

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
              fail("typing TypeApply reported errors for the implicit tree")
            else {
              val result = new SearchResult(itree2, subst)
              incCounter(foundImplicits)
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

    // #3453: in addition to the implicit symbols that may shadow the implicit with
    // name `name`, this method tests whether there's a non-implicit symbol with name
    // `name` in scope.  Inspired by logic in typedIdent.
    private def nonImplicitSynonymInScope(name: Name) = {
      // the implicit ones are handled by the `shadowed` set above
      context.scope.lookupEntry(name) match {
        case x: ScopeEntry  => reallyExists(x.sym) && !x.sym.isImplicit
        case _              => false
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
     *  @param  shadowed  set in which to record names that are shadowed by implicit infos
     *                    If it is null, no shadowing.
     */
    class ImplicitComputation(iss: Infoss, shadowed: util.HashSet[Name]) {
      private var best: SearchResult = SearchFailure
      private def isShadowed(name: Name) = (
           (shadowed != null)
        && (shadowed(name) || nonImplicitSynonymInScope(name))
      )
      private def isIneligible(info: ImplicitInfo) = (
           info.isCyclicOrErroneous
        || isView && isPredefMemberNamed(info.sym, nme.conforms)
        || isShadowed(info.name)
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

      /** Sorted list of eligible implicits.
       */
      val eligible = {
        val matches = iss flatMap { is =>
          val result = is filter (info => checkValid(info.sym) && survives(info))
          if (shadowed ne null)
            shadowed addEntries (is map (_.name))

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
          def tryImplicitInfo(i: ImplicitInfo) =
            try typedImplicit(i, true)
            catch divergenceHandler

          tryImplicitInfo(i) match {
            case SearchFailure  =>
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
      def findAll() = mapFrom(eligible)(typedImplicit(_, false))

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
                return SearchFailure // Stop the search once ambiguity is encountered, see t4457_2.scala
              case _                =>
                if (isView) chosen.useCountView += 1
                else chosen.useCountArg += 1
            }
        }

        if (best == SearchFailure) {
          /** If there is no winner, and we witnessed and caught divergence,
           *  now we can throw it for the error message.
           */
          if (divergence)
            throw DivergentImplicit

          if (invalidImplicits.nonEmpty)
            setAddendum(pos, () =>
              "\n Note: implicit "+invalidImplicits.head+" is not applicable here"+
              " because it comes after the application point and it lacks an explicit result type")
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
      val start       = startCounter(subtypeAppInfos)
      val computation = new ImplicitComputation(iss, if (isLocal) util.HashSet[Name](512) else null) { }
      val applicable  = computation.findAll()

      stopCounter(subtypeAppInfos, start)
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
      else new ImplicitComputation(implicitInfoss, if (isLocal) util.HashSet[Name](128) else null) findBest()

    /** Produce an implicict info map, i.e. a map from the class symbols C of all parts of this type to
     *  the implicit infos in the companion objects of these class symbols C.
     * The parts of a type is the smallest set of types that contains
     *    - the type itself
     *    - the parts of its immediate components (prefix and argument)
     *    - the parts of its base types
     *    - for alias types and abstract types, we take instead the parts
     *    - of their upper bounds.
     *  @return For those parts that refer to classes with companion objects that
     *  can be accessed with unambiguous stable prefixes, the implicits infos
     *  which are members of these companion objects.
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
                println("amb prefix: "+pre+"#"+sym+" "+infos1.head.pre.prefix+"#"+sym)
                infoMap(sym) = List() // ambiguous prefix - ignore implicit members
              }
            case None =>
              if (pre.isStable) {
                val companion = companionSymbolOf(sym, context)
                companion.moduleClass match {
                  case mc: ModuleClassSymbol =>
                    val infos =
                      for (im <- mc.implicitMembers) yield new ImplicitInfo(im.name, singleType(pre, companion), im)
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
              getParts(tp.normalize)
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

    /** The parts of a type is the smallest set of types that contains
     *    - the type itself
     *    - the parts of its immediate components (prefix and argument)
     *    - the parts of its base types
     *    - for alias types and abstract types, we take instead the parts
     *    - of their upper bounds.
     *  @return For those parts that refer to classes with companion objects that
     *  can be accessed with unambiguous stable prefixes, the implicits infos
     *  which are members of these companion objects.

    private def companionImplicits(tp: Type): Infoss = {
      val partMap = new LinkedHashMap[Symbol, Type]
      val seen = mutable.HashSet[Type]()  // cycle detection

      /** Enter all parts of `tp` into `parts` set.
       *  This method is performance critical: about 2-4% of all type checking is spent here
       */
      def getParts(tp: Type) {
        if (seen(tp))
          return
        seen += tp
        tp match {
          case TypeRef(pre, sym, args) =>
            if (sym.isClass) {
              if (!((sym.name == tpnme.REFINE_CLASS_NAME) ||
                    (sym.name startsWith tpnme.ANON_CLASS_NAME) ||
                    (sym.name == tpnme.ROOT)))
                partMap get sym match {
                  case Some(pre1) =>
                    if (!(pre =:= pre1)) partMap(sym) = NoType // ambiguous prefix - ignore implicit members
                  case None =>
                    if (pre.isStable) partMap(sym) = pre
                    val bts = tp.baseTypeSeq
                    var i = 1
                    while (i < bts.length) {
                      getParts(bts(i))
                      i += 1
                    }
                    getParts(pre)
                    args foreach getParts
                }
            } else if (sym.isAliasType) {
              getParts(tp.normalize)
            } else if (sym.isAbstractType) {
              getParts(tp.bounds.hi)
            }
          case ThisType(_) =>
            getParts(tp.widen)
          case _: SingletonType =>
            getParts(tp.widen)
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

      getParts(tp)

      val buf = new ListBuffer[Infos]
      for ((clazz, pre) <- partMap) {
        if (pre != NoType) {
          val companion = clazz.companionModule
          companion.moduleClass match {
            case mc: ModuleClassSymbol =>
              buf += (mc.implicitMembers map (im =>
                new ImplicitInfo(im.name, singleType(pre, companion), im)))
            case _ =>
          }
        }
      }
      //println("companion implicits of "+tp+" = "+buf.toList) // DEBUG
      buf.toList
    }

*/

    /** The implicits made available by type `pt`.
     *  These are all implicits found in companion objects of classes C
     *  such that some part of `tp` has C as one of its superclasses.
     */
    private def implicitsOfExpectedType: Infoss = implicitsCache get pt match {
      case Some(implicitInfoss) =>
        incCounter(implicitCacheHits)
        implicitInfoss
      case None                 =>
        incCounter(implicitCacheMisses)
        val start = startTimer(subtypeETNanos)
//        val implicitInfoss = companionImplicits(pt)
        val implicitInfoss1 = companionImplicitMap(pt).valuesIterator.toList
//        val is1 = implicitInfoss.flatten.toSet
//        val is2 = implicitInfoss1.flatten.toSet
//        for (i <- is1)
//          if (!(is2 contains i)) println("!!! implicit infos of "+pt+" differ, new does not contain "+i+",\nold: "+implicitInfoss+",\nnew: "+implicitInfoss1)
//        for (i <- is2)
//          if (!(is1 contains i)) println("!!! implicit infos of "+pt+" differ, old does not contain "+i+",\nold: "+implicitInfoss+",\nnew: "+implicitInfoss1)
        stopTimer(subtypeETNanos, start)
        implicitsCache(pt) = implicitInfoss1
        if (implicitsCache.size >= sizeLimit)
          implicitsCache -= implicitsCache.keysIterator.next
        implicitInfoss1
    }

    private def TagSymbols =  TagMaterializers.keySet
    private val TagMaterializers = Map[Symbol, Symbol](
      ArrayTagClass        -> MacroInternal_materializeArrayTag,
      ErasureTagClass      -> MacroInternal_materializeErasureTag,
      ClassTagClass        -> MacroInternal_materializeClassTag,
      TypeTagClass         -> MacroInternal_materializeTypeTag,
      ConcreteTypeTagClass -> MacroInternal_materializeConcreteTypeTag
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
          val tree1 = typed(atPos(pos.focus)(arg))
          if (context.hasErrors) processMacroExpansionError(context.errBuffer.head.errPos, context.errBuffer.head.errMsg)
          else new SearchResult(tree1, EmptyTreeTypeSubstituter)
        } catch {
          case ex: TypeError =>
            processMacroExpansionError(ex.pos, ex.msg)
        }
      }

      val prefix = (
        // ClassTags only exist for scala.reflect.mirror, so their materializer
        // doesn't care about prefixes
        if ((tagClass eq ArrayTagClass) || (tagClass eq ErasureTagClass) || (tagClass eq ClassTagClass)) ReflectMirrorPrefix
        else pre match {
          // [Eugene to Martin] this is the crux of the interaction between
          // implicits and reifiers here we need to turn a (supposedly
          // path-dependent) type into a tree that will be used as a prefix I'm
          // not sure if I've done this right - please, review
          case SingleType(prePre, preSym) =>
            gen.mkAttributedRef(prePre, preSym) setType pre
          // necessary only to compile typetags used inside the Universe cake
          case ThisType(thisSym) =>
            gen.mkAttributedThis(thisSym)
          case _ =>
            // if ``pre'' is not a PDT, e.g. if someone wrote
            //   implicitly[scala.reflect.makro.Context#TypeTag[Int]]
            // then we need to fail, because we don't know the prefix to use during type reification
            return failure(tp, "tag error: unsupported prefix type %s (%s)".format(pre, pre.kind))
        }
      )
      // todo. migrate hardcoded materialization in Implicits to corresponding implicit macros
      var materializer = atPos(pos.focus)(
        gen.mkMethodCall(TagMaterializers(tagClass), List(tp), List(prefix))
      )
      if (settings.XlogImplicits.value) println("materializing requested %s.%s[%s] using %s".format(pre, tagClass.name, tp, materializer))
      if (context.macrosEnabled) success(materializer)
      else failure(materializer, "macros are disabled")
    }

    private val ManifestSymbols = Set[Symbol](PartialManifestClass, FullManifestClass, OptManifestClass)

    /** Creates a tree that calls the relevant factory method in object
      * reflect.Manifest for type 'tp'. An EmptyTree is returned if
      * no manifest is found. todo: make this instantiate take type params as well?
      */
    private def manifestOfType(tp: Type, full: Boolean): SearchResult = {

      /** Creates a tree that calls the factory method called constructor in object reflect.Manifest */
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

        val tp1 = tp0.normalize
        tp1 match {
          case ThisType(_) | SingleType(_, _) =>
            // can't generate a reference to a value that's abstracted over by an existential
            if (containsExistential(tp1)) EmptyTree
            else manifestFactoryCall("singleType", tp, gen.mkAttributedQualifier(tp1))
          case ConstantType(value) =>
            manifestOfType(tp1.deconst, full)
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
          case RefinedType(parents, decls) => // !!! not yet: if !full || decls.isEmpty =>
            // refinement is not generated yet
            if (hasLength(parents, 1)) findManifest(parents.head)
            else if (full) manifestFactoryCall("intersectionType", tp, parents map findSubManifest: _*)
            else mot(erasure.intersectionDominator(parents), from, to)
          case ExistentialType(tparams, result) =>
            mot(tp1.skolemizeExistential, from, to)
          case _ =>
            EmptyTree
/*          !!! the following is almost right, but we have to splice nested manifest
 *          !!! types into this type. This requires a substantial extension of
 *          !!! reifiers.
            val reifier = new Reifier()
            val rtree = reifier.reifyTopLevel(tp1)
            manifestFactoryCall("apply", tp, rtree)
*/
          }
      }

      val tagInScope =
        if (full) context.withMacrosDisabled(resolveTypeTag(ReflectMirrorPrefix.tpe, tp, pos, true))
        else context.withMacrosDisabled(resolveArrayTag(tp, pos))
      if (tagInScope.isEmpty) mot(tp, Nil, Nil)
      else {
        val interop =
          if (full) gen.mkMethodCall(ReflectPackage, nme.concreteTypeTagToManifest, List(tp), List(tagInScope))
          else gen.mkMethodCall(ReflectPackage, nme.arrayTagToClassManifest, List(tp), List(tagInScope))
        wrapResult(interop)
      }
    }

    def wrapResult(tree: Tree): SearchResult =
      if (tree == EmptyTree) SearchFailure else new SearchResult(tree, EmptyTreeTypeSubstituter)

    /** The tag corresponding to type `pt`, provided `pt` is a flavor of a tag.
     */
    private def implicitTagOrOfExpectedType(pt: Type): SearchResult = pt.dealias match {
      case TypeRef(pre, sym, arg :: Nil) if ManifestSymbols(sym) =>
        manifestOfType(arg, sym == FullManifestClass) match {
          case SearchFailure if sym == OptManifestClass => wrapResult(gen.mkAttributedRef(NoManifest))
          case result                                   => result
        }
      case TypeRef(pre, sym, arg :: Nil) if TagSymbols(sym) =>
        tagOfType(pre, arg, sym)
      case tp@TypeRef(_, sym, _) if sym.isAbstractType =>
        implicitTagOrOfExpectedType(tp.bounds.lo) // #3977: use tp (==pt.dealias), not pt (if pt is a type alias, pt.bounds.lo == pt)
      case _ =>
        searchImplicit(implicitsOfExpectedType, false)
        // shouldn't we pass `pt` to `implicitsOfExpectedType`, or is the recursive case
        // for an abstract type really only meant for tags?
    }

    /** The result of the implicit search:
     *  First search implicits visible in current context.
     *  If that fails, search implicits in expected type `pt`.
     *  // [Eugene] the following two lines should be deleted after we migrate delegate tag materialization to implicit macros
     *  If that fails, and `pt` is an instance of a ClassTag, try to construct a class tag.
     *  If that fails, and `pt` is an instance of a TypeTag, try to construct a type tag.
     *  If all fails return SearchFailure
     */
    def bestImplicit: SearchResult = {
      val failstart = startTimer(inscopeFailNanos)
      val succstart = startTimer(inscopeSucceedNanos)

      var result = searchImplicit(context.implicitss, true)

      if (result == SearchFailure) {
        stopTimer(inscopeFailNanos, failstart)
      } else {
        stopTimer(inscopeSucceedNanos, succstart)
        incCounter(inscopeImplicitHits)
      }
      if (result == SearchFailure) {
        val previousErrs = context.flushAndReturnBuffer()
        val failstart = startTimer(oftypeFailNanos)
        val succstart = startTimer(oftypeSucceedNanos)

        result = implicitTagOrOfExpectedType(pt)

        if (result == SearchFailure) {
          context.updateBuffer(previousErrs)
          stopTimer(oftypeFailNanos, failstart)
        } else {
          stopTimer(oftypeSucceedNanos, succstart)
          incCounter(oftypeImplicitHits)
        }
      }

      if (result == SearchFailure && settings.debug.value)
        log("no implicits found for "+pt+" "+pt.typeSymbol.info.baseClasses+" "+implicitsOfExpectedType)

      result
    }

    def allImplicits: List[SearchResult] = {
      def search(iss: Infoss, isLocal: Boolean) = applicableInfos(iss, isLocal).values
      (search(context.implicitss, true) ++ search(implicitsOfExpectedType, false)).toList.filter(_.tree ne EmptyTree)
    }

    // find all implicits for some type that contains type variables
    // collect the constraints that result from typing each implicit
    def allImplicitsPoly(tvars: List[TypeVar]): List[(SearchResult, List[TypeConstraint])] = {
      def resetTVars() = tvars foreach { _.constr = new TypeConstraint }

      def eligibleInfos(iss: Infoss, isLocal: Boolean) = new ImplicitComputation(iss, if (isLocal) util.HashSet[Name](512) else null).eligible
      val allEligibleInfos = (eligibleInfos(context.implicitss, true) ++ eligibleInfos(implicitsOfExpectedType, false)).toList

      allEligibleInfos flatMap { ii =>
        // each ImplicitInfo contributes a distinct set of constraints (generated indirectly by typedImplicit)
        // thus, start each type var off with a fresh for every typedImplicit
        resetTVars()
        // any previous errors should not affect us now
        context.flushBuffer()
        val res = typedImplicit(ii, false)
        if (res.tree ne EmptyTree) List((res, tvars map (_.constr)))
        else Nil
      }
    }
  }

  object ImplicitNotFoundMsg {
    def unapply(sym: Symbol): Option[(Message)] = sym.implicitNotFoundMsg map (m => (new Message(sym, m)))
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
        import scala.util.matching.Regex; import collection.breakOut
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
class DivergentImplicit extends Exception
object DivergentImplicit extends DivergentImplicit
