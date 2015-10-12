/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.collection.{ mutable, immutable }
import scala.util.control.ControlThrowable
import symtab.Flags._
import scala.reflect.internal.Depth

/** This trait contains methods related to type parameter inference.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
trait Infer extends Checkable {
  self: Analyzer =>

  import global._
  import definitions._
  import typeDebug.ptBlock
  import typeDebug.str.parentheses
  import typingStack.{ printTyping }

  /** The formal parameter types corresponding to `formals`.
   *  If `formals` has a repeated last parameter, a list of
   *  (numArgs - numFormals + 1) copies of its type is appended
   *  to the other formals. By-name types are replaced with their
   *  underlying type.
   *
   *  @param removeByName allows keeping ByName parameters. Used in NamesDefaults.
   *  @param removeRepeated allows keeping repeated parameter (if there's one argument). Used in NamesDefaults.
   */
  def formalTypes(formals: List[Type], numArgs: Int, removeByName: Boolean = true, removeRepeated: Boolean = true): List[Type] = {
    val numFormals = formals.length
    val formals1   = if (removeByName) formals mapConserve dropByName else formals
    val expandLast = (
         (removeRepeated || numFormals != numArgs)
      && isVarArgTypes(formals1)
    )
    def lastType = formals1.last.dealiasWiden.typeArgs.head
    def expanded(n: Int) = (1 to n).toList map (_ => lastType)

    if (expandLast)
      formals1.init ::: expanded(numArgs - numFormals + 1)
    else
      formals1
  }

  /** Sorts the alternatives according to the given comparison function.
   *  Returns a list containing the best alternative as well as any which
   *  the best fails to improve upon.
   */
  private def bestAlternatives(alternatives: List[Symbol])(isBetter: (Symbol, Symbol) => Boolean): List[Symbol] = {
    def improves(sym1: Symbol, sym2: Symbol) = (
         (sym2 eq NoSymbol)
      || sym2.isError
      || (sym2 hasAnnotation BridgeClass)
      || isBetter(sym1, sym2)
    )

    alternatives sortWith improves match {
      case best :: rest if rest.nonEmpty => best :: rest.filterNot(alt => improves(best, alt))
      case bests                         => bests
    }
  }

  // we must not allow CyclicReference to be thrown when sym.info is called
  // in checkAccessible, because that would mark the symbol erroneous, which it
  // is not. But if it's a true CyclicReference then macro def will report it.
  // See comments to TypeSigError for an explanation of this special case.
  // [Eugene] is there a better way?
  private object CheckAccessibleMacroCycle extends TypeCompleter {
    val tree = EmptyTree
    override def complete(sym: Symbol) = ()
  }

  /** A fresh type variable with given type parameter as origin.
   */
  def freshVar(tparam: Symbol): TypeVar = TypeVar(tparam)

  class NoInstance(msg: String) extends Throwable(msg) with ControlThrowable { }
  private class DeferredNoInstance(getmsg: () => String) extends NoInstance("") {
    override def getMessage(): String = getmsg()
  }
  private def ifNoInstance[T](f: String => T): PartialFunction[Throwable, T] = {
    case x: NoInstance  => f(x.getMessage)
  }

  /** Map every TypeVar to its constraint.inst field.
   *  throw a NoInstance exception if a NoType or WildcardType is encountered.
   */
  object instantiate extends TypeMap {
    private var excludedVars = immutable.Set[TypeVar]()
    private def applyTypeVar(tv: TypeVar): Type = tv match {
      case TypeVar(origin, constr) if !constr.instValid => throw new DeferredNoInstance(() => s"no unique instantiation of type variable $origin could be found")
      case _ if excludedVars(tv)                        => throw new NoInstance("cyclic instantiation")
      case TypeVar(_, constr)                           =>
        excludedVars += tv
        try apply(constr.inst)
        finally excludedVars -= tv
    }
    def apply(tp: Type): Type = tp match {
      case WildcardType | BoundedWildcardType(_) | NoType => throw new NoInstance("undetermined type")
      case tv: TypeVar if !tv.untouchable                 => applyTypeVar(tv)
      case _                                              => mapOver(tp)
    }
  }

  @inline final def falseIfNoInstance(body: => Boolean): Boolean =
    try body catch { case _: NoInstance => false }

  /** Is type fully defined, i.e. no embedded anytypes or wildcards in it?
   */
  private[typechecker] def isFullyDefined(tp: Type): Boolean = tp match {
    case WildcardType | BoundedWildcardType(_) | NoType => false
    case NoPrefix | ThisType(_) | ConstantType(_)       => true
    case TypeRef(pre, _, args)                          => isFullyDefined(pre) && (args forall isFullyDefined)
    case SingleType(pre, _)                             => isFullyDefined(pre)
    case RefinedType(ts, _)                             => ts forall isFullyDefined
    case TypeVar(_, constr) if constr.inst == NoType    => false
    case _                                              => falseIfNoInstance({ instantiate(tp) ; true })
  }

  /** Solve constraint collected in types `tvars`.
   *
   *  @param tvars      All type variables to be instantiated.
   *  @param tparams    The type parameters corresponding to `tvars`
   *  @param variances  The variances of type parameters; need to reverse
   *                    solution direction for all contravariant variables.
   *  @param upper      When `true` search for max solution else min.
   *  @throws NoInstance
   */
  def solvedTypes(tvars: List[TypeVar], tparams: List[Symbol], variances: List[Variance], upper: Boolean, depth: Depth): List[Type] = {
    if (tvars.isEmpty) Nil else {
      printTyping("solving for " + parentheses((tparams, tvars).zipped map ((p, tv) => s"${p.name}: $tv")))
      // !!! What should be done with the return value of "solve", which is at present ignored?
      // The historical commentary says "no panic, it's good enough to just guess a solution,
      // we'll find out later whether it works", meaning don't issue an error here when types
      // don't conform to bounds. That means you can never trust the results of implicit search.
      // For an example where this was not being heeded, SI-2421.
      solve(tvars, tparams, variances, upper, depth)
      tvars map instantiate
    }
  }

  def skipImplicit(tp: Type) = tp match {
    case mt: MethodType if mt.isImplicit  => mt.resultType
    case _                                => tp
  }

  /** Automatically perform the following conversions on expression types:
   *  A method type becomes the corresponding function type.
   *  A nullary method type becomes its result type.
   *  Implicit parameters are skipped.
   *  This method seems to be performance critical.
   */
  def normalize(tp: Type): Type = tp match {
    case PolyType(_, restpe) =>
      logResult(sm"""|Normalizing PolyType in infer:
                     |  was: $restpe
                     |  now""")(normalize(restpe))
    case mt @ MethodType(_, restpe) if mt.isImplicit             => normalize(restpe)
    case mt @ MethodType(_, restpe) if !mt.isDependentMethodType => functionType(mt.paramTypes, normalize(restpe))
    case NullaryMethodType(restpe)                               => normalize(restpe)
    case ExistentialType(tparams, qtpe)                          => newExistentialType(tparams, normalize(qtpe))
    case _                                                       => tp // @MAT aliases already handled by subtyping
  }

  private lazy val stdErrorClass = rootMirror.RootClass.newErrorClass(tpnme.ERROR)
  private lazy val stdErrorValue = stdErrorClass.newErrorValue(nme.ERROR)

  /** The context-dependent inferencer part */
  abstract class Inferencer extends InferencerContextErrors with InferCheckable {
    def context: Context
    import InferErrorGen._

    /* -- Error Messages --------------------------------------------------- */
    def setError[T <: Tree](tree: T): T = {
      // SI-7388, one can incur a cycle calling sym.toString
      // (but it'd be nicer if that weren't so)
      def name = {
        val sym = tree.symbol
        val nameStr = try sym.toString catch { case _: CyclicReference => sym.nameString }
        newTermName(s"<error: $nameStr>")
      }
      def errorClass  = if (context.reportErrors) context.owner.newErrorClass(name.toTypeName) else stdErrorClass
      def errorValue  = if (context.reportErrors) context.owner.newErrorValue(name) else stdErrorValue
      def errorSym    = if (tree.isType) errorClass else errorValue

      if (tree.hasSymbolField)
        tree setSymbol errorSym

      tree setType ErrorType
    }

    def getContext = context

    def explainTypes(tp1: Type, tp2: Type) = {
      if (context.reportErrors)
        withDisambiguation(List(), tp1, tp2)(global.explainTypes(tp1, tp2))
    }

    // When filtering sym down to the accessible alternatives leaves us empty handed.
    private def checkAccessibleError(tree: Tree, sym: Symbol, pre: Type, site: Tree): Tree = {
      if (settings.debug) {
        Console.println(context)
        Console.println(tree)
        Console.println("" + pre + " " + sym.owner + " " + context.owner + " " + context.outer.enclClass.owner + " " + sym.owner.thisType + (pre =:= sym.owner.thisType))
      }
      ErrorUtils.issueTypeError(AccessError(tree, sym, pre, context.enclClass.owner,
        if (settings.check.isDefault)
          analyzer.lastAccessCheckDetails
        else
          ptBlock("because of an internal error (no accessible symbol)",
            "sym.ownerChain"                -> sym.ownerChain,
            "underlyingSymbol(sym)"         -> underlyingSymbol(sym),
            "pre"                           -> pre,
            "site"                          -> site,
            "tree"                          -> tree,
            "sym.accessBoundary(sym.owner)" -> sym.accessBoundary(sym.owner),
            "context.owner"                 -> context.owner,
            "context.outer.enclClass.owner" -> context.outer.enclClass.owner
          )
      ))(context)

      setError(tree)
    }

    /* -- Tests & Checks---------------------------------------------------- */

    /** Check that `sym` is defined and accessible as a member of
     *  tree `site` with type `pre` in current context.
     *  @PP: In case it's not abundantly obvious to anyone who might read
     *  this, the method does a lot more than "check" these things, as does
     *  nearly every method in the compiler, so don't act all shocked.
     *  This particular example "checks" its way to assigning both the
     *  symbol and type of the incoming tree, in addition to forcing lots
     *  of symbol infos on its way to transforming java raw types (but
     *  only of terms - why?)
     *
     * Note: pre is not refchecked -- moreover, refchecking the resulting tree may not refcheck pre,
     *       since pre may not occur in its type (callers should wrap the result in a TypeTreeWithDeferredRefCheck)
     */
    def checkAccessible(tree: Tree, sym: Symbol, pre: Type, site: Tree): Tree = {
      def malformed(ex: MalformedType, instance: Type): Type = {
        val what    = if (ex.msg contains "malformed type") "is malformed" else s"contains a ${ex.msg}"
        val message = s"\n because its instance type $instance $what"
        val error   = AccessError(tree, sym, pre, context.enclClass.owner, message)
        ErrorUtils.issueTypeError(error)(context)
        ErrorType
      }
      def accessible = sym filter (alt => context.isAccessible(alt, pre, site.isInstanceOf[Super])) match {
        case NoSymbol if sym.isJavaDefined && context.unit.isJava => sym  // don't try to second guess Java; see #4402
        case sym1                                                 => sym1
      }
      // XXX So... what's this for exactly?
      if (context.unit.exists)
        context.unit.depends += sym.enclosingTopLevelClass

      if (sym.isError)
        tree setSymbol sym setType ErrorType
      else accessible match {
        case NoSymbol                                                 => checkAccessibleError(tree, sym, pre, site)
        case sym if context.owner.isTermMacro && (sym hasFlag LOCKED) => throw CyclicReference(sym, CheckAccessibleMacroCycle)
        case sym                                                      =>
          val sym1 = if (sym.isTerm) sym.cookJavaRawInfo() else sym // xform java rawtypes into existentials
          val owntype = (
            try pre memberType sym1
            catch { case ex: MalformedType => malformed(ex, pre memberType underlyingSymbol(sym)) }
          )
          tree setSymbol sym1 setType (
            pre match {
              case _: SuperType => owntype map (tp => if (tp eq pre) site.symbol.thisType else tp)
              case _            => owntype
            }
          )
      }
    }

    /** "Compatible" means conforming after conversions.
     *  "Raising to a thunk" is not implicit; therefore, for purposes of applicability and
     *  specificity, an arg type `A` is considered compatible with cbn formal parameter type `=>A`.
     *  For this behavior, the type `pt` must have cbn params preserved; for instance, `formalTypes(removeByName = false)`.
     *
     *  `isAsSpecific` no longer prefers A by testing applicability to A for both m(A) and m(=>A)
     *  since that induces a tie between m(=>A) and m(=>A,B*) [SI-3761]
     */
    private def isCompatible(tp: Type, pt: Type): Boolean = {
      def isCompatibleByName(tp: Type, pt: Type): Boolean = (
           isByNameParamType(pt)
        && !isByNameParamType(tp)
        && isCompatible(tp, dropByName(pt))
      )
      def isCompatibleSam(tp: Type, pt: Type): Boolean = {
        val samFun = typer.samToFunctionType(pt)
        (samFun ne NoType) && isCompatible(tp, samFun)
      }

      val tp1 = normalize(tp)

      (    (tp1 weak_<:< pt)
        || isCoercible(tp1, pt)
        || isCompatibleByName(tp, pt)
        || isCompatibleSam(tp, pt)
      )
    }
    def isCompatibleArgs(tps: List[Type], pts: List[Type]) = (tps corresponds pts)(isCompatible)

    def isWeaklyCompatible(tp: Type, pt: Type): Boolean = {
      def isCompatibleNoParamsMethod = tp match {
        case MethodType(Nil, restpe) => isCompatible(restpe, pt)
        case _                       => false
      }
      (    pt.typeSymbol == UnitClass // can perform unit coercion
        || isCompatible(tp, pt)
        || isCompatibleNoParamsMethod // can perform implicit () instantiation
      )
    }

    /*  Like weakly compatible but don't apply any implicit conversions yet.
     *  Used when comparing the result type of a method with its prototype.
     */
    def isConservativelyCompatible(tp: Type, pt: Type): Boolean =
      context.withImplicitsDisabled(isWeaklyCompatible(tp, pt))

    // Overridden at the point of instantiation, where inferView is visible.
    def isCoercible(tp: Type, pt: Type): Boolean = false

    /* -- Type instantiation------------------------------------------------ */

    /** Replace any (possibly bounded) wildcard types in type `tp`
     *  by existentially bound variables.
     */
    def makeFullyDefined(tp: Type): Type = {
      var tparams: List[Symbol] = Nil
      def addTypeParam(bounds: TypeBounds): Type = {
        val tparam = context.owner.newExistential(newTypeName("_"+tparams.size), context.tree.pos.focus) setInfo bounds
        tparams ::= tparam
        tparam.tpe
      }
      val tp1 = tp map {
        case WildcardType                => addTypeParam(TypeBounds.empty)
        case BoundedWildcardType(bounds) => addTypeParam(bounds)
        case t                           => t
      }
      if (tp eq tp1) tp
      else existentialAbstraction(tparams.reverse, tp1)
    }
    def ensureFullyDefined(tp: Type): Type = if (isFullyDefined(tp)) tp else makeFullyDefined(tp)

    /** Return inferred type arguments of polymorphic expression, given
     *  type vars, its type parameters and result type and a prototype `pt`.
     *  If the type variables cannot be instantiated such that the type
     *  conforms to `pt`, return null.
     */
    private def exprTypeArgs(tvars: List[TypeVar], tparams: List[Symbol], restpe: Type, pt: Type, useWeaklyCompatible: Boolean): List[Type] = {
      def restpeInst = restpe.instantiateTypeParams(tparams, tvars)
      def conforms   = if (useWeaklyCompatible) isWeaklyCompatible(restpeInst, pt) else isCompatible(restpeInst, pt)
      // If the restpe is an implicit method, and the expected type is fully defined
      // optimize type variables wrt to the implicit formals only; ignore the result type.
      // See test pos/jesper.scala
      def variance = restpe match {
        case mt: MethodType if mt.isImplicit && isFullyDefined(pt) => MethodType(mt.params, AnyTpe)
        case _                                                     => restpe
      }
      def solve() = solvedTypes(tvars, tparams, tparams map varianceInType(variance), upper = false, lubDepth(restpe :: pt :: Nil))

      if (conforms)
        try solve() catch { case _: NoInstance => null }
      else
        null
    }
    /** Overload which allocates fresh type vars.
     *  The other one exists because apparently inferExprInstance needs access to the typevars
     *  after the call, and it's wasteful to return a tuple and throw it away almost every time.
     */
    private def exprTypeArgs(tparams: List[Symbol], restpe: Type, pt: Type, useWeaklyCompatible: Boolean): List[Type] =
      exprTypeArgs(tparams map freshVar, tparams, restpe, pt, useWeaklyCompatible)

    /** Return inferred proto-type arguments of function, given
    *  its type and value parameters and result type, and a
    *  prototype `pt` for the function result.
    *  Type arguments need to be either determined precisely by
    *  the prototype, or they are maximized, if they occur only covariantly
    *  in the value parameter list.
    *  If instantiation of a type parameter fails,
    *  take WildcardType for the proto-type argument.
    */
    def protoTypeArgs(tparams: List[Symbol], formals: List[Type], restpe: Type, pt: Type): List[Type] = {
      // Map type variable to its instance, or, if `variance` is variant,
      // to its upper or lower bound
      def instantiateToBound(tvar: TypeVar, variance: Variance): Type = {
        lazy val hiBounds = tvar.constr.hiBounds
        lazy val loBounds = tvar.constr.loBounds
        lazy val upper    = glb(hiBounds)
        lazy val lower    = lub(loBounds)
        def setInst(tp: Type): Type = {
          tvar setInst tp
          assert(tvar.constr.inst != tvar, tvar.origin)
          instantiate(tvar.constr.inst)
        }
        if (tvar.constr.instValid)
          instantiate(tvar.constr.inst)
        else if (loBounds.nonEmpty && variance.isContravariant)
          setInst(lower)
        else if (hiBounds.nonEmpty && (variance.isPositive || loBounds.nonEmpty && upper <:< lower))
          setInst(upper)
        else
          WildcardType
      }

      val tvars = tparams map freshVar
      if (isConservativelyCompatible(restpe.instantiateTypeParams(tparams, tvars), pt))
        map2(tparams, tvars)((tparam, tvar) =>
          try instantiateToBound(tvar, varianceInTypes(formals)(tparam))
          catch { case ex: NoInstance => WildcardType }
        )
      else
        tvars map (_ => WildcardType)
    }

    /** [Martin] Can someone comment this please? I have no idea what it's for
     *  and the code is not exactly readable.
     */
    object AdjustedTypeArgs {
      val Result  = mutable.LinkedHashMap
      type Result = mutable.LinkedHashMap[Symbol, Option[Type]]

      def unapply(m: Result): Some[(List[Symbol], List[Type])] = Some(toLists(
        (m collect {case (p, Some(a)) => (p, a)}).unzip  ))

      object Undets {
        def unapply(m: Result): Some[(List[Symbol], List[Type], List[Symbol])] = Some(toLists{
          val (ok, nok) = m.map{case (p, a) => (p, a.getOrElse(null))}.partition(_._2 ne null)
          val (okArgs, okTparams) = ok.unzip
          (okArgs, okTparams, nok.keys)
        })
      }

      object AllArgsAndUndets {
        def unapply(m: Result): Some[(List[Symbol], List[Type], List[Type], List[Symbol])] = Some(toLists{
          val (ok, nok) = m.map{case (p, a) => (p, a.getOrElse(null))}.partition(_._2 ne null)
          val (okArgs, okTparams) = ok.unzip
          (okArgs, okTparams, m.values.map(_.getOrElse(NothingTpe)), nok.keys)
        })
      }

      private def toLists[A1, A2](pxs: (Iterable[A1], Iterable[A2])) = (pxs._1.toList, pxs._2.toList)
      private def toLists[A1, A2, A3](pxs: (Iterable[A1], Iterable[A2], Iterable[A3])) = (pxs._1.toList, pxs._2.toList, pxs._3.toList)
      private def toLists[A1, A2, A3, A4](pxs: (Iterable[A1], Iterable[A2], Iterable[A3], Iterable[A4])) = (pxs._1.toList, pxs._2.toList, pxs._3.toList, pxs._4.toList)
    }

    /** Retract arguments that were inferred to Nothing because inference failed. Correct types for repeated params.
     *
     * We detect Nothing-due-to-failure by only retracting a parameter if either:
     *  - it occurs in an invariant/contravariant position in `restpe`
     *  - `restpe == WildcardType`
     *
     * Retracted parameters are mapped to None.
     *  TODO:
     *    - make sure the performance hit of storing these in a map is acceptable (it's going to be a small map in 90% of the cases, I think)
     *    - refactor further up the callstack so that we don't have to do this post-factum adjustment?
     *
     * Rewrite for repeated param types:  Map T* entries to Seq[T].
     *  @return map from tparams to inferred arg, if inference was successful, tparams that map to None are considered left undetermined
     *    type parameters that are inferred as `scala.Nothing` and that are not covariant in `restpe` are taken to be undetermined
     */
    def adjustTypeArgs(tparams: List[Symbol], tvars: List[TypeVar], targs: List[Type], restpe: Type = WildcardType): AdjustedTypeArgs.Result  = {
      val buf = AdjustedTypeArgs.Result.newBuilder[Symbol, Option[Type]]

      foreach3(tparams, tvars, targs) { (tparam, tvar, targ) =>
        val retract = (
              targ.typeSymbol == NothingClass                                         // only retract Nothings
          && (restpe.isWildcard || !varianceInType(restpe)(tparam).isPositive)  // don't retract covariant occurrences
        )

        buf += ((tparam,
          if (retract) None
          else Some(
            if (targ.typeSymbol == RepeatedParamClass)     targ.baseType(SeqClass)
            else if (targ.typeSymbol == JavaRepeatedParamClass) targ.baseType(ArrayClass)
            // this infers Foo.type instead of "object Foo" (see also widenIfNecessary)
            else if (targ.typeSymbol.isModuleClass || tvar.constr.avoidWiden) targ
            else targ.widen
          )
        ))
      }
      buf.result()
    }

    /** Return inferred type arguments, given type parameters, formal parameters,
    *  argument types, result type and expected result type.
    *  If this is not possible, throw a `NoInstance` exception.
    *  Undetermined type arguments are represented by `definitions.NothingTpe`.
    *  No check that inferred parameters conform to their bounds is made here.
    *
    *  @param   tparams         the type parameters of the method
    *  @param   formals         the value parameter types of the method
    *  @param   restpe          the result type of the method
    *  @param   argtpes         the argument types of the application
    *  @param   pt              the expected return type of the application
    *  @return  @see adjustTypeArgs
    *
    *  @throws                  NoInstance
    */
    def methTypeArgs(tparams: List[Symbol], formals: List[Type], restpe: Type,
                     argtpes: List[Type], pt: Type): AdjustedTypeArgs.Result = {
      val tvars = tparams map freshVar
      if (!sameLength(formals, argtpes))
        throw new NoInstance("parameter lists differ in length")

      val restpeInst = restpe.instantiateTypeParams(tparams, tvars)

      // first check if typevars can be fully defined from the expected type.
      // The return value isn't used so I'm making it obvious that this side
      // effects, because a function called "isXXX" is not the most obvious
      // side effecter.
      isConservativelyCompatible(restpeInst, pt)

      // Return value unused with the following explanation:
      //
      // Just wait and instantiate from the arguments.  That way,
      // we can try to apply an implicit conversion afterwards.
      // This case could happen if restpe is not fully defined, so the
      // search for an implicit from restpe => pt fails due to ambiguity.
      // See #347.  Therefore, the following two lines are commented out.
      //
      // throw new DeferredNoInstance(() =>
      //   "result type " + normalize(restpe) + " is incompatible with expected type " + pt)

      for (tvar <- tvars)
        if (!isFullyDefined(tvar)) tvar.constr.inst = NoType

      // Then define remaining type variables from argument types.
      map2(argtpes, formals) { (argtpe, formal) =>
        val tp1 = argtpe.deconst.instantiateTypeParams(tparams, tvars)
        val pt1 = formal.instantiateTypeParams(tparams, tvars)

        // Note that isCompatible side-effects: subtype checks involving typevars
        // are recorded in the typevar's bounds (see TypeConstraint)
        if (!isCompatible(tp1, pt1)) {
          throw new DeferredNoInstance(() =>
            "argument expression's type is not compatible with formal parameter type" + foundReqMsg(tp1, pt1))
        }
      }
      val targs = solvedTypes(tvars, tparams, tparams map varianceInTypes(formals), upper = false, lubDepth(formals) max lubDepth(argtpes))
      // Can warn about inferring Any/AnyVal as long as they don't appear
      // explicitly anywhere amongst the formal, argument, result, or expected type.
      // ...or lower bound of a type param, since they're asking for it.
      def canWarnAboutAny = {
        val loBounds = tparams map (_.info.bounds.lo)
        def containsAny(t: Type) = (t contains AnyClass) || (t contains AnyValClass)
        val hasAny = pt :: restpe :: formals ::: argtpes ::: loBounds exists (_.dealiasWidenChain exists containsAny)
        !hasAny
      }
      def argumentPosition(idx: Int): Position = context.tree match {
        case x: ValOrDefDef => x.rhs match {
          case Apply(fn, args) if idx < args.size => args(idx).pos
          case _                                  => context.tree.pos
        }
        case _ => context.tree.pos
      }
      if (settings.warnInferAny && context.reportErrors && canWarnAboutAny) {
        foreachWithIndex(targs) ((targ, idx) =>
          targ.typeSymbol match {
            case sym @ (AnyClass | AnyValClass) =>
              reporter.warning(argumentPosition(idx), s"a type was inferred to be `${sym.name}`; this may indicate a programming error.")
            case _ =>
          }
        )
      }
      adjustTypeArgs(tparams, tvars, targs, restpe)
    }

    /** One must step carefully when assessing applicability due to
     *  complications from varargs, tuple-conversion, named arguments.
     *  This method is used to filter out inapplicable methods,
     *  its behavior slightly configurable based on what stage of
     *  overloading resolution we're at.
     *
     *  This method has boolean parameters, which is usually suboptimal
     *  but I didn't work out a better way.  They don't have defaults,
     *  and the method's scope is limited.
     */
    private[typechecker] def isApplicableBasedOnArity(tpe: Type, argsCount: Int, varargsStar: Boolean, tuplingAllowed: Boolean): Boolean = followApply(tpe) match {
      case OverloadedType(pre, alts) =>
        // followApply may return an OverloadedType (tpe is a value type with multiple `apply` methods)
        alts exists (alt => isApplicableBasedOnArity(pre memberType alt, argsCount, varargsStar, tuplingAllowed))
      case _ =>
        val paramsCount   = tpe.params.length
        // simpleMatch implies we're not using defaults
        val simpleMatch   = paramsCount == argsCount
        val varargsTarget = isVarArgsList(tpe.params)

        // varargsMatch implies we're not using defaults, as varargs and defaults are mutually exclusive
        def varargsMatch  = varargsTarget && (paramsCount - 1) <= argsCount
        // another reason why auto-tupling is a bad idea: it can hide the use of defaults, so must rule those out explicitly
        def tuplingMatch  = tuplingAllowed && eligibleForTupleConversion(paramsCount, argsCount, varargsTarget)
        // varargs and defaults are mutually exclusive, so not using defaults if `varargsTarget`
        // we're not using defaults if there are (at least as many) arguments as parameters (not using exact match to allow for tupling)
        def notUsingDefaults = varargsTarget || paramsCount <= argsCount

        // A varargs star call, e.g. (x, y:_*) can only match a varargs method
        // with the same number of parameters.  See SI-5859 for an example of what
        // would fail were this not enforced before we arrived at isApplicable.
        if (varargsStar)
          varargsTarget && simpleMatch
        else
          simpleMatch || varargsMatch || (tuplingMatch && notUsingDefaults)
    }

    private[typechecker] def followApply(tp: Type): Type = tp match {
      case _ if tp.isError => tp // SI-8228, `ErrorType nonPrivateMember nme.apply` returns an member with an erroneous type!
      case NullaryMethodType(restp) =>
        val restp1 = followApply(restp)
        if (restp1 eq restp) tp else restp1
      case _ =>
        //OPT cut down on #closures by special casing non-overloaded case
        // was: tp.nonPrivateMember(nme.apply) filter (_.isPublic)
        tp nonPrivateMember nme.apply match {
          case NoSymbol                                 => tp
          case sym if !sym.isOverloaded && sym.isPublic => OverloadedType(tp, sym.alternatives)
          case sym                                      => OverloadedType(tp, sym.filter(_.isPublic).alternatives)
        }
    }

    /**
     * Verifies whether the named application is valid. The logic is very
     * similar to the one in NamesDefaults.removeNames.
     *
     * @return a triple (argtpes1, argPos, namesOk) where
     *  - argtpes1 the argument types in named application (assignments to
     *    non-parameter names are treated as assignments, i.e. type Unit)
     *  - argPos a Function1[Int, Int] mapping arguments from their current
     *    to the corresponding position in params
     *  - namesOK is false when there's an invalid use of named arguments
     */
    private def checkNames(argtpes: List[Type], params: List[Symbol]): (List[Type], Array[Int], Boolean) = {
      val argPos = Array.fill(argtpes.length)(-1)
      var positionalAllowed, namesOK = true
      var index = 0
      val argtpes1 = argtpes map {
        case NamedType(name, tp) => // a named argument
          var res = tp
          val pos = params.indexWhere(p => paramMatchesName(p, name) && !p.isSynthetic)

          if (pos == -1) {
            if (positionalAllowed) { // treat assignment as positional argument
              argPos(index) = index
              res = UnitTpe // TODO: this is a bit optimistic, the name may not refer to a mutable variable...
            } else                   // unknown parameter name
              namesOK = false
          } else if (argPos.contains(pos)) { // parameter specified twice
            namesOK = false
          } else {
            if (index != pos)
              positionalAllowed = false
            argPos(index) = pos
          }
          index += 1
          res
        case tp => // a positional argument
          argPos(index) = index
          if (!positionalAllowed)
            namesOK = false // positional after named
          index += 1
          tp
      }
      (argtpes1, argPos, namesOK)
    }

    /** True if the given parameter list can accept a tupled argument list,
     *  and the argument list can be tupled (based on its length.)
     */
    def eligibleForTupleConversion(paramsCount: Int, argsCount: Int, varargsTarget: Boolean): Boolean = {
      def canSendTuple = argsCount match {
        case 0 => !varargsTarget        // avoid () to (()) conversion - SI-3224
        case 1 => false                 // can't tuple a single argument
        case n => n <= MaxTupleArity    // <= 22 arguments
      }
      def canReceiveTuple = paramsCount match {
        case 1 => true
        case 2 => varargsTarget
        case _ => false
      }
      canSendTuple && canReceiveTuple
    }
    def eligibleForTupleConversion(formals: List[Type], argsCount: Int): Boolean = formals match {
      case p :: Nil                                     => eligibleForTupleConversion(1, argsCount, varargsTarget = isScalaRepeatedParamType(p))
      case _ :: p :: Nil if isScalaRepeatedParamType(p) => eligibleForTupleConversion(2, argsCount, varargsTarget = true)
      case _                                            => false
    }

    /** The type of an argument list after being coerced to a tuple.
     *  @pre: the argument list is eligible for tuple conversion.
     */
    private def typeAfterTupleConversion(argtpes: List[Type]): Type = (
      if (argtpes.isEmpty) UnitTpe                 // aka "Tuple0"
      else tupleType(argtpes map {
        case NamedType(name, tp) => UnitTpe  // not a named arg - only assignments here
        case RepeatedType(tp)    => tp       // but probably shouldn't be tupling a call containing :_*
        case tp                  => tp
      })
    )

    /** If the argument list needs to be tupled for the parameter list,
     *  a list containing the type of the tuple.  Otherwise, the original
     *  argument list.
     */
    def tupleIfNecessary(formals: List[Type], argtpes: List[Type]): List[Type] = {
      if (eligibleForTupleConversion(formals, argtpes.size))
        typeAfterTupleConversion(argtpes) :: Nil
      else
        argtpes
    }

    private def isApplicableToMethod(undetparams: List[Symbol], mt: MethodType, argtpes0: List[Type], pt: Type): Boolean = {
      val formals          = formalTypes(mt.paramTypes, argtpes0.length, removeByName = false)
      def missingArgs      = missingParams[Type](argtpes0, mt.params, x => Some(x) collect { case NamedType(n, _) => n })
      def argsTupled       = tupleIfNecessary(mt.paramTypes, argtpes0)
      def argsPlusDefaults = missingArgs match {
        case (args, _) if args forall (_.hasDefault) => argtpes0 ::: makeNamedTypes(args)
        case _                                       => argsTupled
      }
      // If args eq the incoming arg types, fail; otherwise recurse with these args.
      def tryWithArgs(args: List[Type]) = (
           (args ne argtpes0)
        && isApplicable(undetparams, mt, args, pt)
      )
      def tryInstantiating(args: List[Type]) = falseIfNoInstance {
        val restpe = mt resultType args
        val AdjustedTypeArgs.Undets(okparams, okargs, leftUndet) = methTypeArgs(undetparams, formals, restpe, args, pt)
        val restpeInst = restpe.instantiateTypeParams(okparams, okargs)
        // #2665: must use weak conformance, not regular one (follow the monomorphic case above)
        exprTypeArgs(leftUndet, restpeInst, pt, useWeaklyCompatible = true) match {
          case null => false
          case _    => isWithinBounds(NoPrefix, NoSymbol, okparams, okargs)
        }
      }
      def typesCompatible(args: List[Type]) = undetparams match {
        case Nil => isCompatibleArgs(args, formals) && isWeaklyCompatible(mt resultType args, pt)
        case _   => tryInstantiating(args)
      }

      // when using named application, the vararg param has to be specified exactly once
      def reorderedTypesCompatible = checkNames(argtpes0, mt.params) match {
        case (_, _, false)                                                                => false // names are not ok
        case (_, pos, _) if !allArgsArePositional(pos) && !sameLength(formals, mt.params) => false // different length lists and all args not positional
        case (args, pos, _)                                                               => typesCompatible(reorderArgs(args, pos))
      }
      compareLengths(argtpes0, formals) match {
        case 0 if containsNamedType(argtpes0) => reorderedTypesCompatible      // right number of args, wrong order
        case 0                                => typesCompatible(argtpes0)     // fast track if no named arguments are used
        case x if x > 0                       => tryWithArgs(argsTupled)       // too many args, try tupling
        case _                                => tryWithArgs(argsPlusDefaults) // too few args, try adding defaults or tupling
      }
    }

    /** Is there an instantiation of free type variables `undetparams` such that
     *  function type `ftpe` is applicable to `argtpes0` and its result conform to `pt`?
     *
     *  @param ftpe        the type of the function (often a MethodType)
     *  @param argtpes0    the argument types; a NamedType(name, tp) for named
     *    arguments. For each NamedType, if `name` does not exist in `ftpe`, that
     *    type is set to `Unit`, i.e. the corresponding argument is treated as
     *    an assignment expression (@see checkNames).
     */
    private def isApplicable(undetparams: List[Symbol], ftpe: Type, argtpes0: List[Type], pt: Type): Boolean = (
      ftpe match {
        case OverloadedType(pre, alts) => alts exists (alt => isApplicable(undetparams, pre memberType alt, argtpes0, pt))
        case ExistentialType(_, qtpe)  => isApplicable(undetparams, qtpe, argtpes0, pt)
        case mt @ MethodType(_, _)     => isApplicableToMethod(undetparams, mt, argtpes0, pt)
        case NullaryMethodType(restpe) => isApplicable(undetparams, restpe, argtpes0, pt)
        case PolyType(tparams, restpe) => createFromClonedSymbols(tparams, restpe)((tps1, res1) => isApplicable(tps1 ::: undetparams, res1, argtpes0, pt))
        case ErrorType                 => true
        case _                         => false
      }
    )

    /**
     * Are arguments of the given types applicable to `ftpe`? Type argument inference
     * is tried twice: firstly with the given expected type, and secondly with `WildcardType`.
     */
    // Todo: Try to make isApplicable always safe (i.e. not cause TypeErrors).
    // The chance of TypeErrors should be reduced through context errors
    private[typechecker] def isApplicableSafe(undetparams: List[Symbol], ftpe: Type, argtpes0: List[Type], pt: Type): Boolean = {
      def applicableExpectingPt(pt: Type): Boolean = {
        val silent = context.makeSilent(reportAmbiguousErrors = false)
        val result = newTyper(silent).infer.isApplicable(undetparams, ftpe, argtpes0, pt)
        if (silent.reporter.hasErrors && !pt.isWildcard)
          applicableExpectingPt(WildcardType) // second try
        else
          result
      }
      applicableExpectingPt(pt)
    }

    /** Is type `ftpe1` strictly more specific than type `ftpe2`
     *  when both are alternatives in an overloaded function?
     *  @see SLS (sec:overloading-resolution)
     */
    def isAsSpecific(ftpe1: Type, ftpe2: Type): Boolean = {
      def checkIsApplicable(argtpes: List[Type]) = isApplicable(Nil, ftpe2, argtpes, WildcardType)
      def bothAreVarargs                         = isVarArgsList(ftpe1.params) && isVarArgsList(ftpe2.params)
      def onRight = ftpe2 match {
        case OverloadedType(pre, alts)                     => alts forall (alt => isAsSpecific(ftpe1, pre memberType alt))
        case et: ExistentialType                           => et.withTypeVars(isAsSpecific(ftpe1, _))
        case mt @ MethodType(_, restpe)                    => !mt.isImplicit || isAsSpecific(ftpe1, restpe)
        case NullaryMethodType(res)                        => isAsSpecific(ftpe1, res)
        case PolyType(tparams, NullaryMethodType(restpe))  => isAsSpecific(ftpe1, PolyType(tparams, restpe))
        case PolyType(tparams, mt @ MethodType(_, restpe)) => !mt.isImplicit || isAsSpecific(ftpe1, PolyType(tparams, restpe))
        case _                                             => isAsSpecificValueType(ftpe1, ftpe2, Nil, Nil)
      }
      ftpe1 match {
        case OverloadedType(pre, alts)                                      => alts exists (alt => isAsSpecific(pre memberType alt, ftpe2))
        case et: ExistentialType                                            => isAsSpecific(et.skolemizeExistential, ftpe2)
        case NullaryMethodType(restpe)                                      => isAsSpecific(restpe, ftpe2)
        case mt @ MethodType(_, restpe) if mt.isImplicit                    => isAsSpecific(restpe, ftpe2)
        case mt @ MethodType(_, _) if bothAreVarargs                        => checkIsApplicable(mt.paramTypes mapConserve repeatedToSingle)
        case mt @ MethodType(params, _) if params.nonEmpty                  => checkIsApplicable(mt.paramTypes)
        case PolyType(tparams, NullaryMethodType(restpe))                   => isAsSpecific(PolyType(tparams, restpe), ftpe2)
        case PolyType(tparams, mt @ MethodType(_, restpe)) if mt.isImplicit => isAsSpecific(PolyType(tparams, restpe), ftpe2)
        case PolyType(_, mt @ MethodType(params, _)) if params.nonEmpty     => checkIsApplicable(mt.paramTypes)
        case ErrorType                                                      => true
        case _                                                              => onRight
      }
    }
    private def isAsSpecificValueType(tpe1: Type, tpe2: Type, undef1: List[Symbol], undef2: List[Symbol]): Boolean = tpe1 match {
      case PolyType(tparams1, rtpe1) =>
        isAsSpecificValueType(rtpe1, tpe2, undef1 ::: tparams1, undef2)
      case _                         =>
        tpe2 match {
          case PolyType(tparams2, rtpe2) => isAsSpecificValueType(tpe1, rtpe2, undef1, undef2 ::: tparams2)
          case _                         => existentialAbstraction(undef1, tpe1) <:< existentialAbstraction(undef2, tpe2)
        }
    }

    /** Is sym1 (or its companion class in case it is a module) a subclass of
     *  sym2 (or its companion class in case it is a module)?
     */
    def isProperSubClassOrObject(sym1: Symbol, sym2: Symbol): Boolean = (
         (sym1 ne sym2)
      && (sym1 ne NoSymbol)
      && (    (sym1 isSubClass sym2)
           || (sym1.isModuleClass && isProperSubClassOrObject(sym1.linkedClassOfClass, sym2))
           || (sym2.isModuleClass && isProperSubClassOrObject(sym1, sym2.linkedClassOfClass))
         )
    )

    /** is symbol `sym1` defined in a proper subclass of symbol `sym2`?
     */
    def isInProperSubClassOrObject(sym1: Symbol, sym2: Symbol) = (
         (sym2 eq NoSymbol)
      || isProperSubClassOrObject(sym1.safeOwner, sym2.owner)
    )

    def isStrictlyMoreSpecific(ftpe1: Type, ftpe2: Type, sym1: Symbol, sym2: Symbol): Boolean = {
      // ftpe1 / ftpe2 are OverloadedTypes (possibly with one single alternative) if they
      // denote the type of an "apply" member method (see "followApply")
      ftpe1.isError || {
        val specificCount = (if (isAsSpecific(ftpe1, ftpe2)) 1 else 0) -
                            (if (isAsSpecific(ftpe2, ftpe1) &&
                                 // todo: move to isAsSpecific test
//                                 (!ftpe2.isInstanceOf[OverloadedType] || ftpe1.isInstanceOf[OverloadedType]) &&
                                 (!phase.erasedTypes || covariantReturnOverride(ftpe1, ftpe2))) 1 else 0)
        val subClassCount = (if (isInProperSubClassOrObject(sym1, sym2)) 1 else 0) -
                            (if (isInProperSubClassOrObject(sym2, sym1)) 1 else 0)
        specificCount + subClassCount > 0
      }
    }

    private def covariantReturnOverride(ftpe1: Type, ftpe2: Type): Boolean = ftpe1 match {
      case MethodType(_, rtpe1) =>
        ftpe2 match {
          case MethodType(_, rtpe2) => rtpe1 <:< rtpe2 || rtpe2.typeSymbol == ObjectClass
          case _                    => false
        }
      case _ => false
    }

    /** error if arguments not within bounds. */
    def checkBounds(tree: Tree, pre: Type, owner: Symbol, tparams: List[Symbol], targs: List[Type], prefix: String): Boolean = {
      def issueBoundsError()                       = { NotWithinBounds(tree, prefix, targs, tparams, Nil) ; false }
      def issueKindBoundErrors(errs: List[String]) = { KindBoundErrors(tree, prefix, targs, tparams, errs) ; false }
      //@M validate variances & bounds of targs wrt variances & bounds of tparams
      //@M TODO: better place to check this?
      //@M TODO: errors for getters & setters are reported separately
      def check() = checkKindBounds(tparams, targs, pre, owner) match {
        case Nil  => isWithinBounds(pre, owner, tparams, targs) || issueBoundsError()
        case errs => (targs contains WildcardType) || issueKindBoundErrors(errs)
      }

      targs.exists(_.isErroneous) || tparams.exists(_.isErroneous) || check()
    }

    def checkKindBounds(tparams: List[Symbol], targs: List[Type], pre: Type, owner: Symbol): List[String] = {
      checkKindBounds0(tparams, targs, pre, owner, explainErrors = true) map {
        case (targ, tparam, kindErrors) =>
          kindErrors.errorMessage(targ, tparam)
      }
    }

    /** Substitute free type variables `undetparams` of polymorphic argument
     *  expression `tree`, given two prototypes `strictPt`, and `lenientPt`.
     *  `strictPt` is the first attempt prototype where type parameters
     *  are left unchanged. `lenientPt` is the fall-back prototype where type
     *  parameters are replaced by `WildcardType`s. We try to instantiate
     *  first to `strictPt` and then, if this fails, to `lenientPt`. If both
     *  attempts fail, an error is produced.
     */
    def inferArgumentInstance(tree: Tree, undetparams: List[Symbol], strictPt: Type, lenientPt: Type) {
      printTyping(tree, s"inferring arg instance based on pt0=$strictPt, pt1=$lenientPt")
      var targs = exprTypeArgs(undetparams, tree.tpe, strictPt, useWeaklyCompatible = false)
      if ((targs eq null) || !(tree.tpe.subst(undetparams, targs) <:< strictPt))
        targs = exprTypeArgs(undetparams, tree.tpe, lenientPt, useWeaklyCompatible = false)

      substExpr(tree, undetparams, targs, lenientPt)
      printTyping(tree, s"infer arg instance from pt0=$strictPt, pt1=$lenientPt; targs=$targs")
    }

    /** Infer type arguments `targs` for `tparams` of polymorphic expression in `tree`, given prototype `pt`.
     *
     * Substitute `tparams` to `targs` in `tree`, after adjustment by `adjustTypeArgs`, returning the type parameters that were not determined
     * If passed, infers against specified type `treeTp` instead of `tree.tp`.
     */
    def inferExprInstance(tree: Tree, tparams: List[Symbol], pt: Type = WildcardType, treeTp0: Type = null, keepNothings: Boolean = true, useWeaklyCompatible: Boolean = false): List[Symbol] = {
      val treeTp = if (treeTp0 eq null) tree.tpe else treeTp0 // can't refer to tree in default for treeTp0
      val tvars  = tparams map freshVar
      val targs  = exprTypeArgs(tvars, tparams, treeTp, pt, useWeaklyCompatible)
      def infer_s = map3(tparams, tvars, targs)((tparam, tvar, targ) => s"$tparam=$tvar/$targ") mkString ","
      printTyping(tree, s"infer expr instance from pt=$pt, $infer_s")

      // SI-7899 inferring by-name types is unsound. The correct behaviour is conditional because the hole is
      //         exploited in Scalaz (Free.scala), as seen in: run/t7899-regression.
      def dropByNameIfStrict(tp: Type): Type = if (settings.inferByName) tp else dropByName(tp)
      def targsStrict = if (targs eq null) null else targs mapConserve dropByNameIfStrict

      if (keepNothings || (targs eq null)) { //@M: adjustTypeArgs fails if targs==null, neg/t0226
        substExpr(tree, tparams, targsStrict, pt)
        List()
      } else {
        val AdjustedTypeArgs.Undets(okParams, okArgs, leftUndet) = adjustTypeArgs(tparams, tvars, targsStrict)
        def solved_s = map2(okParams, okArgs)((p, a) => s"$p=$a") mkString ","
        def undet_s = leftUndet match {
          case Nil => ""
          case ps  => ps.mkString(", undet=", ",", "")
        }
        printTyping(tree, s"infer solved $solved_s$undet_s")
        substExpr(tree, okParams, okArgs, pt)
        leftUndet
      }
    }

    /** Substitute free type variables `undetparams` of polymorphic argument
     *  expression `tree` to `targs`, Error if `targs` is null.
     */
    private def substExpr(tree: Tree, undetparams: List[Symbol], targs: List[Type], pt: Type) {
      if (targs eq null) {
        if (!tree.tpe.isErroneous && !pt.isErroneous)
          PolymorphicExpressionInstantiationError(tree, undetparams, pt)
      }
      else {
        new TreeTypeSubstituter(undetparams, targs).traverse(tree)
        notifyUndetparamsInferred(undetparams, targs)
      }
    }

    /** Substitute free type variables `undetparams` of application
     *  `fn(args)`, given prototype `pt`.
     *
     *  @param fn          fn: the function that needs to be instantiated.
     *  @param undetparams the parameters that need to be determined
     *  @param args        the actual arguments supplied in the call.
     *  @param pt0         the expected type of the function application
     *  @return            The type parameters that remain uninstantiated,
     *                     and that thus have not been substituted.
     */
    def inferMethodInstance(fn: Tree, undetparams: List[Symbol],
                            args: List[Tree], pt0: Type): List[Symbol] = fn.tpe match {
      case mt @ MethodType(params0, _) =>
        try {
          val pt      = if (pt0.typeSymbol == UnitClass) WildcardType else pt0
          val formals = formalTypes(mt.paramTypes, args.length)
          val argtpes = tupleIfNecessary(formals, args map (x => elimAnonymousClass(x.tpe.deconst)))
          val restpe  = fn.tpe.resultType(argtpes)

          val AdjustedTypeArgs.AllArgsAndUndets(okparams, okargs, allargs, leftUndet) =
            methTypeArgs(undetparams, formals, restpe, argtpes, pt)

          if (checkBounds(fn, NoPrefix, NoSymbol, undetparams, allargs, "inferred ")) {
            val treeSubst = new TreeTypeSubstituter(okparams, okargs)
            treeSubst traverseTrees fn :: args
            notifyUndetparamsInferred(okparams, okargs)

            leftUndet match {
              case Nil  => Nil
              case xs   =>
                // #3890
                val xs1 = treeSubst.typeMap mapOver xs
                if (xs ne xs1)
                  new TreeSymSubstTraverser(xs, xs1) traverseTrees fn :: args

                xs1
            }
          } else Nil
        }
        catch ifNoInstance { msg =>
          NoMethodInstanceError(fn, args, msg); List()
        }
    }

    /** Substitute free type variables `undetparams` of type constructor
     *  `tree` in pattern, given prototype `pt`.
     *
     *  @param tree        the constructor that needs to be instantiated
     *  @param undetparams the undetermined type parameters
     *  @param pt0         the expected result type of the instance
     */
    def inferConstructorInstance(tree: Tree, undetparams: List[Symbol], pt0: Type) {
      val pt       = abstractTypesToBounds(pt0)
      val ptparams = freeTypeParamsOfTerms(pt)
      val ctorTp   = tree.tpe
      val resTp    = ctorTp.finalResultType

      debuglog("infer constr inst "+ tree +"/"+ undetparams +"/ pt= "+ pt +" pt0= "+ pt0 +" resTp: "+ resTp)

      /* Compute type arguments for undetermined params */
      def inferFor(pt: Type): Option[List[Type]] = {
        val tvars   = undetparams map freshVar
        val resTpV  = resTp.instantiateTypeParams(undetparams, tvars)

        if (resTpV <:< pt) {
          try {
            // debuglog("TVARS "+ (tvars map (_.constr)))
            // look at the argument types of the primary constructor corresponding to the pattern
            val variances  =
              if (ctorTp.paramTypes.isEmpty) undetparams map varianceInType(ctorTp)
              else undetparams map varianceInTypes(ctorTp.paramTypes)

            // Note: this is the only place where solvedTypes (or, indirectly, solve) is called
            // with upper = true.
            val targs = solvedTypes(tvars, undetparams, variances, upper = true, lubDepth(resTp :: pt :: Nil))
            // checkBounds(tree, NoPrefix, NoSymbol, undetparams, targs, "inferred ")
            // no checkBounds here. If we enable it, test bug602 fails.
            // TODO: reinstate checkBounds, return params that fail to meet their bounds to undetparams
            Some(targs)
          } catch ifNoInstance { msg =>
            debuglog("NO INST "+ ((tvars, tvars map (_.constr))))
            NoConstructorInstanceError(tree, resTp, pt, msg)
            None
          }
        } else {
          debuglog("not a subtype: "+ resTpV +" </:< "+ pt)
          None
        }
      }

      def inferForApproxPt =
        if (isFullyDefined(pt)) {
          inferFor(pt.instantiateTypeParams(ptparams, ptparams map (x => WildcardType))) flatMap { targs =>
            val ctorTpInst = tree.tpe.instantiateTypeParams(undetparams, targs)
            val resTpInst  = skipImplicit(ctorTpInst.finalResultType)
            val ptvars     =
              ptparams map {
                // since instantiateTypeVar wants to modify the skolem that corresponds to the method's type parameter,
                // and it uses the TypeVar's origin to locate it, deskolemize the existential skolem to the method tparam skolem
                // (the existential skolem was created by adaptConstrPattern to introduce the type slack necessary to soundly deal with variant type parameters)
                case skolem if skolem.isGADTSkolem => freshVar(skolem.deSkolemize.asInstanceOf[TypeSymbol])
                case p => freshVar(p)
              }

            val ptV        = pt.instantiateTypeParams(ptparams, ptvars)

            if (isPopulated(resTpInst, ptV)) {
              ptvars foreach instantiateTypeVar
              debuglog("isPopulated "+ resTpInst +", "+ ptV +" vars= "+ ptvars)
              Some(targs)
            } else None
          }
        } else None

      inferFor(pt) orElse inferForApproxPt match {
        case Some(targs) =>
          new TreeTypeSubstituter(undetparams, targs).traverse(tree)
          notifyUndetparamsInferred(undetparams, targs)
        case _ =>
          def not = if (isFullyDefined(pt)) "" else "not "
          devWarning(s"failed inferConstructorInstance for $tree: ${tree.tpe} undet=$undetparams, pt=$pt (${not}fully defined)")
          ConstrInstantiationError(tree, resTp, pt)
      }
    }

    def instBounds(tvar: TypeVar): TypeBounds = {
      val tparam               = tvar.origin.typeSymbol
      val instType             = toOrigin(tvar.constr.inst)
      val TypeBounds(lo, hi)   = tparam.info.bounds
      val (loBounds, hiBounds) =
        if (isFullyDefined(instType)) (List(instType), List(instType))
        else (tvar.constr.loBounds, tvar.constr.hiBounds)

      TypeBounds(
        lub(lo :: loBounds map toOrigin),
        glb(hi :: hiBounds map toOrigin)
      )
    }

    def isInstantiatable(tvars: List[TypeVar]) = {
      val tvars1 = tvars map (_.cloneInternal)
      // Note: right now it's not clear that solving is complete, or how it can be made complete!
      // So we should come back to this and investigate.
      solve(tvars1, tvars1 map (_.origin.typeSymbol), tvars1 map (_ => Variance.Covariant), upper = false, Depth.AnyDepth)
    }

    // this is quite nasty: it destructively changes the info of the syms of e.g., method type params
    // (see #3692, where the type param T's bounds were set to > : T <: T, so that parts looped)
    // the changes are rolled back by restoreTypeBounds, but might be unintentionally observed in the mean time
    def instantiateTypeVar(tvar: TypeVar) {
      val tparam                    = tvar.origin.typeSymbol
      val TypeBounds(lo0, hi0)      = tparam.info.bounds
      val tb @ TypeBounds(lo1, hi1) = instBounds(tvar)
      val enclCase                  = context.enclosingCaseDef
      def enclCase_s                = enclCase.toString.replaceAll("\\n", " ").take(60)

      if (enclCase.savedTypeBounds.nonEmpty) log(
        sm"""|instantiateTypeVar with nonEmpty saved type bounds {
             |  enclosing  $enclCase_s
             |      saved  ${enclCase.savedTypeBounds}
             |     tparam  ${tparam.shortSymbolClass} ${tparam.defString}
             |}""")

      if (lo1 <:< hi1) {
        if (lo1 <:< lo0 && hi0 <:< hi1) // bounds unimproved
          log(s"redundant bounds: discarding TypeBounds($lo1, $hi1) for $tparam, no improvement on TypeBounds($lo0, $hi0)")
        else if (tparam == lo1.typeSymbolDirect || tparam == hi1.typeSymbolDirect)
          log(s"cyclical bounds: discarding TypeBounds($lo1, $hi1) for $tparam because $tparam appears as bounds")
        else {
          enclCase pushTypeBounds tparam
          tparam setInfo logResult(s"updated bounds: $tparam from ${tparam.info} to")(tb)
        }
      }
      else log(s"inconsistent bounds: discarding TypeBounds($lo1, $hi1)")
    }

    /** Type intersection of simple type tp1 with general type tp2.
     *  The result eliminates some redundancies.
     */
    def intersect(tp1: Type, tp2: Type): Type = {
      if (tp1 <:< tp2) tp1
      else if (tp2 <:< tp1) tp2
      else {
        val reduced2 = tp2 match {
          case rtp @ RefinedType(parents2, decls2) =>
            copyRefinedType(rtp, parents2 filterNot (tp1 <:< _), decls2)
          case _ =>
            tp2
        }
        intersectionType(List(tp1, reduced2))
      }
    }

    def inferTypedPattern(tree0: Tree, pattp: Type, pt0: Type, canRemedy: Boolean): Type = {
      val pt        = abstractTypesToBounds(pt0)
      val ptparams  = freeTypeParamsOfTerms(pt)
      val tpparams  = freeTypeParamsOfTerms(pattp)

      def ptMatchesPattp = pt matchesPattern pattp.widen
      def pattpMatchesPt = pattp matchesPattern pt

      /* If we can absolutely rule out a match we can fail early.
       * This is the case if the scrutinee has no unresolved type arguments
       * and is a "final type", meaning final + invariant in all type parameters.
       */
      if (pt.isFinalType && ptparams.isEmpty && !ptMatchesPattp) {
        IncompatibleScrutineeTypeError(tree0, pattp, pt)
        return ErrorType
      }

      checkCheckable(tree0, pattp, pt, inPattern = true, canRemedy)
      if (pattp <:< pt) ()
      else {
        debuglog("free type params (1) = " + tpparams)

        var tvars = tpparams map freshVar
        var tp    = pattp.instantiateTypeParams(tpparams, tvars)

        if ((tp <:< pt) && isInstantiatable(tvars)) ()
        else {
          tvars = tpparams map freshVar
          tp    = pattp.instantiateTypeParams(tpparams, tvars)

          debuglog("free type params (2) = " + ptparams)

          val ptvars = ptparams map freshVar
          val pt1    = pt.instantiateTypeParams(ptparams, ptvars)

          // See ticket #2486 for an example of code which would incorrectly
          // fail if we didn't allow for pattpMatchesPt.
          if (isPopulated(tp, pt1) && isInstantiatable(tvars ++ ptvars) || pattpMatchesPt)
             ptvars foreach instantiateTypeVar
          else {
            PatternTypeIncompatibleWithPtError1(tree0, pattp, pt)
            return ErrorType
          }
        }
        tvars foreach instantiateTypeVar
      }
      /* If the scrutinee has free type parameters but the pattern does not,
       * we have to flip the arguments so the expected type is treated as more
       * general when calculating the intersection.  See run/bug2755.scala.
       */
      if (tpparams.isEmpty && ptparams.nonEmpty) intersect(pattp, pt)
      else intersect(pt, pattp)
    }

    def inferModulePattern(pat: Tree, pt: Type) =
      if (!(pat.tpe <:< pt)) {
        val ptparams = freeTypeParamsOfTerms(pt)
        debuglog("free type params (2) = " + ptparams)
        val ptvars = ptparams map freshVar
        val pt1 = pt.instantiateTypeParams(ptparams, ptvars)
        if (pat.tpe <:< pt1)
          ptvars foreach instantiateTypeVar
        else
          PatternTypeIncompatibleWithPtError2(pat, pt1, pt)
      }

    object toOrigin extends TypeMap {
      def apply(tp: Type): Type = tp match {
        case TypeVar(origin, _) => origin
        case _ => mapOver(tp)
      }
    }

    object approximateAbstracts extends TypeMap {
      def apply(tp: Type): Type = tp.dealiasWiden match {
        case TypeRef(pre, sym, _) if sym.isAbstractType => WildcardType
        case _                                          => mapOver(tp)
      }
    }

    /** Collects type parameters referred to in a type.
     */
    def freeTypeParamsOfTerms(tp: Type): List[Symbol] = {
      // An inferred type which corresponds to an unknown type
      // constructor creates a file/declaration order-dependent crasher
      // situation, the behavior of which depends on the state at the
      // time the typevar is created. Until we can deal with these
      // properly, we can avoid it by ignoring type parameters which
      // have type constructors amongst their bounds. See SI-4070.
      def isFreeTypeParamOfTerm(sym: Symbol) = (
        sym.isAbstractType
          && sym.owner.isTerm
          && !sym.info.bounds.exists(_.typeParams.nonEmpty)
        )

      // Intentionally *not* using `Type#typeSymbol` here, which would normalize `tp`
      // and collect symbols from the result type of any resulting `PolyType`s, which
      // are not free type parameters of `tp`.
      //
      // Contrast with `isFreeTypeParamNoSkolem`.
      val syms = tp collect {
        case TypeRef(_, sym, _) if isFreeTypeParamOfTerm(sym) => sym
      }
      syms.distinct
    }

    /* -- Overload Resolution ---------------------------------------------- */

    /** Assign `tree` the symbol and type of the alternative which
     *  matches prototype `pt`, if it exists.
     *  If several alternatives match `pt`, take parameterless one.
     *  If no alternative matches `pt`, take the parameterless one anyway.
     */
    def inferExprAlternative(tree: Tree, pt: Type): Tree = {
      val c = context
      class InferTwice(pre: Type, alts: List[Symbol]) extends c.TryTwice {
        def tryOnce(isSecondTry: Boolean): Unit = {
          val alts0 = alts filter (alt => isWeaklyCompatible(pre memberType alt, pt))
          val alts1 = if (alts0.isEmpty) alts else alts0
          val bests = bestAlternatives(alts1) { (sym1, sym2) =>
            val tp1 = pre memberType sym1
            val tp2 = pre memberType sym2

            (    (tp2 eq ErrorType)
              || isWeaklyCompatible(tp1, pt) && !isWeaklyCompatible(tp2, pt)
              || isStrictlyMoreSpecific(tp1, tp2, sym1, sym2)
            )
          }
          // todo: missing test case for bests.isEmpty
          bests match {
            case best :: Nil                              => tree setSymbol best setType (pre memberType best)
            case best :: competing :: _ if alts0.nonEmpty =>
              // SI-6912 Don't give up and leave an OverloadedType on the tree.
              //         Originally I wrote this as `if (secondTry) ... `, but `tryTwice` won't attempt the second try
              //         unless an error is issued. We're not issuing an error, in the assumption that it would be
              //         spurious in light of the erroneous expected type
              if (pt.isErroneous) setError(tree)
              else AmbiguousExprAlternativeError(tree, pre, best, competing, pt, isSecondTry)
            case _                                        => if (bests.isEmpty || alts0.isEmpty) NoBestExprAlternativeError(tree, pt, isSecondTry)
          }
        }
      }
      tree.tpe match {
        case OverloadedType(pre, alts) => (new InferTwice(pre, alts)).apply() ; tree
        case _                         => tree
      }
    }

    // Checks against the name of the parameter and also any @deprecatedName.
    private def paramMatchesName(param: Symbol, name: Name) =
      param.name == name || param.deprecatedParamName.exists(_ == name)

    private def containsNamedType(argtpes: List[Type]): Boolean = argtpes match {
      case Nil                  => false
      case NamedType(_, _) :: _ => true
      case _ :: rest            => containsNamedType(rest)
    }
    private def namesOfNamedArguments(argtpes: List[Type]) =
      argtpes collect { case NamedType(name, _) => name }

    /** Given a list of argument types and eligible method overloads, whittle the
     *  list down to the methods which should be considered for specificity
     *  testing, taking into account here:
     *   - named arguments at the call site (keep only methods with name-matching parameters)
     *   - if multiple methods are eligible, drop any methods which take default arguments
     *   - drop any where arity cannot match under any conditions (allowing for
     *     overloaded applies, varargs, and tupling conversions)
     *  This method is conservative; it can tolerate some varieties of false positive,
     *  but no false negatives.
     *
     *  @param  eligible     the overloaded method symbols
     *  @param  argtpes      the argument types at the call site
     *  @param  varargsStar  true if the call site has a `: _*` attached to the last argument
     */
    private def overloadsToConsiderBySpecificity(eligible: List[Symbol], argtpes: List[Type], varargsStar: Boolean): List[Symbol] = {
      // TODO spec: this namesMatch business is not spec'ed, and is the wrong fix for SI-4592
      // we should instead clarify what the spec means by "typing each argument with an undefined expected type".
      // What does typing a named argument entail when we don't know what the valid parameter names are?
      // (Since we're doing overload resolution, there are multiple alternatives that can define different names.)
      // Luckily, the next step checks applicability to the individual alternatives, so it knows whether an assignment is:
      // 1) a valid named argument
      // 2) a well-typed assignment
      // 3) an error (e.g., rhs does not refer to a variable)
      //
      // For now, the logic is:
      // If there are any foo=bar style arguments, and any of the overloaded
      // methods has a parameter named `foo`, then only those methods are considered when we must disambiguate.
      def namesMatch = namesOfNamedArguments(argtpes) match {
        case Nil   => Nil
        case names => eligible filter (m => names forall (name => m.info.params exists (p => paramMatchesName(p, name))))
      }
      if (eligible.isEmpty || eligible.tail.isEmpty) eligible
      else
        namesMatch match {
          case namesMatch if namesMatch.nonEmpty => namesMatch // TODO: this has no basis in the spec, remove!
          case _ =>
            // If there are multiple applicable alternatives, drop those using default arguments.
            // This is done indirectly by checking applicability based on arity in `isApplicableBasedOnArity`.
            // If defaults are required in the application, the arities won't match up exactly.
            // TODO: should we really allow tupling here?? (If we don't, this is the only call-site with `tuplingAllowed = true`)
            eligible filter (alt => isApplicableBasedOnArity(alt.tpe, argtpes.length, varargsStar, tuplingAllowed = true))
        }
    }

    /** Assign `tree` the type of an alternative which is applicable
     *  to `argtpes`, and whose result type is compatible with `pt`.
     *  If several applicable alternatives exist, drop the alternatives which use
     *  default arguments, then select the most specialized one.
     *  If no applicable alternative exists, and pt != WildcardType, try again
     *  with pt = WildcardType.
     *  Otherwise, if there is no best alternative, error.
     *
     *  @param argtpes0 contains the argument types. If an argument is named, as
     *    "a = 3", the corresponding type is `NamedType("a", Int)`. If the name
     *    of some NamedType does not exist in an alternative's parameter names,
     *    the type is replaces by `Unit`, i.e. the argument is treated as an
     *    assignment expression.
     *
     *  @pre  tree.tpe is an OverloadedType.
     */
    def inferMethodAlternative(tree: Tree, undetparams: List[Symbol], argtpes0: List[Type], pt0: Type): Unit = {
      // This potentially makes up to four attempts: tryOnce may execute
      // with and without views enabled, and bestForExpectedType will try again
      // with pt = WildcardType if it fails with pt != WildcardType.
      val c = context
      class InferMethodAlternativeTwice extends c.TryTwice {
        private[this] val OverloadedType(pre, alts) = tree.tpe
        private[this] var varargsStar = false
        private[this] val argtpes = argtpes0 mapConserve {
          case RepeatedType(tp) => varargsStar = true ; tp
          case tp               => tp
        }

        private def followType(sym: Symbol) = followApply(pre memberType sym)
        // separate method to help the inliner
        private def isAltApplicable(pt: Type)(alt: Symbol) = context inSilentMode { isApplicable(undetparams, followType(alt), argtpes, pt) && !context.reporter.hasErrors }
        private def rankAlternatives(sym1: Symbol, sym2: Symbol) = isStrictlyMoreSpecific(followType(sym1), followType(sym2), sym1, sym2)
        private def bestForExpectedType(pt: Type, isLastTry: Boolean): Unit = {
          val applicable  = overloadsToConsiderBySpecificity(alts filter isAltApplicable(pt), argtpes, varargsStar)
          val ranked      = bestAlternatives(applicable)(rankAlternatives)
          ranked match {
            case best :: competing :: _ => AmbiguousMethodAlternativeError(tree, pre, best, competing, argtpes, pt, isLastTry) // ambiguous
            case best :: Nil            => tree setSymbol best setType (pre memberType best)           // success
            case Nil if pt.isWildcard   => NoBestMethodAlternativeError(tree, argtpes, pt, isLastTry)  // failed
            case Nil                    => bestForExpectedType(WildcardType, isLastTry)                // failed, but retry with WildcardType
          }
        }

        private[this] val pt = if (pt0.typeSymbol == UnitClass) WildcardType else pt0
        def tryOnce(isLastTry: Boolean): Unit = {
          debuglog(s"infer method alt ${tree.symbol} with alternatives ${alts map pre.memberType} argtpes=$argtpes pt=$pt")
          bestForExpectedType(pt, isLastTry)
        }
      }

      (new InferMethodAlternativeTwice).apply()
    }

    /** Assign `tree` the type of all polymorphic alternatives
     *  which have the same number of type parameters as does `argtypes`
     *  with all argtypes are within the corresponding type parameter bounds.
     *  If no such polymorphic alternative exist, error.
     */
    def inferPolyAlternatives(tree: Tree, argtypes: List[Type]): Unit = {
      val OverloadedType(pre, alts) = tree.tpe
      // Alternatives with a matching length type parameter list
      val matchingLength   = tree.symbol filter (alt => sameLength(alt.typeParams, argtypes))
      def allMonoAlts      = alts forall (_.typeParams.isEmpty)
      def errorKind        = matchingLength match {
        case NoSymbol if allMonoAlts => PolyAlternativeErrorKind.NoParams          // no polymorphic method alternative
        case NoSymbol                => PolyAlternativeErrorKind.WrongNumber       // wrong number of tparams
        case _                       => PolyAlternativeErrorKind.ArgsDoNotConform  // didn't conform to bounds
      }
      def fail() = PolyAlternativeError(tree, argtypes, matchingLength, errorKind)
      def finish(sym: Symbol, tpe: Type) = tree setSymbol sym setType tpe
      // Alternatives which conform to bounds
      def checkWithinBounds(sym: Symbol) = sym.alternatives match {
        case Nil if argtypes.exists(_.isErroneous) =>
        case Nil                                   => fail()
        case alt :: Nil                            => finish(alt, pre memberType alt)
        case alts @ (hd :: _)                      =>
          log(s"Attaching AntiPolyType-carrying overloaded type to $sym")
          // Multiple alternatives which are within bounds; spin up an
          // overloaded type which carries an "AntiPolyType" as a prefix.
          val tparams = newAsSeenFromMap(pre, hd.owner) mapOver hd.typeParams
          val bounds  = tparams map (_.tpeHK) // see e.g., #1236
          val tpe     = PolyType(tparams, OverloadedType(AntiPolyType(pre, bounds), alts))
          finish(sym setInfo tpe, tpe)
      }
      matchingLength.alternatives match {
        case Nil        => fail()
        case alt :: Nil => finish(alt, pre memberType alt)
        case _          => checkWithinBounds(matchingLength filter (alt => isWithinBounds(pre, alt.owner, alt.typeParams, argtypes)))
      }
    }
  }
}
