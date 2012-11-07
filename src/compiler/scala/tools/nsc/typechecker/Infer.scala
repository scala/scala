/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.collection.immutable
import scala.collection.mutable.ListBuffer
import scala.util.control.ControlThrowable
import symtab.Flags._

/** This trait ...
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
trait Infer extends Checkable {
  self: Analyzer =>

  import global._
  import definitions._
  import typer.printInference
  import typeDebug.ptBlock

/* -- Type parameter inference utility functions --------------------------- */

  private def assertNonCyclic(tvar: TypeVar) =
    assert(tvar.constr.inst != tvar, tvar.origin)

  /** The formal parameter types corresponding to `formals`.
   *  If `formals` has a repeated last parameter, a list of
   *  (nargs - params.length + 1) copies of its type is returned.
   *  By-name types are replaced with their underlying type.
   *
   *  @param removeByName allows keeping ByName parameters. Used in NamesDefaults.
   *  @param removeRepeated allows keeping repeated parameter (if there's one argument). Used in NamesDefaults.
   */
  def formalTypes(formals: List[Type], nargs: Int, removeByName: Boolean = true, removeRepeated: Boolean = true): List[Type] = {
    val formals1 = if (removeByName) formals mapConserve {
      case TypeRef(_, ByNameParamClass, List(arg)) => arg
      case formal => formal
    } else formals
    if (isVarArgTypes(formals1) && (removeRepeated || formals.length != nargs)) {
      val ft = formals1.last.normalize.typeArgs.head
      formals1.init ::: (for (i <- List.range(formals1.length - 1, nargs)) yield ft)
    } else formals1
  }

  /** Sorts the alternatives according to the given comparison function.
   *  Returns a list containing the best alternative as well as any which
   *  the best fails to improve upon.
   */
  private def bestAlternatives(alternatives: List[Symbol])(isBetter: (Symbol, Symbol) => Boolean): List[Symbol] = {
    def improves(sym1: Symbol, sym2: Symbol) = (
         sym2 == NoSymbol
      || sym2.isError
      || sym2.hasAnnotation(BridgeClass)
      || isBetter(sym1, sym2)
    )

    alternatives sortWith improves match {
      case best :: rest if rest.nonEmpty => best :: rest.filterNot(alt => improves(best, alt))
      case bests                         => bests
    }
  }

  /** Returns `(formals, formalsExpanded)` where `formalsExpanded` are the expected types
   * for the `nbSubPats` sub-patterns of an extractor pattern, of which the corresponding
   * unapply[Seq] call is assumed to have result type `resTp`.
   *
   * `formals` are the formal types before expanding a potential repeated parameter (must come last in `formals`, if at all)
   *
   * @throws TypeError when the unapply[Seq] definition is ill-typed
   * @returns (null, null) when the expected number of sub-patterns cannot be satisfied by the given extractor
   *
   * From the spec:
   *   8.1.8 ExtractorPatterns
   *
   *   An extractor pattern x(p1, ..., pn) where n ≥ 0 is of the same syntactic form as a constructor pattern.
   *   However, instead of a case class, the stable identifier x denotes an object which has a member method named unapply or unapplySeq that matches the pattern.
   *   An unapply method in an object x matches the pattern x(p1, ..., pn) if it takes exactly one argument and one of the following applies:
   *
   *   n = 0 and unapply’s result type is Boolean.
   *
   *   n = 1 and unapply’s result type is Option[T], for some type T.
   *     the (only) argument pattern p1 is typed in turn with expected type T
   *
   *   n > 1 and unapply’s result type is Option[(T1, ..., Tn)], for some types T1, ..., Tn.
   *     the argument patterns p1, ..., pn are typed in turn with expected types T1, ..., Tn
   */
  def extractorFormalTypes(resTp: Type, nbSubPats: Int, unappSym: Symbol): (List[Type], List[Type]) = {
    val isUnapplySeq     = unappSym.name == nme.unapplySeq
    val booleanExtractor = resTp.typeSymbolDirect == BooleanClass

    def seqToRepeatedChecked(tp: Type) = {
      val toRepeated = seqToRepeated(tp)
      if (tp eq toRepeated) throw new TypeError("(the last tuple-component of) the result type of an unapplySeq must be a Seq[_]")
      else toRepeated
    }

    val formals =
      if (nbSubPats == 0 && booleanExtractor && !isUnapplySeq)  Nil
      else resTp.baseType(OptionClass).typeArgs match {
        case optionTArg :: Nil =>
          if (nbSubPats == 1)
            if (isUnapplySeq) List(seqToRepeatedChecked(optionTArg))
            else List(optionTArg)
          // TODO: update spec to reflect we allow any ProductN, not just TupleN
          else getProductArgs(optionTArg) match {
            case Nil if isUnapplySeq => List(seqToRepeatedChecked(optionTArg))
            case tps if isUnapplySeq => tps.init :+ seqToRepeatedChecked(tps.last)
            case tps => tps
          }
        case _ =>
          if (isUnapplySeq)
            throw new TypeError(s"result type $resTp of unapplySeq defined in ${unappSym.owner+unappSym.owner.locationString} not in {Option[_], Some[_]}")
          else
            throw new TypeError(s"result type $resTp of unapply defined in ${unappSym.owner+unappSym.owner.locationString} not in {Boolean, Option[_], Some[_]}")
      }

    // for unapplySeq, replace last vararg by as many instances as required by nbSubPats
    val formalsExpanded =
      if (isUnapplySeq && formals.nonEmpty) formalTypes(formals, nbSubPats)
      else formals

    if (formalsExpanded.lengthCompare(nbSubPats) != 0) (null, null)
    else (formals, formalsExpanded)
  }

  /** A fresh type variable with given type parameter as origin.
   *
   *  @param tparam ...
   *  @return       ...
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
    def apply(tp: Type): Type = tp match {
      case WildcardType | BoundedWildcardType(_) | NoType =>
        throw new NoInstance("undetermined type")
      case tv @ TypeVar(origin, constr) if !tv.untouchable =>
        if (constr.inst == NoType) {
          throw new DeferredNoInstance(() =>
            s"no unique instantiation of type variable $origin could be found")
        } else if (excludedVars(tv)) {
          throw new NoInstance("cyclic instantiation")
        } else {
          excludedVars += tv
          try apply(constr.inst)
          finally excludedVars -= tv
        }
      case _ =>
        mapOver(tp)
    }
  }

  /** Is type fully defined, i.e. no embedded anytypes or wildcards in it?
   *
   *  @param tp ...
   *  @return   ...
   */
  private[typechecker] def isFullyDefined(tp: Type): Boolean = tp match {
    case WildcardType | BoundedWildcardType(_) | NoType =>
      false
    case NoPrefix | ThisType(_) | ConstantType(_) =>
      true
    case TypeRef(pre, sym, args) =>
      isFullyDefined(pre) && (args forall isFullyDefined)
    case SingleType(pre, sym) =>
      isFullyDefined(pre)
    case RefinedType(ts, decls) =>
      ts forall isFullyDefined
    case TypeVar(origin, constr) if (constr.inst == NoType) =>
      false
    case _ =>
      try {
        instantiate(tp); true
      } catch {
        case ex: NoInstance => false
      }
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
  def solvedTypes(tvars: List[TypeVar], tparams: List[Symbol],
                  variances: List[Int], upper: Boolean, depth: Int): List[Type] = {

    if (tvars.nonEmpty)
      printInference("[solve types] solving for " + tparams.map(_.name).mkString(", ") + " in " + tvars.mkString(", "))

    if (!solve(tvars, tparams, variances, upper, depth)) {
      // no panic, it's good enough to just guess a solution, we'll find out
      // later whether it works.  *ZAP* @M danger, Will Robinson! this means
      // that you should never trust inferred type arguments!
      //
      // Need to call checkBounds on the args/typars or type1 on the tree
      // for the expression that results from type inference see e.g., #2421:
      // implicit search had been ignoring this caveat
      // throw new DeferredNoInstance(() =>
      //   "no solution exists for constraints"+(tvars map boundsString))
    }
    for (tvar <- tvars ; if tvar.constr.inst == tvar) {
      if (tvar.origin.typeSymbol.info eq ErrorType)
        // this can happen if during solving a cyclic type parameter
        // such as T <: T gets completed. See #360
        tvar.constr.inst = ErrorType
      else
        assert(false, tvar.origin+" at "+tvar.origin.typeSymbol.owner)
    }
    tvars map instantiate
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
    case mt @ MethodType(params, restpe) if mt.isImplicit =>
      normalize(restpe)
    case mt @ MethodType(_, restpe) if !mt.isDependentMethodType =>
      functionType(mt.paramTypes, normalize(restpe))
    case NullaryMethodType(restpe) =>
      normalize(restpe)
    case ExistentialType(tparams, qtpe) =>
      newExistentialType(tparams, normalize(qtpe))
    case tp1 =>
      tp1 // @MAT aliases already handled by subtyping
  }

  private val stdErrorClass = rootMirror.RootClass.newErrorClass(tpnme.ERROR)
  private val stdErrorValue = stdErrorClass.newErrorValue(nme.ERROR)

  /** The context-dependent inferencer part */
  class Inferencer(context: Context) extends InferencerContextErrors with InferCheckable {
    import InferErrorGen._

    /* -- Error Messages --------------------------------------------------- */
    def setError[T <: Tree](tree: T): T = {
      debuglog("set error: "+ tree)
      // this breaks -Ydebug pretty radically
      // if (settings.debug.value) { // DEBUG
      //   println("set error: "+tree);
      //   throw new Error()
      // }
      def name        = newTermName("<error: " + tree.symbol + ">")
      def errorClass  = if (context.reportErrors) context.owner.newErrorClass(name.toTypeName) else stdErrorClass
      def errorValue  = if (context.reportErrors) context.owner.newErrorValue(name) else stdErrorValue
      def errorSym    = if (tree.isType) errorClass else errorValue

      if (tree.hasSymbolField)
        tree setSymbol errorSym

      tree setType ErrorType
    }

    def getContext = context

    def issue(err: AbsTypeError): Unit = context.issue(err)

    def isPossiblyMissingArgs(found: Type, req: Type) = (
      false
      /** However it is that this condition is expected to imply
       *  "is possibly missing args", it is too weak.  It is
       *  better to say nothing than to offer misleading guesses.

         (found.resultApprox ne found)
      && isWeaklyCompatible(found.resultApprox, req)
      */
    )

    def explainTypes(tp1: Type, tp2: Type) =
      withDisambiguation(List(), tp1, tp2)(global.explainTypes(tp1, tp2))

    /* -- Tests & Checks---------------------------------------------------- */

    /** Check that `sym` is defined and accessible as a member of
     *  tree `site` with type `pre` in current context.
     *
     * Note: pre is not refchecked -- moreover, refchecking the resulting tree may not refcheck pre,
     *       since pre may not occur in its type (callers should wrap the result in a TypeTreeWithDeferredRefCheck)
     */
    def checkAccessible(tree: Tree, sym: Symbol, pre: Type, site: Tree): Tree =
      if (sym.isError) {
        tree setSymbol sym setType ErrorType
      } else {
        if (context.unit.exists)
          context.unit.depends += sym.enclosingTopLevelClass

        var sym1 = sym filter (alt => context.isAccessible(alt, pre, site.isInstanceOf[Super]))
        // Console.println("check acc " + (sym, sym1) + ":" + (sym.tpe, sym1.tpe) + " from " + pre);//DEBUG
        if (sym1 == NoSymbol && sym.isJavaDefined && context.unit.isJava) // don't try to second guess Java; see #4402
          sym1 = sym

        if (sym1 == NoSymbol) {
          if (settings.debug.value) {
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
        else {
          if (context.owner.isTermMacro && (sym1 hasFlag LOCKED)) {
            // we must not let CyclicReference to be thrown from sym1.info
            // because that would mark sym1 erroneous, which it is not
            // but if it's a true CyclicReference then macro def will report it
            // see comments to TypeSigError for an explanation of this special case
            // [Eugene] is there a better way?
            val dummy = new TypeCompleter { val tree = EmptyTree; override def complete(sym: Symbol) {} }
            throw CyclicReference(sym1, dummy)
          }

          if (sym1.isTerm)
            sym1.cookJavaRawInfo() // xform java rawtypes into existentials

          val owntype = {
            try pre.memberType(sym1)
            catch {
              case ex: MalformedType =>
                if (settings.debug.value) ex.printStackTrace
                val sym2 = underlyingSymbol(sym1)
                val itype = pre.memberType(sym2)
                ErrorUtils.issueTypeError(
                  AccessError(tree, sym, pre, context.enclClass.owner,
                          "\n because its instance type "+itype+
                          (if ("malformed type: "+itype.toString==ex.msg) " is malformed"
                           else " contains a "+ex.msg)))(context)
                ErrorType
            }
          }
          tree setSymbol sym1 setType {
            pre match {
              case _: SuperType => owntype map (tp => if (tp eq pre) site.symbol.thisType else tp)
              case _            => owntype
            }
          }
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
      def isCompatibleByName(tp: Type, pt: Type): Boolean = pt match {
        case TypeRef(_, ByNameParamClass, List(res)) if !isByNameParamType(tp) => isCompatible(tp, res)
        case _ => false
      }
      val tp1 = normalize(tp)
      (tp1 weak_<:< pt) || isCoercible(tp1, pt) || isCompatibleByName(tp, pt)
    }
    def isCompatibleArgs(tps: List[Type], pts: List[Type]) =
      (tps corresponds pts)(isCompatible)

    def isWeaklyCompatible(tp: Type, pt: Type): Boolean =
      pt.typeSymbol == UnitClass || // can perform unit coercion
      isCompatible(tp, pt) ||
      tp.isInstanceOf[MethodType] && // can perform implicit () instantiation
      tp.params.isEmpty && isCompatible(tp.resultType, pt)

    /** Like weakly compatible but don't apply any implicit conversions yet.
     *  Used when comparing the result type of a method with its prototype.
     *
     *  [Martin] I think Infer is also created by Erasure, with the default
     *  implementation of isCoercible
     *  [Paulp] (Assuming the above must refer to my comment on isCoercible)
     *  Nope, I examined every occurrence of Inferencer in trunk.  It
     *  appears twice as a self-type, once at its definition, and once
     *  where it is instantiated in Typers.  There are no others.
     *
         % ack -A0 -B0 --no-filename '\bInferencer\b' src
             self: Inferencer =>
             self: Inferencer =>
           class Inferencer(context: Context) extends InferencerContextErrors with InferCheckable {
             val infer = new Inferencer(context0) {
     */
    def isConservativelyCompatible(tp: Type, pt: Type): Boolean =
      context.withImplicitsDisabled(isWeaklyCompatible(tp, pt))

    /** This is overridden in the Typer.infer with some logic, but since
     *  that's the only place in the compiler an Inferencer is ever created,
     *  I suggest this should either be abstract or have the implementation.
     */
    def isCoercible(tp: Type, pt: Type): Boolean = false

    /* -- Type instantiation------------------------------------------------ */

    /** Replace any (possibly bounded) wildcard types in type `tp`
     *  by existentially bound variables.
     */
    def makeFullyDefined(tp: Type): Type = {
      val tparams = new ListBuffer[Symbol]
      def addTypeParam(bounds: TypeBounds): Type = {
        val tparam = context.owner.newExistential(newTypeName("_"+tparams.size), context.tree.pos.focus) setInfo bounds
        tparams += tparam
        tparam.tpe
      }
      val tp1 = tp map {
        case WildcardType =>
          addTypeParam(TypeBounds.empty)
        case BoundedWildcardType(bounds) =>
          addTypeParam(bounds)
        case t => t
      }
      existentialAbstraction(tparams.toList, tp1)
    }

    /** Return inferred type arguments of polymorphic expression, given
     *  its type parameters and result type and a prototype `pt`.
     *  If no minimal type variables exist that make the
     *  instantiated type a subtype of `pt`, return null.
     *
     *  @param tparams ...
     *  @param restpe  ...
     *  @param pt      ...
     *  @return        ...
     */
    private def exprTypeArgs(tparams: List[Symbol], restpe: Type, pt: Type, useWeaklyCompatible: Boolean = false): (List[Type], List[TypeVar]) = {
      val tvars = tparams map freshVar
      val instResTp = restpe.instantiateTypeParams(tparams, tvars)
      if ( if (useWeaklyCompatible) isWeaklyCompatible(instResTp, pt) else isCompatible(instResTp, pt) ) {
        try {
          // If the restpe is an implicit method, and the expected type is fully defined
          // optimize type variables wrt to the implicit formals only; ignore the result type.
          // See test pos/jesper.scala
          val varianceType = restpe match {
            case mt: MethodType if mt.isImplicit && isFullyDefined(pt) =>
              MethodType(mt.params, AnyClass.tpe)
            case _ =>
              restpe
          }
          //println("try to solve "+tvars+" "+tparams)
          (solvedTypes(tvars, tparams, tparams map varianceInType(varianceType),
                      false, lubDepth(List(restpe, pt))), tvars)
        } catch {
          case ex: NoInstance => (null, null)
        }
      } else (null, null)
    }

    /** Return inferred proto-type arguments of function, given
    *  its type and value parameters and result type, and a
    *  prototype `pt` for the function result.
    *  Type arguments need to be either determined precisely by
    *  the prototype, or they are maximized, if they occur only covariantly
    *  in the value parameter list.
    *  If instantiation of a type parameter fails,
    *  take WildcardType for the proto-type argument.
    *
    *  @param tparams ...
    *  @param formals ...
    *  @param restype ...
    *  @param pt      ...
    *  @return        ...
    */
    def protoTypeArgs(tparams: List[Symbol], formals: List[Type], restpe: Type,
                      pt: Type): List[Type] = {
      /** Map type variable to its instance, or, if `variance` is covariant/contravariant,
       *  to its upper/lower bound */
      def instantiateToBound(tvar: TypeVar, variance: Int): Type = try {
        lazy val hiBounds = tvar.constr.hiBounds
        lazy val loBounds = tvar.constr.loBounds
        lazy val upper = glb(hiBounds)
        lazy val lower = lub(loBounds)
        def setInst(tp: Type): Type = {
          tvar setInst tp
          assertNonCyclic(tvar)//debug
          instantiate(tvar.constr.inst)
        }
        //Console.println("instantiate "+tvar+tvar.constr+" variance = "+variance);//DEBUG
        if (tvar.constr.inst != NoType)
          instantiate(tvar.constr.inst)
        else if ((variance & COVARIANT) != 0 && hiBounds.nonEmpty)
          setInst(upper)
        else if ((variance & CONTRAVARIANT) != 0 && loBounds.nonEmpty)
          setInst(lower)
        else if (hiBounds.nonEmpty && loBounds.nonEmpty && upper <:< lower)
          setInst(upper)
        else
          WildcardType
      } catch {
        case ex: NoInstance => WildcardType
      }
      val tvars = tparams map freshVar
      if (isConservativelyCompatible(restpe.instantiateTypeParams(tparams, tvars), pt))
        map2(tparams, tvars)((tparam, tvar) =>
          instantiateToBound(tvar, varianceInTypes(formals)(tparam)))
      else
        tvars map (tvar => WildcardType)
    }

    /** [Martin] Can someone comment this please? I have no idea what it's for
     *  and the code is not exactly readable.
     */
    object AdjustedTypeArgs {
      val Result = scala.collection.mutable.LinkedHashMap
      type Result = scala.collection.mutable.LinkedHashMap[Symbol, Option[Type]]

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
          (okArgs, okTparams, m.values.map(_.getOrElse(NothingClass.tpe)), nok.keys)
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
          && (restpe.isWildcard || (varianceInType(restpe)(tparam) & COVARIANT) == 0) // don't retract covariant occurrences
        )

        // checks !settings.XoldPatmat.value directly so one need not run under -Xexperimental to use virtpatmat
        buf += ((tparam,
          if (retract) None
          else Some(
            if (targ.typeSymbol == RepeatedParamClass)     targ.baseType(SeqClass)
            else if (targ.typeSymbol == JavaRepeatedParamClass) targ.baseType(ArrayClass)
            // this infers Foo.type instead of "object Foo" (see also widenIfNecessary)
            else if (targ.typeSymbol.isModuleClass || ((settings.Xexperimental.value || !settings.XoldPatmat.value) && tvar.constr.avoidWiden)) targ
            else targ.widen
          )
        ))
      }
      buf.result
    }

    /** Return inferred type arguments, given type parameters, formal parameters,
    *  argument types, result type and expected result type.
    *  If this is not possible, throw a `NoInstance` exception.
    *  Undetermined type arguments are represented by `definitions.NothingClass.tpe`.
    *  No check that inferred parameters conform to their bounds is made here.
    *
    *  @param   tparams         the type parameters of the method
    *  @param   formals         the value parameter types of the method
    *  @param   restp           the result type of the method
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
      val targs = solvedTypes(
        tvars, tparams, tparams map varianceInTypes(formals),
        false, lubDepth(formals) max lubDepth(argtpes)
      )
      // Can warn about inferring Any/AnyVal as long as they don't appear
      // explicitly anywhere amongst the formal, argument, result, or expected type.
      def canWarnAboutAny = !(pt :: restpe :: formals ::: argtpes exists (t => (t contains AnyClass) || (t contains AnyValClass)))
      def argumentPosition(idx: Int): Position = context.tree match {
        case x: ValOrDefDef => x.rhs match {
          case Apply(fn, args) if idx < args.size => args(idx).pos
          case _                                  => context.tree.pos
        }
        case _ => context.tree.pos
      }
      if (settings.warnInferAny.value && context.reportErrors && canWarnAboutAny) {
        foreachWithIndex(targs) ((targ, idx) =>
          targ.typeSymbol match {
            case sym @ (AnyClass | AnyValClass) =>
              context.unit.warning(argumentPosition(idx), s"a type was inferred to be `${sym.name}`; this may indicate a programming error.")
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
        alts exists (alt => isApplicableBasedOnArity(pre memberType alt, argsCount, varargsStar, tuplingAllowed))
      case _ =>
        val paramsCount   = tpe.params.length
        val simpleMatch   = paramsCount == argsCount
        val varargsTarget = isVarArgsList(tpe.params)
        def varargsMatch  = varargsTarget && (paramsCount - 1) <= argsCount
        def tuplingMatch  = tuplingAllowed && eligibleForTupleConversion(paramsCount, argsCount, varargsTarget)

        // A varargs star call, e.g. (x, y:_*) can only match a varargs method
        // with the same number of parameters.  See SI-5859 for an example of what
        // would fail were this not enforced before we arrived at isApplicable.
        if (varargsStar)
          varargsTarget && simpleMatch
        else
          simpleMatch || varargsMatch || tuplingMatch
    }

    private[typechecker] def followApply(tp: Type): Type = tp match {
      case NullaryMethodType(restp) =>
        val restp1 = followApply(restp)
        if (restp1 eq restp) tp else restp1
      case _ =>
        val appmeth = {
          //OPT cut down on #closures by special casing non-overloaded case
          // was: tp.nonPrivateMember(nme.apply) filter (_.isPublic)
          val result = tp.nonPrivateMember(nme.apply)
          if ((result eq NoSymbol) || !result.isOverloaded && result.isPublic) result
          else result filter (_.isPublic)
        }
        if (appmeth == NoSymbol) tp
        else OverloadedType(tp, appmeth.alternatives)
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
    private def checkNames(argtpes: List[Type], params: List[Symbol]) = {
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
              res = UnitClass.tpe
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
      if (argtpes.isEmpty) UnitClass.tpe           // aka "Tuple0"
      else tupleType(argtpes map {
        case NamedType(name, tp) => UnitClass.tpe  // not a named arg - only assignments here
        case RepeatedType(tp)    => tp             // but probably shouldn't be tupling a call containing :_*
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

    /** Is there an instantiation of free type variables `undetparams`
     *  such that function type `ftpe` is applicable to
     *  `argtpes` and its result conform to `pt`?
     *
     *  @param undetparams ...
     *  @param ftpe        the type of the function (often a MethodType)
     *  @param argtpes     the argument types; a NamedType(name, tp) for named
     *    arguments. For each NamedType, if `name` does not exist in `ftpe`, that
     *    type is set to `Unit`, i.e. the corresponding argument is treated as
     *    an assignment expression (@see checkNames).
     *  @param pt          ...
     *  @return            ...
     */
    private def isApplicable(undetparams: List[Symbol], ftpe: Type,
                             argtpes0: List[Type], pt: Type): Boolean =
      ftpe match {
        case OverloadedType(pre, alts) =>
          alts exists (alt => isApplicable(undetparams, pre memberType alt, argtpes0, pt))
        case ExistentialType(tparams, qtpe) =>
          isApplicable(undetparams, qtpe, argtpes0, pt)
        case mt @ MethodType(params, _) =>
          val argslen = argtpes0.length
          val formals = formalTypes(mt.paramTypes, argslen, removeByName = false)

          def tryTupleApply = {
            val tupled = tupleIfNecessary(mt.paramTypes, argtpes0)
            (tupled ne argtpes0) && isApplicable(undetparams, ftpe, tupled, pt)
          }
          def typesCompatible(argtpes: List[Type]) = {
            val restpe = ftpe.resultType(argtpes)
            if (undetparams.isEmpty) {
              isCompatibleArgs(argtpes, formals) && isWeaklyCompatible(restpe, pt)
            } else {
              try {
                val AdjustedTypeArgs.Undets(okparams, okargs, leftUndet) = methTypeArgs(undetparams, formals, restpe, argtpes, pt)
                // #2665: must use weak conformance, not regular one (follow the monomorphic case above)
                (exprTypeArgs(leftUndet, restpe.instantiateTypeParams(okparams, okargs), pt, useWeaklyCompatible = true)._1 ne null) &&
                isWithinBounds(NoPrefix, NoSymbol, okparams, okargs)
              } catch {
                case ex: NoInstance => false
              }
            }
          }

          // very similar logic to doTypedApply in typechecker
          val lencmp = compareLengths(argtpes0, formals)
          if (lencmp > 0) tryTupleApply
          else if (lencmp == 0) {
            // fast track if no named arguments are used
            if (!containsNamedType(argtpes0))
              typesCompatible(argtpes0)
            else {
              // named arguments are used
              val (argtpes1, argPos, namesOK) = checkNames(argtpes0, params)
              // when using named application, the vararg param has to be specified exactly once
              (    namesOK
                && (isIdentity(argPos) || sameLength(formals, params))
                && typesCompatible(reorderArgs(argtpes1, argPos)) // nb. arguments and names are OK, check if types are compatible
              )
            }
          }
          else {
            // not enough arguments, check if applicable using defaults
            val missing = missingParams[Type](argtpes0, params, {
              case NamedType(name, _) => Some(name)
              case _ => None
            })._1
            if (missing forall (_.hasDefault)) {
              // add defaults as named arguments
              val argtpes1 = argtpes0 ::: (missing map (p => NamedType(p.name, p.tpe)))
              isApplicable(undetparams, ftpe, argtpes1, pt)
            }
            else tryTupleApply
          }

        case NullaryMethodType(restpe) => // strip nullary method type, which used to be done by the polytype case below
          isApplicable(undetparams, restpe, argtpes0, pt)
        case PolyType(tparams, restpe) =>
          createFromClonedSymbols(tparams, restpe)((tps1, restpe1) => isApplicable(tps1 ::: undetparams, restpe1, argtpes0, pt))
        case ErrorType =>
          true
        case _ =>
          false
      }

    /**
     * Todo: Try to make isApplicable always safe (i.e. not cause TypeErrors).
     * The chance of TypeErrors should be reduced through context errors
     */
    private[typechecker] def isApplicableSafe(undetparams: List[Symbol], ftpe: Type,
                                              argtpes0: List[Type], pt: Type): Boolean = {
      val silentContext = context.makeSilent(false)
      val typer0 = newTyper(silentContext)
      val res1 = typer0.infer.isApplicable(undetparams, ftpe, argtpes0, pt)
      if (pt != WildcardType && silentContext.hasErrors) {
        silentContext.flushBuffer()
        val res2 = typer0.infer.isApplicable(undetparams, ftpe, argtpes0, WildcardType)
        if (silentContext.hasErrors) false else res2
      } else res1
    }

    /** Is type `ftpe1` strictly more specific than type `ftpe2`
     *  when both are alternatives in an overloaded function?
     *  @see SLS (sec:overloading-resolution)
     *
     *  @param ftpe1 ...
     *  @param ftpe2 ...
     *  @return      ...
     */
    def isAsSpecific(ftpe1: Type, ftpe2: Type): Boolean = ftpe1 match {
      case OverloadedType(pre, alts) =>
        alts exists (alt => isAsSpecific(pre memberType alt, ftpe2))
      case et: ExistentialType =>
        isAsSpecific(ftpe1.skolemizeExistential, ftpe2)
        //et.withTypeVars(isAsSpecific(_, ftpe2))
      case NullaryMethodType(res) =>
        isAsSpecific(res, ftpe2)
      case mt: MethodType if mt.isImplicit =>
        isAsSpecific(ftpe1.resultType, ftpe2)
      case mt @ MethodType(params, _) if params.nonEmpty =>
        var argtpes = mt.paramTypes
        if (isVarArgsList(params) && isVarArgsList(ftpe2.params))
          argtpes = argtpes map (argtpe =>
            if (isRepeatedParamType(argtpe)) argtpe.typeArgs.head else argtpe)
        isApplicable(List(), ftpe2, argtpes, WildcardType)
      case PolyType(tparams, NullaryMethodType(res)) =>
        isAsSpecific(PolyType(tparams, res), ftpe2)
      case PolyType(tparams, mt: MethodType) if mt.isImplicit =>
        isAsSpecific(PolyType(tparams, mt.resultType), ftpe2)
      case PolyType(_, (mt @ MethodType(params, _))) if params.nonEmpty =>
        isApplicable(List(), ftpe2, mt.paramTypes, WildcardType)
      // case NullaryMethodType(res) =>
      //   isAsSpecific(res, ftpe2)
      case ErrorType =>
        true
      case _ =>
        ftpe2 match {
          case OverloadedType(pre, alts) =>
            alts forall (alt => isAsSpecific(ftpe1, pre memberType alt))
          case et: ExistentialType =>
            et.withTypeVars(isAsSpecific(ftpe1, _))
          case mt: MethodType =>
            !mt.isImplicit || isAsSpecific(ftpe1, mt.resultType)
          case NullaryMethodType(res) =>
            isAsSpecific(ftpe1, res)
          case PolyType(tparams, NullaryMethodType(res)) =>
            isAsSpecific(ftpe1, PolyType(tparams, res))
          case PolyType(tparams, mt: MethodType) =>
            !mt.isImplicit || isAsSpecific(ftpe1, PolyType(tparams, mt.resultType))
          case _ =>
            isAsSpecificValueType(ftpe1, ftpe2, List(), List())
        }
    }
    private def isAsSpecificValueType(tpe1: Type, tpe2: Type, undef1: List[Symbol], undef2: List[Symbol]): Boolean = (tpe1, tpe2) match {
      case (PolyType(tparams1, rtpe1), _) =>
        isAsSpecificValueType(rtpe1, tpe2, undef1 ::: tparams1, undef2)
      case (_, PolyType(tparams2, rtpe2)) =>
        isAsSpecificValueType(tpe1, rtpe2, undef1, undef2 ::: tparams2)
      case _ =>
        existentialAbstraction(undef1, tpe1) <:< existentialAbstraction(undef2, tpe2)
    }


/*
    def isStrictlyMoreSpecific(ftpe1: Type, ftpe2: Type): Boolean =
      ftpe1.isError || isAsSpecific(ftpe1, ftpe2) &&
      (!isAsSpecific(ftpe2, ftpe1) ||
       !ftpe1.isInstanceOf[OverloadedType] && ftpe2.isInstanceOf[OverloadedType] ||
       phase.erasedTypes && covariantReturnOverride(ftpe1, ftpe2))
*/
    /** Is sym1 (or its companion class in case it is a module) a subclass of
     *  sym2 (or its companion class in case it is a module)?
     */
    def isProperSubClassOrObject(sym1: Symbol, sym2: Symbol): Boolean = (
      (sym1 != sym2) && (sym1 != NoSymbol) && (
           (sym1 isSubClass sym2)
        || (sym1.isModuleClass && isProperSubClassOrObject(sym1.linkedClassOfClass, sym2))
        || (sym2.isModuleClass && isProperSubClassOrObject(sym1, sym2.linkedClassOfClass))
      )
    )

    /** is symbol `sym1` defined in a proper subclass of symbol `sym2`?
     */
    def isInProperSubClassOrObject(sym1: Symbol, sym2: Symbol) =
      sym2 == NoSymbol || isProperSubClassOrObject(sym1.owner, sym2.owner)

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
//        println("is more specific? "+sym1+":"+ftpe1+sym1.locationString+"/"+sym2+":"+ftpe2+sym2.locationString+":"+
//                specificCount+"/"+subClassCount)
        specificCount + subClassCount > 0
      }
    }
/*
      ftpe1.isError || {
        if (isAsSpecific(ftpe1, ftpe2))
          (!isAsSpecific(ftpe2, ftpe1) ||
           isProperSubClassOrObject(sym1.owner, sym2.owner) ||
           !ftpe1.isInstanceOf[OverloadedType] && ftpe2.isInstanceOf[OverloadedType] ||
           phase.erasedTypes && covariantReturnOverride(ftpe1, ftpe2))
        else
          !isAsSpecific(ftpe2, ftpe1) &&
          isProperSubClassOrObject(sym1.owner, sym2.owner)
      }
*/
    private def covariantReturnOverride(ftpe1: Type, ftpe2: Type): Boolean = (ftpe1, ftpe2) match {
      case (MethodType(_, rtpe1), MethodType(_, rtpe2)) =>
        rtpe1 <:< rtpe2 || rtpe2.typeSymbol == ObjectClass
      case _ =>
        false
    }
/*
    /** Is type `tpe1` a strictly better expression alternative than type `tpe2`?
     */
    def isStrictlyBetterExpr(tpe1: Type, tpe2: Type) = {
      isMethod(tpe2) && !isMethod(tpe1) ||
      isNullary(tpe1) && !isNullary(tpe2) ||
      isStrictlyBetter(tpe1, tpe2)
    }

    /** Is type `tpe1` a strictly better alternative than type `tpe2`?
     *  non-methods are always strictly better than methods
     *  nullary methods are always strictly better than non-nullary
     *  if both are non-nullary methods, then tpe1 is strictly better than tpe2 if
     *   - tpe1 specializes tpe2 and tpe2 does not specialize tpe1
     *   - tpe1 and tpe2 specialize each other and tpe1 has a strictly better resulttype than
     *     tpe2
     */
    def isStrictlyBetter(tpe1: Type, tpe2: Type) = {
      def isNullary(tpe: Type): Boolean = tpe match {
        case tp: RewrappingTypeProxy => isNullary(tp.underlying)
        case _ => tpe.paramSectionCount == 0 || tpe.params.isEmpty
      }
      def isMethod(tpe: Type): Boolean = tpe match {
        case tp: RewrappingTypeProxy => isMethod(tp.underlying)
        case MethodType(_, _) | PolyType(_, _) => true
        case _ => false
      }
      def hasStrictlyBetterResult =
        resultIsBetter(tpe1, tpe2, List(), List()) && !resultIsBetter(tpe2, tpe1, List(), List())
      if (!isMethod(tpe1))
        isMethod(tpe2) || hasStrictlyBetterResult

      isNullary(tpe1) && !isNullary(tpe2) ||
      is

      else if (isNullary(tpe1))
        isMethod(tpe2) && (!isNullary(tpe2) || hasStrictlyBetterResult)
      else
        specializes(tpe1, tpe2) && (!specializes(tpe2, tpe1) || hasStrictlyBetterResult)
    }

*/
    /** error if arguments not within bounds. */
    def checkBounds(tree: Tree, pre: Type, owner: Symbol,
                    tparams: List[Symbol], targs: List[Type], prefix: String): Boolean = {
      //@M validate variances & bounds of targs wrt variances & bounds of tparams
      //@M TODO: better place to check this?
      //@M TODO: errors for getters & setters are reported separately
      val kindErrors = checkKindBounds(tparams, targs, pre, owner)

      if(!kindErrors.isEmpty) {
        if (targs contains WildcardType) true
        else { KindBoundErrors(tree, prefix, targs, tparams, kindErrors); false }
      } else if (!isWithinBounds(pre, owner, tparams, targs)) {
        if (!(targs exists (_.isErroneous)) && !(tparams exists (_.isErroneous))) {
          NotWithinBounds(tree, prefix, targs, tparams, kindErrors)
          false
        } else true
      } else true
    }

    def checkKindBounds(tparams: List[Symbol], targs: List[Type], pre: Type, owner: Symbol): List[String] = {
      checkKindBounds0(tparams, targs, pre, owner, true) map {
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
      printInference(
        ptBlock("inferArgumentInstance",
          "tree"        -> tree,
          "tree.tpe"    -> tree.tpe,
          "undetparams" -> undetparams,
          "strictPt"    -> strictPt,
          "lenientPt"   -> lenientPt
        )
      )
      var targs = exprTypeArgs(undetparams, tree.tpe, strictPt)._1
      if ((targs eq null) || !(tree.tpe.subst(undetparams, targs) <:< strictPt))
        targs = exprTypeArgs(undetparams, tree.tpe, lenientPt)._1

      substExpr(tree, undetparams, targs, lenientPt)
      printInference("[inferArgumentInstance] finished, targs = " + targs)
    }

    /** Infer type arguments `targs` for `tparams` of polymorphic expression in `tree`, given prototype `pt`.
     *
     * Substitute `tparams` to `targs` in `tree`, after adjustment by `adjustTypeArgs`, returning the type parameters that were not determined
     * If passed, infers against specified type `treeTp` instead of `tree.tp`.
     */
    def inferExprInstance(tree: Tree, tparams: List[Symbol], pt: Type = WildcardType, treeTp0: Type = null, keepNothings: Boolean = true, useWeaklyCompatible: Boolean = false): List[Symbol] = {
      val treeTp = if(treeTp0 eq null) tree.tpe else treeTp0 // can't refer to tree in default for treeTp0
      printInference(
        ptBlock("inferExprInstance",
          "tree"    -> tree,
          "tree.tpe"-> tree.tpe,
          "tparams" -> tparams,
          "pt"      -> pt
        )
      )
      val (targs, tvars) = exprTypeArgs(tparams, treeTp, pt, useWeaklyCompatible)

      if (keepNothings || (targs eq null)) { //@M: adjustTypeArgs fails if targs==null, neg/t0226
        substExpr(tree, tparams, targs, pt)
        List()
      } else {
        val AdjustedTypeArgs.Undets(okParams, okArgs, leftUndet) = adjustTypeArgs(tparams, tvars, targs)
        printInference(
          ptBlock("inferExprInstance/AdjustedTypeArgs",
            "okParams" -> okParams,
            "okArgs" -> okArgs,
            "leftUndet" -> leftUndet
          )
        )
        substExpr(tree, okParams, okArgs, pt)
        leftUndet
      }
    }

    /** Substitute free type variables `undetparams` of polymorphic argument
     *  expression `tree` to `targs`, Error if `targs` is null.
     *
     *  @param tree ...
     *  @param undetparams ...
     *  @param targs ...
     *  @param pt ...
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
     *  @param pt          the expected type of the function application
     *  @return            The type parameters that remain uninstantiated,
     *                     and that thus have not been substituted.
     */
    def inferMethodInstance(fn: Tree, undetparams: List[Symbol],
                            args: List[Tree], pt0: Type): List[Symbol] = fn.tpe match {
      case mt @ MethodType(params0, _) =>
        try {
          val pt       = if (pt0.typeSymbol == UnitClass) WildcardType else pt0
          val formals  = formalTypes(mt.paramTypes, args.length)
          val argtpes  = tupleIfNecessary(formals, args map (x => elimAnonymousClass(x.tpe.deconst)))
          val restpe   = fn.tpe.resultType(argtpes)

          val AdjustedTypeArgs.AllArgsAndUndets(okparams, okargs, allargs, leftUndet) =
            methTypeArgs(undetparams, formals, restpe, argtpes, pt)

          printInference("[infer method] solving for %s in %s based on (%s)%s (%s)".format(
            undetparams.map(_.name).mkString(", "),
            fn.tpe,
            argtpes.mkString(", "),
            restpe,
            (okparams map (_.name), okargs).zipped.map(_ + "=" + _).mkString("solved: ", ", ", "")
          ))

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

    def widen(tp: Type): Type = abstractTypesToBounds(tp)

    /** Substitute free type variables `undetparams` of type constructor
     *  `tree` in pattern, given prototype `pt`.
     *
     *  @param tree        the constuctor that needs to be instantiated
     *  @param undetparams the undetermined type parameters
     *  @param pt          the expected result type of the instance
     */
    def inferConstructorInstance(tree: Tree, undetparams: List[Symbol], pt0: Type) {
      val pt       = widen(pt0)
      val ptparams = freeTypeParamsOfTerms(pt)
      val ctorTp   = tree.tpe
      val resTp    = ctorTp.finalResultType

      debuglog("infer constr inst "+ tree +"/"+ undetparams +"/ pt= "+ pt +" pt0= "+ pt0 +" resTp: "+ resTp)

      /** Compute type arguments for undetermined params
       */
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
            val targs      = solvedTypes(tvars, undetparams, variances, true, lubDepth(List(resTp, pt)))
            // checkBounds(tree, NoPrefix, NoSymbol, undetparams, targs, "inferred ")
            // no checkBounds here. If we enable it, test bug602 fails.
            // TODO: reinstate checkBounds, return params that fail to meet their bounds to undetparams
            Some(targs)
          } catch ifNoInstance { msg =>
            debuglog("NO INST "+ (tvars, tvars map (_.constr)))
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

      val inferred = inferFor(pt) orElse inferForApproxPt

      inferred match {
        case Some(targs) =>
          new TreeTypeSubstituter(undetparams, targs).traverse(tree)
          notifyUndetparamsInferred(undetparams, targs)
        case _ =>
          debugwarn("failed inferConstructorInstance for "+ tree  +" : "+ tree.tpe +" under "+ undetparams +" pt = "+ pt +(if(isFullyDefined(pt)) " (fully defined)" else " (not fully defined)"))
          // if (settings.explaintypes.value) explainTypes(resTp.instantiateTypeParams(undetparams, tvars), pt)
          ConstrInstantiationError(tree, resTp, pt)
      }
    }

    def instBounds(tvar: TypeVar): (Type, Type) = {
      val tparam = tvar.origin.typeSymbol
      val instType = toOrigin(tvar.constr.inst)
      val (loBounds, hiBounds) =
        if (instType != NoType && isFullyDefined(instType)) (List(instType), List(instType))
        else (tvar.constr.loBounds, tvar.constr.hiBounds)
      val lo = lub(tparam.info.bounds.lo :: loBounds map toOrigin)
      val hi = glb(tparam.info.bounds.hi :: hiBounds map toOrigin)
      (lo, hi)
    }

    def isInstantiatable(tvars: List[TypeVar]) = {
      val tvars1 = tvars map (_.cloneInternal)
      // Note: right now it's not clear that solving is complete, or how it can be made complete!
      // So we should come back to this and investigate.
      solve(tvars1, tvars1 map (_.origin.typeSymbol), tvars1 map (x => COVARIANT), false)
    }

    // this is quite nasty: it destructively changes the info of the syms of e.g., method type params (see #3692, where the type param T's bounds were set to >: T <: T, so that parts looped)
    // the changes are rolled back by restoreTypeBounds, but might be unintentially observed in the mean time
    def instantiateTypeVar(tvar: TypeVar) {
      val tparam = tvar.origin.typeSymbol
      if (false &&
          tvar.constr.inst != NoType &&
          isFullyDefined(tvar.constr.inst) &&
          (tparam.info.bounds containsType tvar.constr.inst)) {
        context.nextEnclosing(_.tree.isInstanceOf[CaseDef]).pushTypeBounds(tparam)
        tparam setInfo tvar.constr.inst
        tparam resetFlag DEFERRED
        debuglog("new alias of " + tparam + " = " + tparam.info)
      } else {
        val (lo, hi) = instBounds(tvar)
        if (lo <:< hi) {
          if (!((lo <:< tparam.info.bounds.lo) && (tparam.info.bounds.hi <:< hi)) // bounds were improved
             && tparam != lo.typeSymbolDirect && tparam != hi.typeSymbolDirect) { // don't create illegal cycles
            context.nextEnclosing(_.tree.isInstanceOf[CaseDef]).pushTypeBounds(tparam)
            tparam setInfo TypeBounds(lo, hi)
            debuglog("new bounds of " + tparam + " = " + tparam.info)
          } else {
            debuglog("redundant: "+tparam+" "+tparam.info+"/"+lo+" "+hi)
          }
        } else {
          debuglog("inconsistent: "+tparam+" "+lo+" "+hi)
        }
      }
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
      val pt        = widen(pt0)
      val ptparams  = freeTypeParamsOfTerms(pt)
      val tpparams  = freeTypeParamsOfTerms(pattp)

      def ptMatchesPattp = pt matchesPattern pattp.widen
      def pattpMatchesPt = pattp matchesPattern pt

      /** If we can absolutely rule out a match we can fail early.
       *  This is the case if the scrutinee has no unresolved type arguments
       *  and is a "final type", meaning final + invariant in all type parameters.
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
      /** If the scrutinee has free type parameters but the pattern does not,
       *  we have to flip the arguments so the expected type is treated as more
       *  general when calculating the intersection.  See run/bug2755.scala.
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
      def apply(tp: Type): Type = tp.normalize match {
        case TypeRef(pre, sym, _) if sym.isAbstractType => WildcardType
        case _ => mapOver(tp)
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

/*
    def checkNotShadowed(pos: Position, pre: Type, best: Symbol, eligible: List[Symbol]) =
      if (!phase.erasedTypes)
        for (alt <- eligible) {
          if (isProperSubClassOrObject(alt.owner, best.owner))
            error(pos,
                  "erroneous reference to overloaded definition,\n"+
                  "most specific definition is: "+best+best.locationString+" of type "+pre.memberType(best)+
                  ",\nyet alternative definition   "+alt+alt.locationString+" of type "+pre.memberType(alt)+
                  "\nis defined in a subclass")
        }
*/

    /** Assign `tree` the symbol and type of the alternative which
     *  matches prototype `pt`, if it exists.
     *  If several alternatives match `pt`, take parameterless one.
     *  If no alternative matches `pt`, take the parameterless one anyway.
     */
    def inferExprAlternative(tree: Tree, pt: Type) = tree.tpe match {
      case OverloadedType(pre, alts) => tryTwice { isSecondTry =>
        val alts0 = alts filter (alt => isWeaklyCompatible(pre.memberType(alt), pt))
        val alts1 = if (alts0.isEmpty) alts else alts0

        val bests = bestAlternatives(alts1) { (sym1, sym2) =>
          val tp1 = pre.memberType(sym1)
          val tp2 = pre.memberType(sym2)

          (    tp2 == ErrorType
            || (!isWeaklyCompatible(tp2, pt) && isWeaklyCompatible(tp1, pt))
            || isStrictlyMoreSpecific(tp1, tp2, sym1, sym2)
          )
        }
        // todo: missing test case for bests.isEmpty
        bests match {
          case best :: Nil                              => tree setSymbol best setType (pre memberType best)
          case best :: competing :: _ if alts0.nonEmpty => if (!pt.isErroneous) AmbiguousExprAlternativeError(tree, pre, best, competing, pt, isSecondTry)
          case _                                        => if (bests.isEmpty || alts0.isEmpty) NoBestExprAlternativeError(tree, pt, isSecondTry)
        }
      }
    }

    @inline private def inSilentMode(context: Context)(expr: => Boolean): Boolean = {
      val oldState = context.state
      context.setBufferErrors()
      val res = expr
      val contextWithErrors = context.hasErrors
      context.flushBuffer()
      context.restoreState(oldState)
      res && !contextWithErrors
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
      // If there are any foo=bar style arguments, and any of the overloaded
      // methods has a parameter named `foo`, then only those methods are considered.
      val namesMatch = namesOfNamedArguments(argtpes) match {
        case Nil   => Nil
        case names => eligible filter (m => names forall (name => m.info.params exists (p => paramMatchesName(p, name))))
      }
      if (namesMatch.nonEmpty)
        namesMatch
      else if (eligible.isEmpty || eligible.tail.isEmpty)
        eligible
      else
        eligible filter (alt =>
          !alt.hasDefault && isApplicableBasedOnArity(alt.tpe, argtpes.length, varargsStar, tuplingAllowed = true)
        )
    }

    /** Assign `tree` the type of an alternative which is applicable
     *  to `argtpes`, and whose result type is compatible with `pt`.
     *  If several applicable alternatives exist, drop the alternatives which use
     *  default arguments, then select the most specialized one.
     *  If no applicable alternative exists, and pt != WildcardType, try again
     *  with pt = WildcardType.
     *  Otherwise, if there is no best alternative, error.
     *
     *  @param argtpes contains the argument types. If an argument is named, as
     *    "a = 3", the corresponding type is `NamedType("a", Int)'. If the name
     *    of some NamedType does not exist in an alternative's parameter names,
     *    the type is replaces by `Unit`, i.e. the argument is treated as an
     *    assignment expression.
     *
     *  @pre  tree.tpe is an OverloadedType.
     */
    def inferMethodAlternative(tree: Tree, undetparams: List[Symbol], argtpes0: List[Type], pt0: Type): Unit = {
      val OverloadedType(pre, alts) = tree.tpe
      var varargsStar = false
      val argtpes = argtpes0 mapConserve {
        case RepeatedType(tp) => varargsStar = true ; tp
        case tp               => tp
      }
      def followType(sym: Symbol) = followApply(pre memberType sym)
      def bestForExpectedType(pt: Type, isLastTry: Boolean): Unit = {
        val applicable0 = alts filter (alt => inSilentMode(context)(isApplicable(undetparams, followType(alt), argtpes, pt)))
        val applicable  = overloadsToConsiderBySpecificity(applicable0, argtpes, varargsStar)
        val ranked      = bestAlternatives(applicable)((sym1, sym2) =>
          isStrictlyMoreSpecific(followType(sym1), followType(sym2), sym1, sym2)
        )
        ranked match {
          case best :: competing :: _    => AmbiguousMethodAlternativeError(tree, pre, best, competing, argtpes, pt, isLastTry) // ambiguous
          case best :: Nil               => tree setSymbol best setType (pre memberType best)           // success
          case Nil if pt eq WildcardType => NoBestMethodAlternativeError(tree, argtpes, pt, isLastTry)  // failed
          case Nil                       => bestForExpectedType(WildcardType, isLastTry)                // failed, but retry with WildcardType
        }
      }
      // This potentially makes up to four attempts: tryTwice may execute
      // with and without views enabled, and bestForExpectedType will try again
      // with pt = WildcardType if it fails with pt != WildcardType.
      tryTwice { isLastTry =>
        val pt = if (pt0.typeSymbol == UnitClass) WildcardType else pt0
        debuglog(s"infer method alt ${tree.symbol} with alternatives ${alts map pre.memberType} argtpes=$argtpes pt=$pt")
        bestForExpectedType(pt, isLastTry)
      }
    }

    /** Try inference twice, once without views and once with views,
     *  unless views are already disabled.
     *
     *  @param infer ...
     */
    def tryTwice(infer: Boolean => Unit): Unit = {
      if (context.implicitsEnabled) {
        val saved = context.state
        var fallback = false
        context.setBufferErrors()
        // We cache the current buffer because it is impossible to
        // distinguish errors that occurred before entering tryTwice
        // and our first attempt in 'withImplicitsDisabled'. If the
        // first attempt fails we try with implicits on *and* clean
        // buffer but that would also flush any pre-tryTwice valid
        // errors, hence some manual buffer tweaking is necessary.
        val errorsToRestore = context.flushAndReturnBuffer()
        try {
          context.withImplicitsDisabled(infer(false))
          if (context.hasErrors) {
            fallback = true
            context.restoreState(saved)
            context.flushBuffer()
            infer(true)
          }
        } catch {
          case ex: CyclicReference  => throw ex
          case ex: TypeError        => // recoverable cyclic references
            context.restoreState(saved)
            if (!fallback) infer(true) else ()
        } finally {
          context.restoreState(saved)
          context.updateBuffer(errorsToRestore)
        }
      }
      else infer(true)
    }

    /** Assign `tree` the type of all polymorphic alternatives
     *  with `nparams` as the number of type parameters, if it exists.
     *  If no such polymorphic alternative exist, error.
     *
     *  @param tree ...
     *  @param nparams ...
     */
    def inferPolyAlternatives(tree: Tree, argtypes: List[Type]): Unit = {
      val OverloadedType(pre, alts) = tree.tpe
      val sym0 = tree.symbol filter (alt => sameLength(alt.typeParams, argtypes))
      def fail(kind: PolyAlternativeErrorKind.ErrorType) =
        PolyAlternativeError(tree, argtypes, sym0, kind)

      if (sym0 == NoSymbol) return (
        if (alts exists (_.typeParams.nonEmpty))
          fail(PolyAlternativeErrorKind.WrongNumber)
        else fail(PolyAlternativeErrorKind.NoParams))

      val (resSym, resTpe) = {
        if (!sym0.isOverloaded)
          (sym0, pre.memberType(sym0))
        else {
          val sym = sym0 filter (alt => isWithinBounds(pre, alt.owner, alt.typeParams, argtypes))
          if (sym == NoSymbol) {
            if (argtypes forall (x => !x.isErroneous))
              fail(PolyAlternativeErrorKind.ArgsDoNotConform)
            return
          }
          else if (sym.isOverloaded) {
            val xs      = sym.alternatives
            val tparams = new AsSeenFromMap(pre, xs.head.owner) mapOver xs.head.typeParams
            val bounds  = tparams map (_.tpeHK) // see e.g., #1236
            val tpe     = PolyType(tparams, OverloadedType(AntiPolyType(pre, bounds), xs))

            (sym setInfo tpe, tpe)
          }
          else (sym, pre.memberType(sym))
        }
      }
      // Side effects tree with symbol and type
      tree setSymbol resSym setType resTpe
    }
  }
}

