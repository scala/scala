/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.typechecker

import scala.collection.mutable.ListBuffer
import symtab.Flags._

/** This trait ...
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
trait Infer requires Analyzer {
  import global._
  import definitions._
  import posAssigner.atPos

  // statistics
  var normM = 0
  var normP = 0
  var normO = 0

/* -- Type parameter inference utility functions --------------------------- */

  def assertNonCyclic(tvar: TypeVar) =
    assert(tvar.constr.inst != tvar, tvar.origin)

  def isVarArgs(formals: List[Type]) =
    !formals.isEmpty && (formals.last.symbol == RepeatedParamClass)

  /** The formal parameter types corresponding to <code>formals</code>.
   *  If <code>formals</code> has a repeated last parameter, a list of
   *  (nargs - params.length + 1) copies of its type is returned.
   *
   *  @param formals ...
   *  @param nargs ...
   */
  def formalTypes(formals: List[Type], nargs: int): List[Type] = {
    val formals1 = formals map {
      case TypeRef(_, sym, List(arg)) if (sym == ByNameParamClass) => arg
      case formal => formal
    }
    if (isVarArgs(formals1)) {
      val ft = formals1.last.typeArgs.head
      formals1.init ::: (for (val i <- List.range(formals1.length - 1, nargs)) yield ft)
    } else formals1
  }

  def actualTypes(actuals: List[Type], nformals: int): List[Type] =
    if (nformals == 1 && actuals.length != 1)
      List(if (actuals.length == 0) UnitClass.tpe else tupleType(actuals))
    else actuals

  def actualArgs(pos: PositionType, actuals: List[Tree], nformals: int): List[Tree] =
    if (nformals == 1 && actuals.length != 1) List(atPos(pos)(gen.mkTuple(actuals))) else actuals

  /** A fresh type varable with given type parameter as origin.
   *
   *  @param tparam ...
   *  @return       ...
   */
  def freshVar(tparam: Symbol): TypeVar =
    new TypeVar(tparam.tpe, new TypeConstraint)

  //todo: remove comments around following privates; right now they cause an IllegalAccess
  // error when built with scalac

  /*private*/ class NoInstance(msg: String) extends RuntimeException(msg)

  /*private*/ class DeferredNoInstance(getmsg: () => String) extends NoInstance("") {
    override def getMessage(): String = getmsg()
  }

  /*private*/ object instantiateMap extends TypeMap {
    def apply(t: Type): Type = instantiate(t)
  }

  /** map every TypeVar to its constraint.inst field.
   *  throw a NoInstance exception if a NoType or WildcardType is encountered.
   *
   *  @param  tp ...
   *  @return    ...
   *  @throws    NoInstance
   */
  def instantiate(tp: Type): Type = tp match {
    case WildcardType | NoType =>
      throw new NoInstance("undetermined type")
    case TypeVar(origin, constr) =>
      if (constr.inst != NoType) instantiate(constr.inst)
      else throw new DeferredNoInstance(() =>
        "no unique instantiation of type variable " + origin + " could be found")
    case _ =>
      instantiateMap.mapOver(tp)
  }

  /** Is type fully defined, i.e. no embedded anytypes or wildcards in it?
   *
   *  @param tp ...
   *  @return   ...
   */
  def isFullyDefined(tp: Type): boolean = try {
    instantiate(tp); true
  } catch {
    case ex: NoInstance => false
  }

  /** Do type arguments <code>targs</code> conform to formal parameters
   *  <code>tparams</code>?
   *
   *  @param tparams ...
   *  @param targs   ...
   *  @return        ...
   */
  private def isWithinBounds(pre: Type, owner: Symbol, tparams: List[Symbol], targs: List[Type]): boolean = {
    val bounds = tparams map { tparam =>
      tparam.info.asSeenFrom(pre, owner).subst(tparams, targs).bounds
    }
    !(List.map2(bounds, targs)((bound, targ) => bound containsType targ) contains false)
  }

  /** Solve constraint collected in types <code>tvars</code>.
   *
   *  @param tvars      All type variables to be instantiated.
   *  @param tparams    The type parameters corresponding to <code>tvars</code>
   *  @param variances  The variances of type parameters; need to reverse
   *                    solution direction for all contravariant variables.
   *  @param upper      When <code>true</code> search for max solution else min.
   *  @throws NoInstance
   */
  private def solve(tvars: List[TypeVar], tparams: List[Symbol],
                    variances: List[int], upper: boolean): List[Type] = {
    val config = tvars zip (tparams zip variances)

    def solveOne(tvar: TypeVar, tparam: Symbol, variance: int): unit = {
      if (tvar.constr.inst == NoType) {
        val up = if (variance != CONTRAVARIANT) upper else !upper
        tvar.constr.inst = null
        val bound: Type = if (up) tparam.info.bounds.hi else tparam.info.bounds.lo
        // Console.println("solveOne0 "+tvar+" "+config+" "+bound);//DEBUG
        var cyclic = bound contains tparam
        for (val (tvar2, (tparam2, variance2)) <- config) {
          if (tparam2 != tparam &&
              ((bound contains tparam2) ||
               up && (tparam2.info.bounds.lo =:= tparam.tpe) ||
               !up && (tparam2.info.bounds.hi =:= tparam.tpe))) {
            if (tvar2.constr.inst eq null) cyclic = true
            solveOne(tvar2, tparam2, variance2)
          }
        }
        if (!cyclic) {
          if (up) {
            if (bound.symbol != AnyClass) {
              tvar.constr.hibounds =
                bound.subst(tparams, tvars) :: tvar.constr.hibounds
            }
            for (val tparam2 <- tparams)
              if (tparam2.info.bounds.lo =:= tparam.tpe)
                tvar.constr.hibounds =
                  tparam2.tpe.subst(tparams, tvars) :: tvar.constr.hibounds
          } else {
            if (bound.symbol != AllClass && bound.symbol != tparam) {
              tvar.constr.lobounds =
                bound.subst(tparams, tvars) :: tvar.constr.lobounds
            }
            for (val tparam2 <- tparams)
              if (tparam2.info.bounds.hi =:= tparam.tpe)
                tvar.constr.lobounds =
                  tparam2.tpe.subst(tparams, tvars) :: tvar.constr.lobounds
          }
        }
        tvar.constr.inst = NoType // necessary because hibounds/lobounds may contain tvar
        //Console.println("solving "+tvar+" "+up+" "+(if (up) (tvar.constr.hibounds) else tvar.constr.lobounds))//DEBUG
        tvar.constr.inst = if (up) glb(tvar.constr.hibounds) else lub(tvar.constr.lobounds)
        assertNonCyclic(tvar)//debug
      }
    }
    for (val (tvar, (tparam, variance)) <- config)
      solveOne(tvar, tparam, variance)
    tvars map instantiate
  }

  def skipImplicit(tp: Type) =
    if (tp.isInstanceOf[ImplicitMethodType]) tp.resultType else tp

  /** Automatically perform the following conversions on expression types:
   *  A method type becomes the corresponding function type.
   *  A nullary method type becomes its result type.
   *  Implicit parameters are skipped.
   *
   *  @param tp ...
   *  @return   ...
   */
  def normalize(tp: Type): Type = skipImplicit(tp) match {
    case MethodType(formals, restpe) =>
      if (util.Statistics.enabled) normM = normM + 1
      functionType(formals, normalize(restpe))
    case PolyType(List(), restpe) =>
      if (util.Statistics.enabled) normP = normP + 1
      normalize(restpe)
    case tp1 =>
      if (util.Statistics.enabled) normO = normO + 1
      tp1
  }

  private val stdErrorClass = RootClass.newErrorClass(nme.ERROR.toTypeName)
  private val stdErrorValue = stdErrorClass.newErrorValue(nme.ERROR)

  /** The context-dependent inferencer part */
  class Inferencer(context: Context) {

    /* -- Error Messages --------------------------------------------------- */

    def setError[T <: Tree](tree: T): T = {
      if (tree.hasSymbol)
        if (context.reportGeneralErrors) {
          val name = newTermName("<error: " + tree.symbol + ">")
          tree.setSymbol(
            if (tree.isType) context.owner.newErrorClass(name.toTypeName)
            else context.owner.newErrorValue(name))
        } else {
          tree.setSymbol(if (tree.isType) stdErrorClass else stdErrorValue)
        }
      tree.setType(ErrorType)
    }

    def decode(name: Name): String =
      (if (name.isTypeName) "type " else "value ") + name.decode

    def treeSymTypeMsg(tree: Tree): String =
      if (tree.symbol eq null)
        "expression of type " + tree.tpe
      else if (tree.symbol.hasFlag(OVERLOADED))
        "overloaded method " + tree.symbol + " with alternatives " + tree.tpe
      else
        tree.symbol.toString() +
        (if (tree.tpe.paramSectionCount > 0) ": " else " of type ") +
        tree.tpe +
        (if (tree.symbol.name == nme.apply) tree.symbol.locationString else "")

    def applyErrorMsg(tree: Tree, msg: String, argtpes: List[Type], pt: Type) = (
      treeSymTypeMsg(tree) + msg + argtpes.mkString("(", ",", ")") +
       (if (pt == WildcardType) "" else " with expected result type " + pt)
    )

    def foundReqMsg(found: Type, req: Type): String =
      withDisambiguation(found, req) {
        ";\n found   : " + found.toLongString + "\n required: " + req
      }

    def typeErrorMsg(found: Type, req: Type) =
      "type mismatch" + foundReqMsg(found, req) +
      (if (!(found.resultType eq found) && isWeaklyCompatible(found.resultType, req))
        "\n possible cause: missing arguments for method or constructor"
       else "")

    def error(pos: PositionType, msg: String): unit =
      context.error(pos, msg)

    def errorTree(tree: Tree, msg: String): Tree = {
      if (!tree.isErroneous) error(tree.pos, msg)
      setError(tree)
    }

    def typeError(pos: PositionType, found: Type, req: Type) {
      if (!found.isErroneous && !req.isErroneous) {
        error(pos, typeErrorMsg(found, req))
        if (settings.explaintypes.value) explainTypes(found, req)
      }
    }

    def typeErrorTree(tree: Tree, found: Type, req: Type): Tree = {
      typeError(tree.pos, found, req)
      setError(tree)
    }

    def explainTypes(tp1: Type, tp2: Type) =
      withDisambiguation(tp1, tp2) { global.explainTypes(tp1, tp2) }

    /** If types `tp1' `tp2' contain different type variables with same name
     *  differentiate the names by including owner information
     */
    private def withDisambiguation[T](tp1: Type, tp2: Type)(op: => T): T = {

      def explainName(sym: Symbol) = {
        if (!sym.name.toString.endsWith(")")) {
          sym.name = newTypeName(sym.name.toString+"(in "+sym.owner+")")
        }
      }

      val patches = {
        val syms1 = typeRefs.collect(tp1)
        val syms2 = typeRefs.collect(tp2)
        for {
          val sym1 <- syms1
          val sym2 <- syms2
          sym1 != sym2 && sym1.toString == sym2.toString
        } yield {
          val name = sym1.name
          explainName(sym1)
          explainName(sym2)
          if (sym1.owner == sym2.owner) sym2.name = newTypeName("(some other)"+sym2.name)
          (sym1, sym2, name)
        }
      }

      val result = op

      for (val (sym1, sym2, name) <- patches) {
        sym1.name = name
        sym2.name = name
      }

      result
    }

    /* -- Tests & Checks---------------------------------------------------- */

    /** Check that <code>sym</code> is defined and accessible as a member of
     *  tree <code>site</code> with type <code>pre</code> in current context.
     *
     *  @param tree ...
     *  @param sym  ...
     *  @param pre  ...
     *  @param site ...
     *  @return     ...
     */
    def checkAccessible(tree: Tree, sym: Symbol, pre: Type, site: Tree): Tree =
      if (sym.isError) {
        tree setSymbol sym setType ErrorType
      } else {
        def accessError(explanation: String): Tree =
          errorTree(tree, underlying(sym).toString() + " cannot be accessed in " +
                    (if (sym.isClassConstructor) context.enclClass.owner else pre.widen) +
                    explanation)
        if (context.unit != null) sym.toplevelClass match {
          case clazz : ClassSymbol =>
            // System.err.println("TOP: " + clazz + " " + clazz.sourceFile)
            if (clazz.sourceFile != null) {
                context.unit.depends += clazz.sourceFile
              //Console.println("DEPEND " + global.currentRun.currentUnit + " ON " + clazz.sourceFile + " XXX " + context.unit)
            }

          case _ =>
        }
        val sym1 = sym filter (alt => context.isAccessible(alt, pre, site.isInstanceOf[Super]))
        if (sym1 == NoSymbol) {
          if (settings.debug.value) {
            Console.println(context)
            Console.println(tree)
            Console.println("" + pre + " " + sym.owner + " " + context.owner + " " + context.outer.enclClass.owner + " " + sym.owner.thisType + (pre =:= sym.owner.thisType))
          }
          accessError("")
        } else {
          //Console.println("check acc " + sym1 + ":" + sym1.tpe + " from " + pre);//DEBUG
          var owntype = try{
            pre.memberType(sym1)
          } catch {
            case ex: MalformedType =>
              val sym2 = underlying(sym1)
              val itype = withoutMalformedChecks(pre.memberType(sym2))
              accessError("\n because its instance type "+itype+
                          (if ("malformed type: "+itype.toString==ex.msg) " is malformed"
                           else " contains a "+ex.msg))
              ErrorType
          }
          if (pre.isInstanceOf[SuperType])
            owntype = owntype.substSuper(pre, site.symbol.thisType)
          tree setSymbol sym1 setType owntype
        }
      }

    def isCompatible(tp: Type, pt: Type): boolean = {
      val tp1 = normalize(tp)
      (tp1 <:< pt) || isCoercible(tp, pt)
    }

    def isWeaklyCompatible(tp: Type, pt: Type): boolean =
      pt.symbol == UnitClass || isCompatible(tp, pt)

    def isCoercible(tp: Type, pt: Type): boolean = false

    def isCompatible(tps: List[Type], pts: List[Type]): boolean =
      List.map2(tps, pts)((tp, pt) => isCompatible(tp, pt)) forall (x => x)

    /* -- Type instantiation------------------------------------------------ */

    /** Return inferred type arguments of polymorphic expression, given
     *  its type parameters and result type and a prototype <code>pt</code>.
     *  If no minimal type variables exist that make the
     *  instantiated type a subtype of <code>pt</code>, return null.
     *
     *  @param tparams ...
     *  @param restpe  ...
     *  @param pt      ...
     *  @return        ...
     */
    private def exprTypeArgs(tparams: List[Symbol], restpe: Type, pt: Type): List[Type] = {
      val tvars = tparams map freshVar
      if (isCompatible(restpe.subst(tparams, tvars), pt)) {
        try {
          solve(tvars, tparams, tparams map varianceInType(restpe), false)
        } catch {
          case ex: NoInstance => null
        }
      } else null
    }

    /** Return inferred proto-type arguments of function, given
    *  its type and value parameters and result type, and a
    *  prototype <code>pt</code> for the function result.
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
      /** Map type variable to its instance, or, if `variance' is covariant/contravariant,
       *  to its upper/lower bound */
      def instantiateToBound(tvar: TypeVar, variance: int): Type = try {
        //Console.println("instantiate "+tvar+tvar.constr+" variance = "+variance);//DEBUG
        if (tvar.constr.inst != NoType) {
          instantiate(tvar.constr.inst)
        } else if ((variance & COVARIANT) != 0 && !tvar.constr.hibounds.isEmpty) {
          tvar.constr.inst = glb(tvar.constr.hibounds)
          assertNonCyclic(tvar)//debug
          instantiate(tvar.constr.inst)
        } else if ((variance & CONTRAVARIANT) != 0 && !tvar.constr.lobounds.isEmpty) {
          tvar.constr.inst = lub(tvar.constr.lobounds)
          assertNonCyclic(tvar)//debug
          instantiate(tvar.constr.inst)
        } else if (!tvar.constr.hibounds.isEmpty && !tvar.constr.lobounds.isEmpty &&
                   glb(tvar.constr.hibounds) <:< lub(tvar.constr.lobounds)) {
          tvar.constr.inst = glb(tvar.constr.hibounds)
          assertNonCyclic(tvar)//debug
          instantiate(tvar.constr.inst)
        } else {
          WildcardType
        }
      } catch {
        case ex: NoInstance => WildcardType
      }
      val tvars = tparams map freshVar
      if (isCompatible(restpe.subst(tparams, tvars), pt))
        List.map2(tparams, tvars) ((tparam, tvar) =>
          instantiateToBound(tvar, varianceInTypes(formals)(tparam)))
      else
        tvars map (tvar => WildcardType)
    }

    /** Return inferred type arguments, given type parameters, formal parameters,
    *  argument types, result type and expected result type.
    *  If this is not possible, throw a <code>NoInstance</code> exception.
    *  Undetermined type arguments are represented by `definitions.AllClass.tpe'.
    *  No check that inferred parameters conform to their bounds is made here.
    *
    *  @param   tparams         the type parameters of the method
    *  @param   formals         the value parameter types of the method
    *  @param   restp           the result type of the method
    *  @param   argtpes         the argument types of the application
    *  @param   pt              the expected return type of the application
    *  @param   uninstantiated  a listbuffer receiving all uninstantiated type parameters
    *                           (type parameters mapped by the constraint solver to `scala.All'
    *                           and not covariant in <code>restpe</code> are taken to be
    *                           uninstantiated. Maps all those type arguments to their
    *                           corresponding type parameters).
    *  @return                  ...
    *  @throws                  NoInstance
    */
    // bq: was private, but need it for unapply checking
    def methTypeArgs(tparams: List[Symbol], formals: List[Type], restpe: Type,
                             argtpes: List[Type], pt: Type,
                             uninstantiated: ListBuffer[Symbol]): List[Type] = {
      val tvars = tparams map freshVar
      if (formals.length != argtpes.length) {
        throw new NoInstance("parameter lists differ in length")
      }
      // check first whether type variables can be fully defined from
      // expected result type.
      if (!isCompatible(restpe.subst(tparams, tvars), pt)) {
        throw new DeferredNoInstance(() =>
          "result type " + normalize(restpe) + " is incompatible with expected type " + pt)
      }
      for (val tvar <- tvars)
        if (!isFullyDefined(tvar)) tvar.constr.inst = NoType

      // Then define remaining type variables from argument types.
      List.map2(argtpes, formals) {(argtpe, formal) =>
        if (!isCompatible(argtpe.deconst.subst(tparams, tvars),
                          formal.subst(tparams, tvars))) {
          if (settings.explaintypes.value)
            explainTypes(argtpe.deconst.subst(tparams, tvars), formal.subst(tparams, tvars))
          throw new DeferredNoInstance(() =>
            "argument expression's type is not compatible with formal parameter type" +
            foundReqMsg(argtpe.deconst.subst(tparams, tvars), formal.subst(tparams, tvars)))
        }
        ()
      }
      val targs = solve(tvars, tparams, tparams map varianceInTypes(formals), false)
      List.map2(tparams, targs) {(tparam, targ) =>
        if (targ.symbol == AllClass && (varianceInType(restpe)(tparam) & COVARIANT) == 0) {
          uninstantiated += tparam
          tparam.tpe
        } else targ}
    }


    /** Is there an instantiation of free type variables <code>undetparams</code>
     *  such that function type <code>ftpe</code> is applicable to
     *  <code>argtpes</code> and its result conform to <code>pt</code>?
     *
     *  @param undetparams ...
     *  @param ftpe        ...
     *  @param argtpes     ...
     *  @param pt          ...
     *  @return            ...
     */
    def isApplicable(undetparams: List[Symbol], ftpe: Type,
                     argtpes0: List[Type], pt: Type): boolean =
      ftpe match {
        case MethodType(formals0, restpe) =>
          val formals = formalTypes(formals0, argtpes0.length)
          val argtpes = actualTypes(argtpes0, formals.length)
          if (undetparams.isEmpty) {
            (formals.length == argtpes.length &&
             isCompatible(argtpes, formals) &&
             isWeaklyCompatible(restpe, pt))
          } else {
            try {
              val uninstantiated = new ListBuffer[Symbol]
              val targs = methTypeArgs(undetparams, formals, restpe, argtpes, pt, uninstantiated)
              (exprTypeArgs(uninstantiated.toList, restpe.subst(undetparams, targs), pt) ne null) &&
              isWithinBounds(NoPrefix, NoSymbol, undetparams, targs)
            } catch {
              case ex: NoInstance => false
            }
          }
        case PolyType(tparams, restpe) =>
          val tparams1 = cloneSymbols(tparams)
          isApplicable(tparams1 ::: undetparams, restpe.substSym(tparams, tparams1), argtpes0, pt)
        case ErrorType =>
          true
        case _ =>
          false
      }

    def isApplicableSafe(undetparams: List[Symbol], ftpe: Type, argtpes0: List[Type], pt: Type): boolean = {
      val reportAmbiguousErrors = context.reportAmbiguousErrors
      context.reportAmbiguousErrors = false
      try {
        isApplicable(undetparams, ftpe, argtpes0, pt)
      } catch {
        case ex: TypeError =>
          false
      } finally {
        context.reportAmbiguousErrors = reportAmbiguousErrors
      }
    }

    /** Does type <code>ftpe1</code> specialize type <code>ftpe2</code>
     *  when both are alternatives in an overloaded function?
     *
     *  @param ftpe1 ...
     *  @param ftpe2 ...
     *  @return      ...
     */
    def specializes(ftpe1: Type, ftpe2: Type): boolean = ftpe1 match {
      case MethodType(formals, _) =>
        isApplicable(List(), ftpe2, formals, WildcardType)
      case PolyType(tparams, MethodType(formals, _)) =>
        isApplicable(List(), ftpe2, formals, WildcardType)
      case ErrorType =>
        true
      case _ =>
        false
    }

    /** Is type `tpe1' a strictly better alternative than type `ftpe2'?
     *
     *  @param tpe1 ...
     *  @param tpe2 ...
     *  @return     ...
     */
    def isStrictlyBetter(tpe1: Type, tpe2: Type) = {
      def isNullary(tpe: Type) = tpe.paramSectionCount == 0 || tpe.paramTypes.isEmpty
      isNullary(tpe1) && !isNullary(tpe2) ||
      specializes(tpe1, tpe2) && !specializes(tpe2, tpe1)
    }

    /** error if arguments not within bounds. */
    def checkBounds(pos: PositionType, pre: Type, owner: Symbol,
                    tparams: List[Symbol], targs: List[Type], prefix: String) {
      if (!isWithinBounds(pre, owner, tparams, targs)) {
        if (!(targs exists (.isErroneous)) && !(tparams exists (.isErroneous))) {
          error(pos,
                prefix + "type arguments " + targs.mkString("[", ",", "]") +
                " do not conform to " + tparams.head.owner + "'s type parameter bounds " +
                (tparams map (.defString)).mkString("[", ",", "]"))
        }
        if (settings.explaintypes.value) {
          val bounds = tparams map (.info.subst(tparams, targs).bounds)
          List.map2(targs, bounds)((targ, bound) => explainTypes(bound.lo, targ))
          List.map2(targs, bounds)((targ, bound) => explainTypes(targ, bound.hi))
          ()
        }
      }
    }

    /** Substitite free type variables `undetparams' of polymorphic argument
     *  expression `tree', given two prototypes `strictPt', and `lenientPt'.
     *  `strictPt' is the first attempt prototype where type parameters
     *  are left unchanged. `lenientPt' is the fall-back prototype where type
     *  parameters are replaced by `WildcardType's. We try to instantiate
     *  first to `strictPt' and then, if this fails, to `lenientPt'. If both
     *  attempts fail, an error is produced.
     */
    def inferArgumentInstance(tree: Tree, undetparams: List[Symbol],
                              strictPt: Type, lenientPt: Type): unit = {
      var targs = exprTypeArgs(undetparams, tree.tpe, strictPt)
      if (targs eq null) targs = exprTypeArgs(undetparams, tree.tpe, lenientPt)
      substExpr(tree, undetparams, targs, lenientPt)
    }

    /** Substitite free type variables `undetparams; of polymorphic expression
     *  <code>tree</code>, given prototype <code>pt</code>.
     *
     *  @param tree ...
     *  @param undetparams ...
     *  @param pt ...
     */
    def inferExprInstance(tree: Tree, undetparams: List[Symbol], pt: Type): unit =
      substExpr(tree, undetparams, exprTypeArgs(undetparams, tree.tpe, pt), pt)

    /** Substitite free type variables `undetparams' of polymorphic argument
     *  expression <code>tree</code> to `targs', Error if `targs' is null
     *
     *  @param tree ...
     *  @param undetparams ...
     *  @param targs ...
     *  @param pt ...
     */
    private def substExpr(tree: Tree, undetparams: List[Symbol],
                          targs: List[Type], pt: Type) {
      if (targs eq null) {
        if (!tree.tpe.isErroneous && !pt.isErroneous)
          error(tree.pos, "polymorphic expression cannot be instantiated to expected type" +
                foundReqMsg(PolyType(undetparams, skipImplicit(tree.tpe)), pt))
      } else {
        new TreeTypeSubstituter(undetparams, targs).traverse(tree)
      }
    }

    /** Substitite free type variables <code>undetparams</code> of application
     *  <code>fn(args)</code>, given prototype <code>pt</code>.
     *
     *  @param fn          ...
     *  @param undetparams ...
     *  @param args        ...
     *  @param pt          ...
     *  @return            Return the list of type parameters that remain uninstantiated.
     */
    def inferMethodInstance(fn: Tree, undetparams: List[Symbol],
                            args: List[Tree], pt: Type): List[Symbol] = fn.tpe match {
      case MethodType(formals0, restpe) =>
        try {
          val formals = formalTypes(formals0, args.length)
          val argtpes = actualTypes(args map (.tpe.deconst), formals.length)
          val uninstantiated = new ListBuffer[Symbol]
          val targs = methTypeArgs(undetparams, formals, restpe, argtpes, pt, uninstantiated)
          checkBounds(fn.pos, NoPrefix, NoSymbol, undetparams, targs, "inferred ")
          //Console.println("UNAPPLY subst type "+undetparams+" to "+targs+" in "+fn+" ( "+args+ ")")
          val treeSubst = new TreeTypeSubstituter(undetparams, targs)
          treeSubst.traverse(fn)
          treeSubst.traverseTrees(args)
          //Console.println("UNAPPLY gives "+fn+" ( "+args+ "), argtpes = "+argtpes+", pt = "+pt)
          uninstantiated.toList
        } catch {
          case ex: NoInstance =>
            errorTree(fn,
              "no type parameters for " +
              applyErrorMsg(
                fn, " exist so that it can be applied to arguments ",
                args map (.tpe.widen), WildcardType) +
              "\n --- because ---\n" + ex.getMessage())
            List()
        }
    }

    /** Is intersection of given types populated? That is,
     *  for all types tp1, tp2 in intersection
     *    for all common base classes bc of tp1 and tp2
     *      let bt1, bt2 be the base types of tp1, tp2 relative to class bc
     *      Then:
     *        bt1 and bt2 have the same prefix, and
     *        any correspondiong non-variant type arguments of bt1 and bt2 are the same
     */
    def isPopulated(tp1: Type, tp2: Type): boolean = {
      def isConsistent(tp1: Type, tp2: Type): boolean = (tp1, tp2) match {
        case (TypeRef(pre1, sym1, args1), TypeRef(pre2, sym2, args2)) =>
          assert(sym1 == sym2)
          pre1 =:= pre2 &&
          !(List.map3(args1, args2, sym1.typeParams) {
            (arg1, arg2, tparam) =>
              //if (tparam.variance == 0 && !(arg1 =:= arg2)) Console.println("inconsistent: "+arg1+"!="+arg2)//DEBUG
            tparam.variance != 0 || arg1 =:= arg2
          } contains false)
      }
      if (tp1.symbol.isClass && tp1.symbol.hasFlag(FINAL)) tp1 <:< tp2
      else tp1.baseClasses forall (bc =>
        tp2.closurePos(bc) < 0 || isConsistent(tp1.baseType(bc), tp2.baseType(bc)))
    }

    /** Type with all top-level occurrences of abstract types replaced by their bounds */
    def widen(tp: Type): Type = tp match {
      case TypeRef(pre, sym, _) if sym.isAbstractType =>
        widen(tp.bounds.hi)
      case rtp @ RefinedType(parents, decls) =>
        copyRefinedType(rtp, List.mapConserve(parents)(widen), decls)
      case _ =>
        tp
    }

    /** Substitite free type variables <code>undetparams</code> of type constructor
     *  <code>tree</code> in pattern, given prototype <code>pt</code>.
     *
     *  @param tree        ...
     *  @param undetparams ...
     *  @param pt          ...
     */
    def inferConstructorInstance(tree: Tree, undetparams: List[Symbol], pt: Type): unit = {
      var restpe = tree.tpe.finalResultType
      var tvars = undetparams map freshVar

      /** Compute type arguments for undetermined params and substitute them in given tree.
       */
      def computeArgs =
        try {
          val targs = solve(tvars, undetparams, undetparams map varianceInType(restpe), true)
          checkBounds(tree.pos, NoPrefix, NoSymbol, undetparams, targs, "inferred ")
          new TreeTypeSubstituter(undetparams, targs).traverse(tree)
        } catch {
          case ex: NoInstance =>
            errorTree(tree, "constructor of type " + restpe +
                      " can be instantiated in more than one way to expected type " + pt +
                      "\n --- because ---\n" + ex.getMessage())
        }
      def instError = {
        if (settings.debug.value) Console.println("ici " + tree + " " + undetparams + " " + pt)
        if (settings.explaintypes.value) explainTypes(restpe.subst(undetparams, tvars), pt)
        errorTree(tree, "constructor cannot be instantiated to expected type" +
                  foundReqMsg(restpe, pt))
      }
      if (restpe.subst(undetparams, tvars) <:< pt) {
        computeArgs
      } else if (isFullyDefined(pt)) {
        if (settings.debug.value) log("infer constr " + tree + ":" + restpe + ", pt = " + pt)
        var ptparams = freeTypeParamsOfTerms.collect(pt)
        if (settings.debug.value) log("free type params = " + ptparams)
        val ptWithWildcards = pt.subst(ptparams, ptparams map (ptparam => WildcardType))
        tvars = undetparams map freshVar
        if (restpe.subst(undetparams, tvars) <:< ptWithWildcards) {
          computeArgs
          restpe = skipImplicit(tree.tpe.resultType)
          if (settings.debug.value) log("new tree = " + tree + ":" + restpe)
          val ptvars = ptparams map freshVar
          val pt1 = pt.subst(ptparams, ptvars)
          if (isPopulated(restpe, pt1)) {
            ptvars foreach instantiateTypeVar
          } else { if (settings.debug.value) Console.println("no instance: "); instError }
        } else { if (settings.debug.value) Console.println("not a subtype " + restpe.subst(undetparams, tvars) + " of " + ptWithWildcards); instError }
      } else { if (settings.debug.value) Console.println("not fuly defined: " + pt); instError }
    }

    def instantiateTypeVar(tvar: TypeVar) = {
      val tparam = tvar.origin.symbol
      if (false &&
          tvar.constr.inst != NoType &&
          isFullyDefined(tvar.constr.inst) &&
          (tparam.info.bounds containsType tvar.constr.inst)) {
        context.nextEnclosing(.tree.isInstanceOf[CaseDef]).pushTypeBounds(tparam)
        tparam setInfo tvar.constr.inst
        tparam resetFlag DEFERRED
        if (settings.debug.value) log("new alias of " + tparam + " = " + tparam.info)
      } else {
        val instType = toOrigin(tvar.constr.inst)
        val (loBounds, hiBounds) =
          if (instType != NoType && isFullyDefined(instType)) (List(instType), List(instType))
          else (tvar.constr.lobounds, tvar.constr.hibounds)
        val lo = lub(tparam.info.bounds.lo :: loBounds map toOrigin)
        val hi = glb(tparam.info.bounds.hi :: hiBounds map toOrigin)
        if (!(lo <:< hi)) {
          if (settings.debug.value) log("inconsistent: "+tparam+" "+lo+" "+hi)
        } else if (!((lo <:< tparam.info.bounds.lo) && (tparam.info.bounds.hi <:< hi))) {
          context.nextEnclosing(.tree.isInstanceOf[CaseDef]).pushTypeBounds(tparam)
          tparam setInfo mkTypeBounds(lo, hi)
          if (settings.debug.value) log("new bounds of " + tparam + " = " + tparam.info)
        } else {
          if (settings.debug.value) log("redundant: "+tparam+" "+tparam.info+"/"+lo+" "+hi)
        }
      }
    }

    def checkCheckable(pos: PositionType, tp: Type): unit = {
      def patternWarning(tp: Type, prefix: String) =
        context.unit.uncheckedWarning(pos, prefix+tp+" in type pattern is unchecked since it is eliminated by erasure")
      def isLocalBinding(sym: Symbol) =
        sym.isAbstractType &&
        (sym.name == nme.WILDCARD.toTypeName || {
          val e = context.scope.lookupEntry(sym.name)
          (e ne null) && e.sym == sym && e.owner == context.scope
        })
      tp match {
        case SingleType(pre, _) =>
          checkCheckable(pos, pre)
        case TypeRef(pre, sym, args) =>
          if (sym.isAbstractType)
            patternWarning(tp, "abstract type ")
          else if (sym == AllClass || sym == AllRefClass)
            error(pos, "this type cannot be used in a type pattern")
          else
            for (val arg <- args) {
              if (sym == ArrayClass) checkCheckable(pos, arg)
              else arg match {
                case TypeRef(_, sym, _) if isLocalBinding(sym) =>
                  ;
                case _ =>
                  patternWarning(arg, "non variable type-argument ")
              }
            }
          checkCheckable(pos, pre)
        case RefinedType(parents, decls) =>
          if (decls.isEmpty) for (val p <- parents) checkCheckable(pos, p)
          else patternWarning(tp, "refinement ")
        case ThisType(_) =>
          ;
        case NoPrefix =>
          ;
        case _ =>
          patternWarning(tp, "type ")
      }
    }

    /** Type intersection of simple type `tp1' with general type `tp2'
     *  The result eliminates some redundancies
     */
    def intersect(tp1: Type, tp2: Type): Type = {
      if (tp1 <:< tp2) tp1
      else if (tp2 <:< tp1) tp2
      else {
        val reduced2 = tp2 match {
          case rtp @ RefinedType(parents2, decls2) =>
            copyRefinedType(rtp, parents2 filter (p2 => !(tp1 <:< p2)), decls2)
          case _ =>
            tp2
        }
        intersectionType(List(tp1, reduced2))
      }
    }

    def inferTypedPattern(pos: PositionType, pattp: Type, pt: Type): Type = {
      checkCheckable(pos, pattp)
      if (!(pattp <:< pt)) {
        val tpparams = freeTypeParamsOfTerms.collect(pattp)
        if (settings.debug.value) log("free type params (1) = " + tpparams)
        var tvars = tpparams map freshVar
        var tp = pattp.subst(tpparams, tvars)
        if (!(tp <:< pt)) {
          tvars = tpparams map freshVar
          tp = pattp.subst(tpparams, tvars)
          val ptparams = freeTypeParamsOfTerms.collect(pt)
          if (settings.debug.value) log("free type params (2) = " + ptparams)
          val ptvars = ptparams map freshVar
          val pt1 = pt.subst(ptparams, ptvars)
          if (!isPopulated(tp, pt1)) {
            error(pos, "pattern type is incompatibe with expected type"+foundReqMsg(pattp, pt))
            return pattp
          }
          ptvars foreach instantiateTypeVar
        }
        tvars foreach instantiateTypeVar
      }
      intersect(pt, pattp)
    }

    def inferModulePattern(pat: Tree, pt: Type) =
      if (!(pat.tpe <:< pt)) {
        val ptparams = freeTypeParamsOfTerms.collect(pt)
        if (settings.debug.value) log("free type params (2) = " + ptparams)
        val ptvars = ptparams map freshVar
        val pt1 = pt.subst(ptparams, ptvars)
        if (pat.tpe <:< pt1)
          ptvars foreach instantiateTypeVar
        else
          error(pat.pos, "pattern type is incompatibe with expected type"+foundReqMsg(pat.tpe, pt))
      }

    object toOrigin extends TypeMap {
      def apply(tp: Type): Type = tp match {
        case TypeVar(origin, _) => origin
        case _ => mapOver(tp)
      }
    }

    abstract class SymCollector extends TypeTraverser {
      private var result: List[Symbol] = _
      protected def includeCondition(sym: Symbol): boolean

      override def traverse(tp: Type): TypeTraverser = {
        tp match {
          case TypeRef(_, sym, _) =>
            if (includeCondition(sym) && !result.contains(sym)) result = sym :: result
          case _ =>
        }
        mapOver(tp)
        this
      }

      /** Collect all abstract type symbols referred to by type <code>tp</code>.
       *
       *  @param tp ...
       *  @return   ...
       */
      def collect(tp: Type): List[Symbol] = {
        result = List()
        traverse(tp)
        result
      }
    }

    object approximateAbstracts extends TypeMap {
      def apply(tp: Type): Type = tp match {
        case TypeRef(pre, sym, _) if sym.isAbstractType => WildcardType
        case _ => mapOver(tp)
      }
    }

    /** A traverser to collect type parameters referred to in a type
     */
    object freeTypeParamsOfTerms extends SymCollector {
      protected def includeCondition(sym: Symbol): boolean = sym.isAbstractType && sym.owner.isTerm
    }

    object typeRefs extends SymCollector {
      protected def includeCondition(sym: Symbol): boolean = true
    }

    def checkDead(tree: Tree): Tree = {
      if (settings.Xwarndeadcode.value && tree.tpe.symbol == AllClass)
        context.warning (tree.pos, "dead code following this construct")
      tree
    }

    /* -- Overload Resolution ---------------------------------------------- */

    def checkNotShadowed(pos: PositionType, pre: Type, best: Symbol, eligible: List[Symbol]) =
      if (!phase.erasedTypes)
        for (val alt <- eligible) {
          if (alt.owner != best.owner && alt.owner.isSubClass(best.owner))
            error(pos,
                  "erroneous reference to overloaded definition,\n"+
                  "most specific definition is: "+best+best.locationString+" of type "+pre.memberType(best)+
                  ",\nyet alternative definition   "+alt+alt.locationString+" of type "+pre.memberType(alt)+
                  "\nis defined in a subclass")
        }

    /** Assign <code>tree</code> the symbol and type of the alternative which
     *  matches prototype <code>pt</code>, if it exists.
     *  If several alternatives match `pt', take parameterless one.
     *  If no alternative matches `pt', take the parameterless one anyway.
     */
    def inferExprAlternative(tree: Tree, pt: Type): unit = tree.tpe match {
      case OverloadedType(pre, alts) => tryTwice {
        var alts1 = alts filter (alt => isCompatible(pre.memberType(alt), pt))
        if (alts1.isEmpty) alts1 = alts
        def improves(sym1: Symbol, sym2: Symbol): boolean =
          sym2 == NoSymbol ||
          { val tp1 = pre.memberType(sym1)
            val tp2 = pre.memberType(sym2)
            (tp2 == ErrorType ||
             !global.typer.infer.isCompatible(tp2, pt) && global.typer.infer.isCompatible(tp1, pt) ||
             isStrictlyBetter(tp1, tp2)) }
        val best = ((NoSymbol: Symbol) /: alts1) ((best, alt) =>
          if (improves(alt, best)) alt else best)
        val competing = alts1 dropWhile (alt => best == alt || improves(best, alt))
        if (best == NoSymbol) {
          if (settings.debug.value) {
            tree match {
              case Select(qual, _) =>
                Console.println("qual: " + qual + ":" + qual.tpe +
                                   " with decls " + qual.tpe.decls +
                                   " with members " + qual.tpe.members +
                                   " with members " + qual.tpe.member(newTermName("$minus")))
              case _ =>
            }
          }
          typeErrorTree(tree, tree.symbol.tpe, pt)
        } else if (!competing.isEmpty) {
          if (!pt.isErroneous)
            context.ambiguousError(tree.pos, pre, best, competing.head, "expected type " + pt)
          setError(tree)
          ()

        } else {
          val applicable = alts1 filter (alt =>
            global.typer.infer.isCompatible(pre.memberType(alt), pt))
          checkNotShadowed(tree.pos, pre, best, applicable)
          tree.setSymbol(best).setType(pre.memberType(best))
        }
      }
    }

    /** Assign <code>tree</code> the type of an alternative which is applicable
     *  to <code>argtpes</code>, and whose result type is compatible with `pt'.
     *  If several applicable alternatives exist, take the
     *  most specialized one.
     *  If no applicable alternative exists, and pt != WildcardType, try again
     *  with pt = WildcardType.
     *  Otherwise, if there is no best alternative, error.
     */
    def inferMethodAlternative(tree: Tree, undetparams: List[Symbol], argtpes: List[Type], pt: Type): unit = tree.tpe match {
      case OverloadedType(pre, alts) => tryTwice {
        if (settings.debug.value) log("infer method alt " + tree.symbol + " with alternatives " + (alts map pre.memberType) + ", argtpes = " + argtpes + ", pt = " + pt)
        val applicable = alts filter (alt => isApplicable(undetparams, pre.memberType(alt), argtpes, pt))
        def improves(sym1: Symbol, sym2: Symbol) = (
          sym2 == NoSymbol || sym2.isError ||
          specializes(pre.memberType(sym1), pre.memberType(sym2))
        )
        val best = ((NoSymbol: Symbol) /: applicable) ((best, alt) =>
          if (improves(alt, best)) alt else best)
        val competing = applicable dropWhile (alt => best == alt || improves(best, alt))
        if (best == NoSymbol) {
          if (pt == WildcardType) {
            errorTree(tree, applyErrorMsg(tree, " cannot be applied to ", argtpes, pt))
          } else {
            inferMethodAlternative(tree, undetparams, argtpes, WildcardType)
          }
        } else if (!competing.isEmpty) {
          if (!(argtpes exists (.isErroneous)) && !pt.isErroneous)
            context.ambiguousError(tree.pos, pre, best, competing.head,
                                   "argument types " + argtpes.mkString("(", ",", ")") +
                                   (if (pt == WildcardType) "" else " and expected result type " + pt))
          setError(tree)
          ()
        } else {
          checkNotShadowed(tree.pos, pre, best, applicable)
          tree.setSymbol(best).setType(pre.memberType(best))
        }
      }
    }

    /** Try inference twice, once without views and once with views,
     *  unless views are already disabled.
     *
     *  @param infer ...
     */
    def tryTwice(infer: => unit): unit = {
      if (context.implicitsEnabled) {
        val reportGeneralErrors = context.reportGeneralErrors
        context.reportGeneralErrors = false
        context.implicitsEnabled = false
        try {
          infer
        } catch {
          case ex: TypeError =>
            context.reportGeneralErrors = reportGeneralErrors
            context.implicitsEnabled = true
            infer
        }
        context.reportGeneralErrors = reportGeneralErrors
        context.implicitsEnabled = true
      } else infer
    }

    /** Assign <code>tree</code> the type of unique polymorphic alternative
     *  with <code>nparams</code> as the number of type parameters, if it exists.
     *  If several or none such polymorphic alternatives exist, error.
     *
     *  @param tree ...
     *  @param nparams ...
     */
    def inferPolyAlternatives(tree: Tree, argtypes: List[Type]): unit = tree.tpe match {
      case OverloadedType(pre, alts) =>
        val sym0 = tree.symbol filter { alt => alt.typeParams.length == argtypes.length }
        if (sym0 == NoSymbol) {
          error(
            tree.pos,
            if (alts exists (alt => alt.typeParams.length > 0))
              "wrong number of type parameters for " + treeSymTypeMsg(tree)
            else treeSymTypeMsg(tree) + " does not take type parameters")
          return
        }
        if (sym0.hasFlag(OVERLOADED)) {
          val sym = sym0 filter { alt => isWithinBounds(pre, alt.owner, alt.typeParams, argtypes) }
          if (sym == NoSymbol) {
            if (!(argtypes exists (.isErroneous))) {
              error(
                tree.pos,
                "type arguments " + argtypes.mkString("[", ",", "]") +
                " conform to the bounds of none of the overloaded alternatives of\n "+sym0+
                ": "+sym0.info)
              return
            }
          }
          if (sym.hasFlag(OVERLOADED)) {
            val tparams = new AsSeenFromMap(pre, sym.alternatives.head.owner).mapOver(
              sym.alternatives.head.typeParams)
            val bounds = tparams map (.tpe)
            val tpe =
              PolyType(tparams,
                       OverloadedType(AntiPolyType(pre, bounds), sym.alternatives))
            sym.setInfo(tpe)
            tree.setSymbol(sym).setType(tpe)
          } else {
            tree.setSymbol(sym).setType(pre.memberType(sym))
          }
        } else {
          tree.setSymbol(sym0).setType(pre.memberType(sym0))
        }
    }
  }
}
