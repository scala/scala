/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.typechecker
import scala.tools.nsc.util.{Position, NoPosition}
import scala.collection.mutable.ListBuffer
import symtab.Flags._

/** This trait ...
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
trait Infer {
  self: Analyzer =>
  import global._
  import definitions._

  // statistics
  var normM = 0
  var normP = 0
  var normO = 0

  private final val inferInfo = false

/* -- Type parameter inference utility functions --------------------------- */

  private def assertNonCyclic(tvar: TypeVar) =
    assert(tvar.constr.inst != tvar, tvar.origin)

  def isVarArgs(formals: List[Type]) =
    !formals.isEmpty && (formals.last.typeSymbol == RepeatedParamClass)

  /** The formal parameter types corresponding to <code>formals</code>.
   *  If <code>formals</code> has a repeated last parameter, a list of
   *  (nargs - params.length + 1) copies of its type is returned.
   *  By-name types are replaced with their underlying type.
   *
   *  @param formals ...
   *  @param nargs ...
   */
  def formalTypes(formals: List[Type], nargs: Int): List[Type] =
    formalTypes(formals, nargs, true)

  /** This variant allows keeping ByName parameters. Useed in NamesDefaults. */
  def formalTypes(formals: List[Type], nargs: Int, removeByName: Boolean): List[Type] = {
    val formals1 = if (removeByName) formals map {
      case TypeRef(_, sym, List(arg)) if (sym == ByNameParamClass) => arg
      case formal => formal
    } else formals
    if (isVarArgs(formals1)) {
      val ft = formals1.last.normalize.typeArgs.head
      formals1.init ::: (for (i <- List.range(formals1.length - 1, nargs)) yield ft)
    } else formals1
  }

  def actualTypes(actuals: List[Type], nformals: Int): List[Type] =
    if (nformals == 1 && actuals.length != 1)
      List(if (actuals.length == 0) UnitClass.tpe else tupleType(actuals))
    else actuals

  def actualArgs(pos: Position, actuals: List[Tree], nformals: Int): List[Tree] =
    if (nformals == 1 && actuals.length != 1 && actuals.length <= definitions.MaxTupleArity && !phase.erasedTypes)
      List(atPos(pos)(gen.mkTuple(actuals))) else actuals

  /** A fresh type varable with given type parameter as origin.
   *
   *  @param tparam ...
   *  @return       ...
   */
  def freshVar(tparam: Symbol): TypeVar =
    new TypeVar(tparam.tpe, new TypeConstraint)  //@M TODO: might be affected by change to tpe in Symbol

  //todo: remove comments around following privates; right now they cause an IllegalAccess
  // error when built with scalac

  /*private*/ class NoInstance(msg: String) extends RuntimeException(msg)

  /*private*/ class DeferredNoInstance(getmsg: () => String) extends NoInstance("") {
    override def getMessage(): String = getmsg()
  }

  /** map every TypeVar to its constraint.inst field.
   *  throw a NoInstance exception if a NoType or WildcardType is encountered.
   *
   *  @param  tp ...
   *  @return    ...
   *  @throws    NoInstance
   */
  object instantiate extends TypeMap {
    private var excludedVars = scala.collection.immutable.Set[TypeVar]()
    def apply(tp: Type): Type = tp match {
      case WildcardType | NoType =>
        throw new NoInstance("undetermined type")
      case tv @ TypeVar(origin, constr) =>
        if (constr.inst == NoType) {
          throw new DeferredNoInstance(() =>
            "no unique instantiation of type variable " + origin + " could be found")
        } else if (excludedVars contains tv) {
          throw new NoInstance("cyclic instantiation")
        } else {
          excludedVars += tv
          val res = apply(constr.inst)
          excludedVars -= tv
          res
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
    case WildcardType | NoType =>
      false
    case NoPrefix | ThisType(_) | ConstantType(_) =>
      true
    case TypeRef(pre, sym, args) =>
      isFullyDefined(pre) && (args.isEmpty || (args forall isFullyDefined))
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

  /** Solve constraint collected in types <code>tvars</code>.
   *
   *  @param tvars      All type variables to be instantiated.
   *  @param tparams    The type parameters corresponding to <code>tvars</code>
   *  @param variances  The variances of type parameters; need to reverse
   *                    solution direction for all contravariant variables.
   *  @param upper      When <code>true</code> search for max solution else min.
   *  @throws NoInstance
   */
  def solvedTypes(tvars: List[TypeVar], tparams: List[Symbol],
                  variances: List[Int], upper: Boolean, depth: Int): List[Type] = {
//    def boundsString(tvar: TypeVar) =
//      "\n  "+
//      ((tvar.constr.lobounds map (_ + " <: " + tvar.origin.typeSymbol.name)) :::
//       (tvar.constr.hibounds map (tvar.origin.typeSymbol.name + " <: " + _)) mkString ", ")
    if (!solve(tvars, tparams, variances, upper, depth)) {
//    no panic, it's good enough to just guess a solution, we'll find out
//    later whether it works.
//      throw new DeferredNoInstance(() =>
//        "no solution exists for constraints"+(tvars map boundsString))
    }
    for (tvar <- tvars)
      if (tvar.constr.inst == tvar)
        if (tvar.origin.typeSymbol.info eq ErrorType) {
          // this can happen if during solving a cyclic type paramater
          // such as T <: T gets completed. See #360
          tvar.constr.inst = ErrorType
        } else assert(false, tvar.origin)
    tvars map instantiate
  }

  def skipImplicit(tp: Type) =
    if (tp.isInstanceOf[ImplicitMethodType]) tp.resultType else tp

  /** Automatically perform the following conversions on expression types:
   *  A method type becomes the corresponding function type.
   *  A nullary method type becomes its result type.
   *  Implicit parameters are skipped.
   */
  def normalize(tp: Type): Type = skipImplicit(tp) match {
    case MethodType(params, restpe) if (!restpe.isDependent) =>
      if (util.Statistics.enabled) normM += 1
      functionType(params map (_.tpe), normalize(restpe))
    case PolyType(List(), restpe) =>
      if (util.Statistics.enabled) normP += 1
      normalize(restpe)
    case ExistentialType(tparams, qtpe) =>
      ExistentialType(tparams, normalize(qtpe))
    case tp1 =>
      if (util.Statistics.enabled) normO += 1
      tp1 // @MAT aliases already handled by subtyping
  }

  private val stdErrorClass = RootClass.newErrorClass(nme.ERROR.toTypeName)
  private val stdErrorValue = stdErrorClass.newErrorValue(nme.ERROR)

  /** The context-dependent inferencer part */
  class Inferencer(context: Context) {

    /* -- Error Messages --------------------------------------------------- */

    private var addendumPos: Position = NoPosition
    private var addendum: () => String = _

    def setAddendum(pos: Position, msg: () => String) = {
      addendumPos = pos
      addendum = msg
    }

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
        (if (tree.symbol.isModule) ""
         else if (tree.tpe.paramSectionCount > 0) ": "+tree.tpe
         else " of type "+tree.tpe) +
        (if (tree.symbol.name == nme.apply) tree.symbol.locationString else "")

    def applyErrorMsg(tree: Tree, msg: String, argtpes: List[Type], pt: Type) =
      treeSymTypeMsg(tree) + msg + argtpes.mkString("(", ",", ")") +
       (if (pt == WildcardType) "" else " with expected result type " + pt)

    // todo: use also for other error messages
    private def existentialContext(tp: Type) = tp.existentialSkolems match {
      case List() => ""
      case skolems =>
        def disambiguate(ss: List[String]) = ss match {
          case List() => ss
          case s :: ss1 => s :: (ss1 map (s1 => if (s1 == s) "(some other)"+s1 else s1))
        }
      " where "+(disambiguate(skolems map (_.existentialToString)) mkString ", ")
    }

    def foundReqMsg(found: Type, req: Type): String =
      withDisambiguation(found, req) {
        ";\n found   : " + found.toLongString + existentialContext(found) +
         "\n required: " + req + existentialContext(req)
      }

    def typeErrorMsg(found: Type, req: Type) = {
      //println(found.baseTypeSeq)
      "type mismatch" + foundReqMsg(found, req) +
      (if ((found.resultApprox ne found) && isWeaklyCompatible(found.resultApprox, req))
        "\n possible cause: missing arguments for method or constructor"
       else "")
    }

    def error(pos: Position, msg: String) {
      context.error(pos, msg)
    }

    def errorTree(tree: Tree, msg: String): Tree = {
      if (!tree.isErroneous) error(tree.pos, msg)
      setError(tree)
    }

    def typeError(pos: Position, found: Type, req: Type) {
      if (!found.isErroneous && !req.isErroneous) {
        error(pos,
              typeErrorMsg(found, req)+
              (if (pos != NoPosition && pos == addendumPos) addendum()
               else ""))
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
     *  differentiate the names by including owner information.  Also, if the
     *  type error is because of a conflict between two identically named
     *  classes and one is in package scala, fully qualify the name so one
     *  need not deduce why "java.util.Iterator" and "Iterator" don't match.
     */
    private def withDisambiguation[T](tp1: Type, tp2: Type)(op: => T): T = {

      def explainName(sym: Symbol) = {
        if (!sym.name.toString.endsWith(")")) {
          sym.name = newTypeName(sym.name.toString+"(in "+sym.owner+")")
        }
      }

      val patches = new ListBuffer[(Symbol, Symbol, Name)]
      for {
        t1 @ TypeRef(_, sym1, _) <- tp1
        t2 @ TypeRef(_, sym2, _) <- tp2
        if sym1 != sym2
      } {
        if (t1.toString == t2.toString) { // type variable collisions
          val name = sym1.name
          explainName(sym1)
          explainName(sym2)
          if (sym1.owner == sym2.owner) sym2.name = newTypeName("(some other)"+sym2.name)
          patches += ((sym1, sym2, name))
        }
        else if (sym1.name == sym2.name) { // symbol name collisions where one is in scala._
          val name = sym1.name
          def scalaQualify(s: Symbol) =
            if (s.owner.isScalaPackageClass) s.name = newTypeName("scala." + s.name)
          List(sym1, sym2) foreach scalaQualify
          patches += ((sym1, sym2, name))
        }
      }

      val result = op

      for ((sym1, sym2, name) <- patches) {
        sym1.name = name
        sym2.name = name
      }

      result
    }

    /* -- Tests & Checks---------------------------------------------------- */

    /** Check that <code>sym</code> is defined and accessible as a member of
     *  tree <code>site</code> with type <code>pre</code> in current context.
     */
    def checkAccessible(tree: Tree, sym: Symbol, pre: Type, site: Tree): Tree =
      if (sym.isError) {
        tree setSymbol sym setType ErrorType
      } else {
        def accessError(explanation: String): Tree =
          errorTree(tree, underlying(sym).toString() + " cannot be accessed in " +
                    (if (sym.isClassConstructor) context.enclClass.owner else pre.widen) +
                    explanation)

        val topClass = context.owner.toplevelClass
        if (context.unit != null)
          context.unit.depends += sym.toplevelClass

        val sym1 = sym filter (alt => context.isAccessible(alt, pre, site.isInstanceOf[Super]))
        if (sym1 == NoSymbol) {
          if (settings.debug.value) {
            Console.println(context)
            Console.println(tree)
            Console.println("" + pre + " " + sym.owner + " " + context.owner + " " + context.outer.enclClass.owner + " " + sym.owner.thisType + (pre =:= sym.owner.thisType))
          }
          accessError("")
        } else {
          // Modify symbol's type so that raw types C
          // are converted to existentials C[T] forSome { type T }.
          // We can't do this on class loading because it would result
          // in infinite cycles.
          def cook(sym: Symbol) {
            val tpe1 = rawToExistential(sym.tpe)
            if (tpe1 ne sym.tpe) {
              if (settings.debug.value) println("cooked: "+sym+":"+sym.tpe)
              sym.setInfo(tpe1)
            }
          }
          if (sym1.isTerm) {
            if (sym1 hasFlag JAVA)
              cook(sym1)
            else if (sym1 hasFlag OVERLOADED)
              for (sym2 <- sym1.alternatives)
                if (sym2 hasFlag JAVA)
                  cook(sym2)
          }
          //Console.println("check acc " + sym1 + ":" + sym1.tpe + " from " + pre);//DEBUG
          var owntype = try{
            pre.memberType(sym1)
          } catch {
            case ex: MalformedType =>
              if (settings.debug.value) ex.printStackTrace
              val sym2 = underlying(sym1)
              val itype = pre.memberType(sym2)
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

    def isPlausiblyCompatible(tp: Type, pt: Type): Boolean = tp match {
      case PolyType(_, restpe) =>
        isPlausiblyCompatible(restpe, pt)
      case mt: ImplicitMethodType =>
        isPlausiblyCompatible(mt.resultType, pt)
      case ExistentialType(tparams, qtpe) =>
        isPlausiblyCompatible(qtpe, pt)
      case MethodType(params, _) =>
        val formals = tp.paramTypes
        pt.normalize match {
          case TypeRef(pre, sym, args) =>
            !sym.isClass || {
              val l = args.length - 1
              l == formals.length &&
              sym == FunctionClass(l) &&
              List.forall2(args, formals) (isPlausiblySubType) &&
              isPlausiblySubType(tp.resultApprox, args.last)
            }
          case _ =>
            true
        }
      case _ =>
        true
    }

    private def isPlausiblySubType(tp1: Type, tp2: Type): Boolean = tp1.normalize match {
      case TypeRef(_, sym1, _) =>
        !sym1.isClass || {
          tp2.normalize match {
            case TypeRef(_, sym2, _) => !sym2.isClass || (sym1 isSubClass sym2)
            case _ => true
          }
        }
      case _ =>
        true
    }

    def isCompatible(tp: Type, pt: Type): Boolean = {
      val tp1 = normalize(tp)
      (tp1 <:< pt) || isCoercible(tp, pt)
    }

    def isWeaklyCompatible(tp: Type, pt: Type): Boolean =
      pt.typeSymbol == UnitClass || // can perform unit coercion
      isCompatible(tp, pt) ||
      tp.isInstanceOf[MethodType] && // can perform implicit () instantiation
      tp.paramTypes.length == 0 && isCompatible(tp.resultType, pt)

    def isCoercible(tp: Type, pt: Type): Boolean = false

    def isCompatible(tps: List[Type], pts: List[Type]): Boolean =
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
      if (isCompatible(restpe.instantiateTypeParams(tparams, tvars), pt)) {
        try {
          // If the restpe is an implicit method, and the expected type is fully defined
          // optimze type varianbles wrt to the implicit formals only; ignore the result type.
          // See test pos/jesper.scala
          val varianceType = restpe match {
            case mt: ImplicitMethodType if isFullyDefined(pt) =>
              MethodType(mt.params, AnyClass.tpe)
            case _ =>
              restpe
          }
          //println("try to solve "+tvars+" "+tparams)
          solvedTypes(tvars, tparams, tparams map varianceInType(varianceType),
                      false, lubDepth(List(restpe, pt)))
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
      def instantiateToBound(tvar: TypeVar, variance: Int): Type = try {
        //Console.println("instantiate "+tvar+tvar.constr+" variance = "+variance);//DEBUG
        if (tvar.constr.inst != NoType) {
          instantiate(tvar.constr.inst)
        } else if ((variance & COVARIANT) != 0 && !tvar.constr.hibounds.isEmpty) {
          tvar setInst glb(tvar.constr.hibounds)
          assertNonCyclic(tvar)//debug
          instantiate(tvar.constr.inst)
        } else if ((variance & CONTRAVARIANT) != 0 && !tvar.constr.lobounds.isEmpty) {
          tvar setInst lub(tvar.constr.lobounds)
          assertNonCyclic(tvar)//debug
          instantiate(tvar.constr.inst)
        } else if (!tvar.constr.hibounds.isEmpty && !tvar.constr.lobounds.isEmpty &&
                   glb(tvar.constr.hibounds) <:< lub(tvar.constr.lobounds)) {
          tvar setInst glb(tvar.constr.hibounds)
          assertNonCyclic(tvar)//debug
          instantiate(tvar.constr.inst)
        } else {
          WildcardType
        }
      } catch {
        case ex: NoInstance => WildcardType
      }
      val tvars = tparams map freshVar
      if (isWeaklyCompatible(restpe.instantiateTypeParams(tparams, tvars), pt))
        List.map2(tparams, tvars) ((tparam, tvar) =>
          instantiateToBound(tvar, varianceInTypes(formals)(tparam)))
      else
        tvars map (tvar => WildcardType)
    }

    /** Retract any Nothing arguments which appear covariantly in result type,
     *  and treat them as uninstantiated parameters instead.
     *  Map T* entries to Seq[T].
     */
    def adjustTypeArgs(tparams: List[Symbol], targs: List[Type], restpe: Type, uninstantiated: ListBuffer[Symbol]): List[Type] =
      List.map2(tparams, targs) {(tparam, targ) =>
        if (targ.typeSymbol == NothingClass && (restpe == WildcardType || (varianceInType(restpe)(tparam) & COVARIANT) == 0)) {
          uninstantiated += tparam
          tparam.tpe
        } else if (targ.typeSymbol == RepeatedParamClass) {
          targ.baseType(SeqClass)
        } else {
          targ.widen
        }
      }

    /** Return inferred type arguments, given type parameters, formal parameters,
    *  argument types, result type and expected result type.
    *  If this is not possible, throw a <code>NoInstance</code> exception.
    *  Undetermined type arguments are represented by `definitions.NothingClass.tpe'.
    *  No check that inferred parameters conform to their bounds is made here.
    *
    *  bq: was private, but need it for unapply checking
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
    def methTypeArgs(tparams: List[Symbol], formals: List[Type], restpe: Type,
                             argtpes: List[Type], pt: Type,
                             uninstantiated: ListBuffer[Symbol]): List[Type] = {
      val tvars = tparams map freshVar
      if (formals.length != argtpes.length) {
        throw new NoInstance("parameter lists differ in length")
      }
      // check first whether type variables can be fully defined from
      // expected result type.
      if (!isWeaklyCompatible(restpe.instantiateTypeParams(tparams, tvars), pt)) {
//      just wait and instantiate from the arguments.
//      that way, we can try to apply an implicit conversion afterwards.
//      This case could happen if restpe is not fully defined, so that
//      search for an implicit from it to pt fails because of an ambiguity.
//      See #0347. Therefore, the following two lines are commented out.
//        throw new DeferredNoInstance(() =>
//          "result type " + normalize(restpe) + " is incompatible with expected type " + pt)
      }
      for (tvar <- tvars)
        if (!isFullyDefined(tvar)) tvar.constr.inst = NoType

      // Then define remaining type variables from argument types.
      List.map2(argtpes, formals) {(argtpe, formal) =>
        if (!isCompatible(argtpe.deconst.instantiateTypeParams(tparams, tvars),
                          formal.instantiateTypeParams(tparams, tvars))) {
          throw new DeferredNoInstance(() =>
            "argument expression's type is not compatible with formal parameter type" +
            foundReqMsg(argtpe.deconst.instantiateTypeParams(tparams, tvars), formal.instantiateTypeParams(tparams, tvars)))
        }
        ()
      }
//      println("solve "+tvars+" "+(tvars map (_.constr)))
      val targs = solvedTypes(tvars, tparams, tparams map varianceInTypes(formals),
                              false, lubDepth(formals) max lubDepth(argtpes))
//      val res =
      adjustTypeArgs(tparams, targs, restpe, uninstantiated)
//      println("meth type args "+", tparams = "+tparams+", formals = "+formals+", restpe = "+restpe+", argtpes = "+argtpes+", underlying = "+(argtpes map (_.widen))+", pt = "+pt+", uninstantiated = "+uninstantiated.toList+", result = "+res) //DEBUG
//      res
    }

    private[typechecker] def followApply(tp: Type): Type = tp match {
      case PolyType(List(), restp) =>
        val restp1 = followApply(restp)
        if (restp1 eq restp) tp else restp1
      case _ =>
        val appmeth = tp.nonPrivateMember(nme.apply) filter (_.isPublic)
        if (appmeth == NoSymbol) tp
        else OverloadedType(tp, appmeth.alternatives)
    }

    def hasExactlyNumParams(tp: Type, n: Int): Boolean = tp match {
      case OverloadedType(pre, alts) =>
        alts exists (alt => hasExactlyNumParams(pre.memberType(alt), n))
      case _ =>
        formalTypes(tp.paramTypes, n).length == n
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
      val argPos = (new Array[Int](argtpes.length)) map (x => -1)
      var positionalAllowed = true
      var namesOK = true
      var index = 0
      val argtpes1 = argtpes map {
        case NamedType(name, tp) => // a named argument
          var res = tp
          val pos = params.findIndexOf(p => p.name == name && !p.hasFlag(SYNTHETIC))
          if (pos == -1) {
            if (positionalAllowed) { // treat assignment as positional argument
              argPos(index) = index
              res = UnitClass.tpe
            } else                   // unknown parameter name
              namesOK = false
          } else if (argPos.contains(pos)) { // parameter specified twice
            namesOK = false
          } else {
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

    /** Is there an instantiation of free type variables <code>undetparams</code>
     *  such that function type <code>ftpe</code> is applicable to
     *  <code>argtpes</code> and its result conform to <code>pt</code>?
     *
     *  @param undetparams ...
     *  @param ftpe        the type of the function (often a MethodType)
     *  @param argtpes     the argument types; a NamedType(name, tp) for named
     *    arguments. For each NamedType, if `name' does not exist in `ftpe', that
     *    type is set to `Unit', i.e. the corresponding argument is treated as
     *    an assignment expression (@see checkNames).
     *  @param pt          ...
     *  @return            ...
     */
    private def isApplicable(undetparams: List[Symbol], ftpe: Type,
                             argtpes0: List[Type], pt: Type): Boolean =
      ftpe match {
        case OverloadedType(pre, alts) =>
          alts exists (alt => isApplicable(undetparams, pre.memberType(alt), argtpes0, pt))
        case ExistentialType(tparams, qtpe) =>
          isApplicable(undetparams, qtpe, argtpes0, pt)
        case MethodType(params, _) =>
          // repeat varargs as needed, remove ByName
          val formals = formalTypes(params map (_.tpe), argtpes0.length)

          def tryTupleApply: Boolean = {
            // if 1 formal, 1 argtpe (a tuple), otherwise unmodified argtpes0
            val tupleArgTpe = actualTypes(argtpes0 map {
                // no assignment is treated as named argument here
              case NamedType(name, tp) => UnitClass.tpe
              case tp => tp
              }, formals.length)

            argtpes0.length != tupleArgTpe.length &&
              isApplicable(undetparams, ftpe, tupleArgTpe, pt)
          }
          def typesCompatible(argtpes: List[Type]) = {
            val restpe = ftpe.resultType(argtpes)
            if (undetparams.isEmpty) {
              (isCompatible(argtpes, formals) &&
               isWeaklyCompatible(restpe, pt))
            } else {
              try {
                val uninstantiated = new ListBuffer[Symbol]
                val targs = methTypeArgs(undetparams, formals, restpe, argtpes, pt, uninstantiated)
                (exprTypeArgs(uninstantiated.toList, restpe.instantiateTypeParams(undetparams, targs), pt) ne null) &&
                isWithinBounds(NoPrefix, NoSymbol, undetparams, targs)
              } catch {
                case ex: NoInstance => false
              }
            }
          }

          // very similar logic to doTypedApply in typechecker
          if (argtpes0.length > formals.length) tryTupleApply
          else if (argtpes0.length == formals.length) {
            if (!argtpes0.exists(_.isInstanceOf[NamedType])) {
              // fast track if no named arguments are used
              typesCompatible(argtpes0)
            } else {
              // named arguments are used
              val (argtpes1, argPos, namesOK) = checkNames(argtpes0, params)
              if (!namesOK) false
              // when using named application, the vararg param has to be specified exactly once
              else if (!isIdentity(argPos) && (formals.length != params.length)) false
              else {
                // nb. arguments and names are OK, check if types are compatible
                typesCompatible(reorderArgs(argtpes1, argPos))
              }
            }
          } else {
            // not enough arguments, check if applicable using defaults
            val missing = missingParams[Type](argtpes0, params, {
              case NamedType(name, _) => Some(name)
              case _ => None
            })._1
            if (missing.exists(!_.hasFlag(DEFAULTPARAM))) tryTupleApply
            else {
              val argtpes1 = argtpes0 ::: missing.map {
                p => NamedType(p.name, p.tpe) // add defaults as named arguments
              }
              isApplicable(undetparams, ftpe, argtpes1, pt)
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

    private[typechecker] def isApplicableSafe(undetparams: List[Symbol], ftpe: Type,
                                              argtpes0: List[Type], pt: Type): Boolean = {
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

    /** Is type <code>ftpe1</code> strictly more specific than type <code>ftpe2</code>
     *  when both are alternatives in an overloaded function?
     *  @see SLS (sec:overloading-resolution)
     *
     *  @param ftpe1 ...
     *  @param ftpe2 ...
     *  @return      ...
     */
    def isAsSpecific(ftpe1: Type, ftpe2: Type): Boolean = ftpe1 match {
      case OverloadedType(pre, alts) =>
        alts exists (alt => isAsSpecific(pre.memberType(alt), ftpe2))
      case et: ExistentialType =>
        et.withTypeVars(isAsSpecific(_, ftpe2)) // !!! why isStrictly?
      case MethodType(params @ (x :: xs), _) =>
        isApplicable(List(), ftpe2, params map (_.tpe), WildcardType)
      case PolyType(_, MethodType(params @ (x :: xs), _)) =>
        isApplicable(List(), ftpe2, params map (_.tpe), WildcardType)
      case ErrorType =>
        true
      case _ =>
        ftpe2 match {
          case OverloadedType(pre, alts) =>
            alts forall (alt => isAsSpecific(ftpe1, pre.memberType(alt)))
          case et: ExistentialType =>
            et.withTypeVars(isAsSpecific(ftpe1, _))
          case MethodType(_, _) | PolyType(_, MethodType(_, _)) =>
            true
          case _ =>
            isAsSpecificValueType(ftpe1, ftpe2, List(), List())
        }
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
    def isProperSubClassOrObject(sym1: Symbol, sym2: Symbol): Boolean =
      sym1 != sym2 && sym1 != NoSymbol && (sym1 isSubClass sym2) ||
      sym1.isModuleClass && isProperSubClassOrObject(sym1.linkedClassOfClass, sym2) ||
      sym2.isModuleClass && isProperSubClassOrObject(sym1, sym2.linkedClassOfClass)

    /** is symbol `sym1` defined in a proper subclass of symbol `sym2`?
     */
    def isInProperSubClassOrObject(sym1: Symbol, sym2: Symbol) =
      sym2 == NoSymbol || isProperSubClassOrObject(sym1.owner, sym2.owner)

    def isStrictlyMoreSpecific(ftpe1: Type, ftpe2: Type, sym1: Symbol, sym2: Symbol): Boolean =
      // ftpe1 / ftpe2 are OverloadedTypes (possibly with one single alternative) if they
      // denote the type of an "apply" member method (see "followApply")
      ftpe1.isError || {
        val specificCount = (if (isAsSpecific(ftpe1, ftpe2)) 1 else 0) -
                            (if (isAsSpecific(ftpe2, ftpe1) &&
                                 // todo: move to isAsSepecific test
                                 (!ftpe2.isInstanceOf[OverloadedType] || ftpe1.isInstanceOf[OverloadedType]) &&
                                 (!phase.erasedTypes || covariantReturnOverride(ftpe1, ftpe2))) 1 else 0)
        val subClassCount = (if (isInProperSubClassOrObject(sym1, sym2)) 1 else 0) -
                            (if (isInProperSubClassOrObject(sym2, sym1)) 1 else 0)
        specificCount + subClassCount > 0
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

    private def isAsSpecificValueType(tpe1: Type, tpe2: Type, undef1: List[Symbol], undef2: List[Symbol]): Boolean = (tpe1, tpe2) match {
      case (PolyType(tparams1, rtpe1), _) =>
        isAsSpecificValueType(rtpe1, tpe2, undef1 ::: tparams1, undef2)
      case (_, PolyType(tparams2, rtpe2)) =>
        isAsSpecificValueType(tpe1, rtpe2, undef1, undef2 ::: tparams2)
      case _ =>
        existentialAbstraction(undef1, tpe1) <:< existentialAbstraction(undef2, tpe2)
    }

/*
    /** Is type `tpe1' a strictly better expression alternative than type `tpe2'?
     */
    def isStrictlyBetterExpr(tpe1: Type, tpe2: Type) = {
      isMethod(tpe2) && !isMethod(tpe1) ||
      isNullary(tpe1) && !isNullary(tpe2) ||
      isStrictlyBetter(tpe1, tpe2)
    }

    /** Is type `tpe1' a strictly better alternative than type `tpe2'?
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
        case _ => tpe.paramSectionCount == 0 || tpe.paramTypes.isEmpty
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
    def checkBounds(pos: Position, pre: Type, owner: Symbol,
                    tparams: List[Symbol], targs: List[Type], prefix: String) = {
      //@M validate variances & bounds of targs wrt variances & bounds of tparams
      //@M TODO: better place to check this?
      //@M TODO: errors for getters & setters are reported separately
      val kindErrors = checkKindBounds(tparams, targs, pre, owner)

      if(!kindErrors.isEmpty) {
        error(pos,
          prefix + "kinds of the type arguments " + targs.mkString("(", ",", ")") +
          " do not conform to the expected kinds of the type parameters "+ tparams.mkString("(", ",", ")") + tparams.head.locationString+ "." +
          kindErrors.toList.mkString("\n", ", ", ""))
      } else if (!isWithinBounds(pre, owner, tparams, targs)) {
        if (!(targs exists (_.isErroneous)) && !(tparams exists (_.isErroneous))) {
          //val bounds = instantiatedBounds(pre, owner, tparams, targs)//DEBUG
          //println("bounds = "+bounds+", targs = "+targs+", targclasses = "+(targs map (_.getClass))+", parents = "+(targs map (_.parents)))
          //println(List.map2(bounds, targs)((bound, targ) => bound containsType targ))
          error(pos,
                prefix + "type arguments " + targs.mkString("[", ",", "]") +
                " do not conform to " + tparams.head.owner + "'s type parameter bounds " +
                (tparams map (_.defString)).mkString("[", ",", "]"))
          if (settings.explaintypes.value) {
            val bounds = tparams map (tp => tp.info.instantiateTypeParams(tparams, targs).bounds)
            List.map2(targs, bounds)((targ, bound) => explainTypes(bound.lo, targ))
            List.map2(targs, bounds)((targ, bound) => explainTypes(targ, bound.hi))
            ()
          }
        }
      }
    }

    /** Check whether <arg>sym1</arg>'s variance conforms to <arg>sym2</arg>'s variance
     *
     * If <arg>sym2</arg> is invariant, <arg>sym1</arg>'s variance is irrelevant. Otherwise they must be equal.
     */
    def variancesMatch(sym1: Symbol, sym2: Symbol): Boolean = (sym2.variance==0 || sym1.variance==sym2.variance)

    /** Check well-kindedness of type application (assumes arities are already checked) -- @M
     *
     * This check is also performed when abstract type members become concrete (aka a "type alias") -- then tparams.length==1
     * (checked one type member at a time -- in that case, prefix is the name of the type alias)
     *
     * Type application is just like value application: it's "contravariant" in the sense that
     * the type parameters of the supplied type arguments must conform to the type parameters of
     * the required type parameters:
     *   - their bounds must be less strict
     *   - variances must match (here, variances are absolute, the variance of a type parameter does not influence the variance of its higher-order parameters)
     *   - @M TODO: are these conditions correct,sufficient&necessary?
     *
     *  e.g. class Iterable[t, m[+x <: t]] --> the application Iterable[Int, List] is okay, since
     *       List's type parameter is also covariant and its bounds are weaker than <: Int
     */
    def checkKindBounds(tparams: List[Symbol], targs: List[Type], pre: Type, owner: Symbol): List[String] = {
      def transform(tp: Type, clazz: Symbol): Type = tp.asSeenFrom(pre, clazz) // instantiate type params that come from outside the abstract type we're currently checking

      // check that the type parameters <arg>hkargs</arg> to a higher-kinded type conform to the expected params <arg>hkparams</arg>
      def checkKindBoundsHK(hkargs: List[Symbol], arg: Symbol, param: Symbol, paramowner: Symbol): (List[(Symbol, Symbol)], List[(Symbol, Symbol)], List[(Symbol, Symbol)]) = {
// NOTE: sometimes hkargs != arg.typeParams, the symbol and the type may have very different type parameters
        val hkparams = param.typeParams

        if(hkargs.length != hkparams.length) {
          if(arg == AnyClass || arg == NothingClass) (Nil, Nil, Nil) // Any and Nothing are kind-overloaded
          else (List((arg, param)), Nil, Nil)
        } else {
          val _arityMismatches = new ListBuffer[(Symbol, Symbol)]
          val _varianceMismatches = new ListBuffer[(Symbol, Symbol)]
          val _stricterBounds = new ListBuffer[(Symbol, Symbol)]
          def varianceMismatch(a: Symbol, p: Symbol) { _varianceMismatches += ((a, p)) }
          def stricterBound(a: Symbol, p: Symbol) { _stricterBounds += ((a, p)) }
          def arityMismatches(as: Iterable[(Symbol, Symbol)]) { _arityMismatches ++= as }
          def varianceMismatches(as: Iterable[(Symbol, Symbol)]) { _varianceMismatches ++= as }
          def stricterBounds(as: Iterable[(Symbol, Symbol)]) { _stricterBounds ++= as }

          for ((hkarg, hkparam) <- hkargs zip hkparams) {
            if (hkparam.typeParams.isEmpty) { // base-case: kind *
              if (!variancesMatch(hkarg, hkparam))
                varianceMismatch(hkarg, hkparam)

              // instantiateTypeParams(tparams, targs) --> higher-order bounds may contain references to type arguments
              // substSym(hkparams, hkargs) --> these types are going to be compared as types of kind *
              //    --> their arguments use different symbols, but are conceptually the same
              //        (could also replace the types by polytypes, but can't just strip the symbols, as ordering is lost then)
              if (!(transform(hkparam.info.instantiateTypeParams(tparams, targs).bounds.substSym(hkparams, hkargs), paramowner) <:< transform(hkarg.info.bounds, owner)))
                stricterBound(hkarg, hkparam)
            } else {
              val (am, vm, sb) = checkKindBoundsHK(hkarg.typeParams, hkarg, hkparam, paramowner)
              arityMismatches(am)
              varianceMismatches(vm)
              stricterBounds(sb)
            }
          }

          (_arityMismatches.toList, _varianceMismatches.toList, _stricterBounds.toList)
        }
      }

      // @M TODO this method is duplicated all over the place (varianceString)
      def varStr(s: Symbol): String =
        if (s.isCovariant) "covariant"
        else if (s.isContravariant) "contravariant"
        else "invariant";

      def qualify(a0: Symbol, b0: Symbol): String = if (a0.toString != b0.toString) "" else {
        assert(a0 ne b0)
        assert(a0.owner ne b0.owner)
        var a = a0; var b = b0
        while (a.owner.name == b.owner.name) { a = a.owner; b = b.owner}
        if (a.locationString ne "") " (" + a.locationString.trim + ")" else ""
      }

      val errors = new ListBuffer[String]
      (tparams zip targs).foreach{ case (tparam, targ) if (targ.isHigherKinded || !tparam.typeParams.isEmpty) => //println("check: "+(tparam, targ))
        val (arityMismatches, varianceMismatches, stricterBounds) =
          checkKindBoundsHK(targ.typeParams, targ.typeSymbolDirect, tparam, tparam.owner) // NOTE: *not* targ.typeSymbol, which normalizes
            // NOTE 2: must use the typeParams of the type targ, not the typeParams of the symbol of targ!!

        if (!(arityMismatches.isEmpty && varianceMismatches.isEmpty && stricterBounds.isEmpty)){
          errors += (targ+"'s type parameters do not match "+tparam+"'s expected parameters: "+
            (for ((a, p) <- arityMismatches)
             yield a+qualify(a,p)+ " has "+reporter.countElementsAsString(a.typeParams.length, "type parameter")+", but "+
              p+qualify(p,a)+" has "+reporter.countAsString(p.typeParams.length)).toList.mkString(", ") +
            (for ((a, p) <- varianceMismatches)
             yield a+qualify(a,p)+ " is "+varStr(a)+", but "+
              p+qualify(p,a)+" is declared "+varStr(p)).toList.mkString(", ") +
            (for ((a, p) <- stricterBounds)
              yield a+qualify(a,p)+"'s bounds "+a.info+" are stricter than "+
              p+qualify(p,a)+"'s declared bounds "+p.info).toList.mkString(", "))
        }
       // case (tparam, targ) => println("no check: "+(tparam, targ, tparam.typeParams.isEmpty))
       case _ =>
      }

      errors.toList
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
                              strictPt: Type, lenientPt: Type) {
      if (inferInfo)
        println("infer argument instance "+tree+":"+tree.tpe+"\n"+
                "  undetparams = "+undetparams+"\n"+
                "  strict pt = "+strictPt+"\n"+
                "  lenient pt = "+lenientPt)
      var targs = exprTypeArgs(undetparams, tree.tpe, strictPt)
      if ((targs eq null) || !(tree.tpe.subst(undetparams, targs) <:< strictPt)) {
        targs = exprTypeArgs(undetparams, tree.tpe, lenientPt)
      }
      substExpr(tree, undetparams, targs, lenientPt)
    }

    /** Substitute free type variables `undetparams; of polymorphic expression
     *  <code>tree</code>, given prototype <code>pt</code>.
     *
     *  @param tree ...
     *  @param undetparams ...
     *  @param pt ...
     */
    def inferExprInstance(tree: Tree, tparams: List[Symbol], pt: Type, keepNothings: Boolean): List[Symbol] = {
      if (inferInfo)
        println("infer expr instance "+tree+":"+tree.tpe+"\n"+
                "  tparams = "+tparams+"\n"+
                "  pt = "+pt)
      val targs = exprTypeArgs(tparams, tree.tpe, pt)
      val uninstantiated = new ListBuffer[Symbol]
      val detargs = if (keepNothings || (targs eq null)) targs
                    else adjustTypeArgs(tparams, targs, WildcardType, uninstantiated)
      val undetparams = uninstantiated.toList
      val detparams = tparams remove (undetparams contains _)
      substExpr(tree, detparams, detargs, pt)
      if (inferInfo)
        println("inferred expr instance "+tree+", detargs = "+detargs+", undetparams = "+undetparams)
      undetparams
    }

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
                            args: List[Tree], pt0: Type): List[Symbol] = fn.tpe match {
      case MethodType(params0, _) =>
        if (inferInfo)
          println("infer method instance "+fn+"\n"+
                  "  undetparams = "+undetparams+"\n"+
                  "  args = "+args+"\n"+
                  "  pt = "+pt0)
        try {
          val pt = if (pt0.typeSymbol == UnitClass) WildcardType else pt0
          val formals = formalTypes(params0 map (_.tpe), args.length)
          val argtpes = actualTypes(args map (_.tpe.deconst), formals.length)
          val restpe = fn.tpe.resultType(argtpes)
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
                args map (_.tpe.widen), WildcardType) +
              "\n --- because ---\n" + ex.getMessage())
            List()
        }
    }

    /** Type with all top-level occurrences of abstract types replaced by their bounds */
    def widen(tp: Type): Type = tp match { // @M don't normalize here (compiler loops on pos/bug1090.scala )
      case TypeRef(_, sym, _) if sym.isAbstractType =>
        widen(tp.bounds.hi)
      case TypeRef(_, sym, _) if sym.isAliasType =>
        widen(tp.normalize)
      case rtp @ RefinedType(parents, decls) =>
        copyRefinedType(rtp, List.mapConserve(parents)(widen), decls)
      case AnnotatedType(_, underlying, _) =>
        widen(underlying)
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
    def inferConstructorInstance(tree: Tree, undetparams: List[Symbol], pt0: Type) {
      val pt = widen(pt0)
      //println("infer constr inst "+tree+"/"+undetparams+"/"+pt0)
      var restpe = tree.tpe.finalResultType
      var tvars = undetparams map freshVar

      /** Compute type arguments for undetermined params and substitute them in given tree.
       */
      def computeArgs =
        try {
          val targs = solvedTypes(tvars, undetparams, undetparams map varianceInType(restpe),
                                  true, lubDepth(List(restpe, pt)))
//          checkBounds(tree.pos, NoPrefix, NoSymbol, undetparams, targs, "inferred ")
//          no checkBounds here. If we enable it, test bug602 fails.
          new TreeTypeSubstituter(undetparams, targs).traverse(tree)
        } catch {
          case ex: NoInstance =>
            errorTree(tree, "constructor of type " + restpe +
                      " cannot be uniquely instantiated to expected type " + pt +
                      "\n --- because ---\n" + ex.getMessage())
        }
      def instError = {
        if (settings.debug.value) Console.println("ici " + tree + " " + undetparams + " " + pt)
        if (settings.explaintypes.value) explainTypes(restpe.instantiateTypeParams(undetparams, tvars), pt)
        errorTree(tree, "constructor cannot be instantiated to expected type" +
                  foundReqMsg(restpe, pt))
      }
      if (restpe.instantiateTypeParams(undetparams, tvars) <:< pt) {
        computeArgs
      } else if (isFullyDefined(pt)) {
        if (settings.debug.value) log("infer constr " + tree + ":" + restpe + ", pt = " + pt)
        var ptparams = freeTypeParamsOfTerms.collect(pt)
        if (settings.debug.value) log("free type params = " + ptparams)
        val ptWithWildcards = pt.instantiateTypeParams(ptparams, ptparams map (ptparam => WildcardType))
        tvars = undetparams map freshVar
        if (restpe.instantiateTypeParams(undetparams, tvars) <:< ptWithWildcards) {
          computeArgs
          restpe = skipImplicit(tree.tpe.resultType)
          if (settings.debug.value) log("new tree = " + tree + ":" + restpe)
          val ptvars = ptparams map freshVar
          val pt1 = pt.instantiateTypeParams(ptparams, ptvars)
          if (isPopulated(restpe, pt1)) {
            ptvars foreach instantiateTypeVar
          } else { if (settings.debug.value) Console.println("no instance: "); instError }
        } else { if (settings.debug.value) Console.println("not a subtype " + restpe.instantiateTypeParams(undetparams, tvars) + " of " + ptWithWildcards); instError }
      } else { if (settings.debug.value) Console.println("not fuly defined: " + pt); instError }
    }

    def instBounds(tvar: TypeVar): (Type, Type) = {
      val tparam = tvar.origin.typeSymbol
      val instType = toOrigin(tvar.constr.inst)
      val (loBounds, hiBounds) =
        if (instType != NoType && isFullyDefined(instType)) (List(instType), List(instType))
        else (tvar.constr.lobounds, tvar.constr.hibounds)
      val lo = lub(tparam.info.bounds.lo :: loBounds map toOrigin)
      val hi = glb(tparam.info.bounds.hi :: hiBounds map toOrigin)
      (lo, hi)
    }

    def isInstantiatable(tvars: List[TypeVar]) = {
      def cloneTypeVar(tv: TypeVar) = {
        val tv1 = TypeVar(tv.origin, new TypeConstraint(tv.constr.lobounds, tv.constr.hibounds))
        tv1.constr.inst = tv.constr.inst
        tv1
      }
      val tvars1 = tvars map cloneTypeVar
      // Note: right now it's not clear that solving is complete, or how it can be made complete!
      // So we should come back to this and investigate.
      solve(tvars1, tvars1 map (_.origin.typeSymbol), tvars1 map (x => COVARIANT), false)
    }

    def instantiateTypeVar(tvar: TypeVar) {
      val tparam = tvar.origin.typeSymbol
      if (false &&
          tvar.constr.inst != NoType &&
          isFullyDefined(tvar.constr.inst) &&
          (tparam.info.bounds containsType tvar.constr.inst)) {
        context.nextEnclosing(_.tree.isInstanceOf[CaseDef]).pushTypeBounds(tparam)
        tparam setInfo tvar.constr.inst
        tparam resetFlag DEFERRED
        if (settings.debug.value) log("new alias of " + tparam + " = " + tparam.info)
      } else {
        val (lo, hi) = instBounds(tvar)
        if (lo <:< hi) {
          if (!((lo <:< tparam.info.bounds.lo) && (tparam.info.bounds.hi <:< hi))) {
            context.nextEnclosing(_.tree.isInstanceOf[CaseDef]).pushTypeBounds(tparam)
            tparam setInfo mkTypeBounds(lo, hi)
            if (settings.debug.value) log("new bounds of " + tparam + " = " + tparam.info)
          } else {
            if (settings.debug.value) log("redundant: "+tparam+" "+tparam.info+"/"+lo+" "+hi)
          }
        } else {
          if (settings.debug.value) log("inconsistent: "+tparam+" "+lo+" "+hi)
        }
      }
    }

    def checkCheckable(pos: Position, tp: Type, kind: String) {
      def patternWarning(tp: Type, prefix: String) = {
        context.unit.uncheckedWarning(pos, prefix+tp+" in type"+kind+" is unchecked since it is eliminated by erasure")
      }
      def check(tp: Type, bound: List[Symbol]) {
        def isLocalBinding(sym: Symbol) =
          sym.isAbstractType &&
          ((bound contains sym) ||
           sym.name == nme.WILDCARD.toTypeName || {
            val e = context.scope.lookupEntry(sym.name)
            (e ne null) && e.sym == sym && e.owner == context.scope
          })
        tp match {
          case SingleType(pre, _) =>
            check(pre, bound)
          case TypeRef(pre, sym, args) =>
            if (sym.isAbstractType)
              patternWarning(tp, "abstract type ")
            else if (sym.isAliasType)
              check(tp.normalize, bound)
            else if (sym == NothingClass || sym == NullClass)
              error(pos, "this type cannot be used in a type pattern")
            else
              for (arg <- args) {
                if (sym == ArrayClass) check(arg, bound)
                else arg match {
                  case TypeRef(_, sym, _) if isLocalBinding(sym) =>
                    ;
                  case _ =>
                    patternWarning(arg, "non variable type-argument ")
                }
              }
            check(pre, bound)
          case RefinedType(parents, decls) =>
            if (decls.isEmpty) for (p <- parents) check(p, bound)
            else patternWarning(tp, "refinement ")
          case ExistentialType(quantified, tp1) =>
            check(tp1, bound ::: quantified)
          case ThisType(_) =>
            ;
          case NoPrefix =>
            ;
          case _ =>
            patternWarning(tp, "type ")
        }
      }
      check(tp, List())
    }

    /** Type intersection of simple type <code>tp1</code> with general
     *  type <code>tp2</code>. The result eliminates some redundancies.
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

    def inferTypedPattern(pos: Position, pattp: Type, pt0: Type): Type = {
      val pt = widen(pt0)
      checkCheckable(pos, pattp, " pattern")
      if (!(pattp <:< pt)) {
        val tpparams = freeTypeParamsOfTerms.collect(pattp)
        if (settings.debug.value) log("free type params (1) = " + tpparams)
        var tvars = tpparams map freshVar
        var tp = pattp.instantiateTypeParams(tpparams, tvars)
        if (!((tp <:< pt) && isInstantiatable(tvars))) {
          tvars = tpparams map freshVar
          tp = pattp.instantiateTypeParams(tpparams, tvars)
          val ptparams = freeTypeParamsOfTerms.collect(pt)
          if (settings.debug.value) log("free type params (2) = " + ptparams)
          val ptvars = ptparams map freshVar
          val pt1 = pt.instantiateTypeParams(ptparams, ptvars)
          if (!(isPopulated(tp, pt1) && isInstantiatable(tvars ::: ptvars))) {
            //println(tpparams)
            //println(tvars map instBounds)
            //println(ptvars map instBounds)
            error(pos, "pattern type is incompatible with expected type"+foundReqMsg(pattp, pt))
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
        val pt1 = pt.instantiateTypeParams(ptparams, ptvars)
        if (pat.tpe <:< pt1)
          ptvars foreach instantiateTypeVar
        else
          error(pat.pos, "pattern type is incompatible with expected type"+foundReqMsg(pat.tpe, pt))
      }

    object toOrigin extends TypeMap {
      def apply(tp: Type): Type = tp match {
        case TypeVar(origin, _) => origin
        case _ => mapOver(tp)
      }
    }

    abstract class SymCollector extends TypeCollector(List[Symbol]()) {
      protected def includeCondition(sym: Symbol): Boolean

      def traverse(tp: Type) {
        tp.normalize match {
          case TypeRef(_, sym, _) =>
            if (includeCondition(sym) && !result.contains(sym)) result = sym :: result
          case _ =>
        }
        mapOver(tp)
      }
    }

    object approximateAbstracts extends TypeMap {
      def apply(tp: Type): Type = tp.normalize match {
        case TypeRef(pre, sym, _) if sym.isAbstractType => WildcardType
        case _ => mapOver(tp)
      }
    }

    /** A traverser to collect type parameters referred to in a type
     */
    object freeTypeParamsOfTerms extends SymCollector {
      protected def includeCondition(sym: Symbol): Boolean =
        sym.isAbstractType && sym.owner.isTerm
    }

    /** A traverser to collect type parameters referred to in a type
     */
    object freeTypeParametersNoSkolems extends SymCollector {
      protected def includeCondition(sym: Symbol): Boolean =
        sym.isTypeParameter && sym.owner.isTerm
    }

    object typeRefs extends SymCollector {
      protected def includeCondition(sym: Symbol): Boolean = true
    }

    def checkDead(tree: Tree): Tree = {
      if (settings.Xwarndeadcode.value && tree.tpe != null && tree.tpe.typeSymbol == NothingClass)
        context.warning (tree.pos, "dead code following this construct")
      tree
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

    /** Assign <code>tree</code> the symbol and type of the alternative which
     *  matches prototype <code>pt</code>, if it exists.
     *  If several alternatives match `pt', take parameterless one.
     *  If no alternative matches `pt', take the parameterless one anyway.
     */
    def inferExprAlternative(tree: Tree, pt: Type): Unit = tree.tpe match {
      case OverloadedType(pre, alts) => tryTwice {
        var alts1 = alts filter (alt => isWeaklyCompatible(pre.memberType(alt), pt))
        //println("trying "+alts1+(alts1 map (_.tpe))+(alts1 map (_.locationString))+" for "+pt)
        val applicable = alts1
        var secondTry = false
        if (alts1.isEmpty) {
          alts1 = alts
          secondTry = true
        }
        def improves(sym1: Symbol, sym2: Symbol): Boolean =
          sym2 == NoSymbol ||
          { val tp1 = pre.memberType(sym1)
            val tp2 = pre.memberType(sym2)
            (tp2 == ErrorType ||
             !global.typer.infer.isWeaklyCompatible(tp2, pt) && global.typer.infer.isWeaklyCompatible(tp1, pt) ||
             isStrictlyMoreSpecific(tp1, tp2, sym1, sym2)) }
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
          if (secondTry) {
            typeErrorTree(tree, tree.symbol.tpe, pt)
          } else {
            if (!pt.isErroneous)
              context.ambiguousError(tree.pos, pre, best, competing.head, "expected type " + pt)
            setError(tree)
          }
        } else {
//          val applicable = alts1 filter (alt =>
//            global.typer.infer.isWeaklyCompatible(pre.memberType(alt), pt))
//          checkNotShadowed(tree.pos, pre, best, applicable)
          tree.setSymbol(best).setType(pre.memberType(best))
        }
      }
    }

    /** Assign <code>tree</code> the type of an alternative which is applicable
     *  to <code>argtpes</code>, and whose result type is compatible with `pt'.
     *  If several applicable alternatives exist, drop the alternatives which use
     *  default arguments, then select the most specialized one.
     *  If no applicable alternative exists, and pt != WildcardType, try again
     *  with pt = WildcardType.
     *  Otherwise, if there is no best alternative, error.
     *
     *  @param argtpes contains the argument types. If an argument is named, as
     *    "a = 3", the corresponding type is `NamedType("a", Int)'. If the name
     *    of some NamedType does not exist in an alternative's parameter names,
     *    the type is replaces by `Unit', i.e. the argument is treated as an
     *    assignment expression.
     */
    def inferMethodAlternative(tree: Tree, undetparams: List[Symbol],
                               argtpes: List[Type], pt0: Type): Unit = tree.tpe match {
      case OverloadedType(pre, alts) =>
        val pt = if (pt0.typeSymbol == UnitClass) WildcardType else pt0
        tryTwice {
          if (settings.debug.value)
            log("infer method alt "+ tree.symbol +" with alternatives "+
                (alts map pre.memberType) +", argtpes = "+ argtpes +", pt = "+ pt)

          val allApplicable = alts filter (alt =>
            isApplicable(undetparams, followApply(pre.memberType(alt)), argtpes, pt))

          // if there are multiple, drop those that use a default
          // (keep those that use vararg / tupling conversion)
          val applicable =
            if (allApplicable.length <= 1) allApplicable
            else allApplicable filter (alt => {
              val mtypes = followApply(alt.tpe) match {
                case OverloadedType(_, alts) =>
                  // for functional values, the `apply' method might be overloaded
                  alts map (_.tpe)
                case t => List(t)
              }
              mtypes.exists(t => t.paramTypes.length < argtpes.length || // tupling (*)
                                 hasExactlyNumParams(t, argtpes.length)) // same nb or vararg
              // (*) more arguments than parameters, but still applicable: tuplig conversion works.
              //     todo: should not return "false" when paramTypes = (Unit) no argument is given
              //     (tupling would work)
            })

          def improves(sym1: Symbol, sym2: Symbol) =
            sym2 == NoSymbol || sym2.isError ||
            isStrictlyMoreSpecific(followApply(pre.memberType(sym1)),
                                   followApply(pre.memberType(sym2)), sym1, sym2)

          val best = ((NoSymbol: Symbol) /: applicable) ((best, alt) =>
            if (improves(alt, best)) alt else best)
          val competing = applicable.dropWhile(alt => best == alt || improves(best, alt))
          if (best == NoSymbol) {
            if (pt == WildcardType) {
              errorTree(tree, applyErrorMsg(tree, " cannot be applied to ", argtpes, pt))
            } else {
              inferMethodAlternative(tree, undetparams, argtpes, WildcardType)
            }
          } else if (!competing.isEmpty) {
            if (!(argtpes exists (_.isErroneous)) && !pt.isErroneous)
              context.ambiguousError(tree.pos, pre, best, competing.head,
                                     "argument types " + argtpes.mkString("(", ",", ")") +
                                     (if (pt == WildcardType) "" else " and expected result type " + pt))
            setError(tree)
            ()
          } else {
//            checkNotShadowed(tree.pos, pre, best, applicable)
            tree.setSymbol(best).setType(pre.memberType(best))
          }
        }
      case _ =>
    }

    /** Try inference twice, once without views and once with views,
     *  unless views are already disabled.
     *
     *  @param infer ...
     */
    def tryTwice(infer: => Unit) {
      if (context.implicitsEnabled) {
        val reportGeneralErrors = context.reportGeneralErrors
        context.reportGeneralErrors = false
        context.implicitsEnabled = false
        try {
          infer
        } catch {
          case ex: CyclicReference =>
            throw ex
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
    def inferPolyAlternatives(tree: Tree, argtypes: List[Type]): Unit = tree.tpe match {
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
            if (!(argtypes exists (_.isErroneous))) {
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
            val bounds = tparams map (_.tpe)  //@M TODO: might be affected by change to tpe in Symbol
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

