/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

import collection.mutable.ListBuffer;
import symtab.Flags._;
import scala.tools.util.Position;

/** Methods to create symbols and to enter them into scopes. */
trait TypeCheckers: Analyzer {
  import global._;
  import definitions._;
  import posAssigner.atPos;

  class TypeCheckPhase(prev: Phase) extends StdPhase(prev) {
    def name = "typechecker";
    val global: TypeCheckers.this.global.type = TypeCheckers.this.global;
    def apply(unit: CompilationUnit): unit =
      unit.body = new TypeChecker(startContext.make(unit)).transformExpr(unit.body)
  }

  class TypeChecker(context: Context) {

    import context.unit;

    val infer = new Inferencer(context);
    import infer._;

    object namer extends Namer(context);

    /** Mode constants
    */
    private val NOmode        = 0x000;
    private val EXPRmode      = 0x001;  // these 3 modes are mutually exclusive.
    private val PATTERNmode   = 0x002;
    private val TYPEmode      = 0x004;

    private val INCONSTRmode  = 0x008;  // orthogonal to above. When set we are
                                        // in the body of a constructor

    private val FUNmode       = 0x10;   // orthogonal to above. When set
                                        // we are looking for a method or constructor

    private val POLYmode      = 0x020;  // orthogonal to above. When set
                                        // expression types can be polymorphic.

    private val QUALmode      = 0x040;  // orthogonal to above. When set
                                        // expressions may be packages and
                                        // Java statics modules.

    private val TAPPmode      = 0x080;  // Set for the function/type constructor part
	                                // of a type application. When set we do not
		                        // decompose PolyTypes.

    private val stickyModes: int  = EXPRmode | PATTERNmode | TYPEmode | INCONSTRmode;

    /** Report a type error.
     *  @param pos    The position where to report the error
     *  @param ex     The exception that caused the error */
    def reportTypeError(pos: int, ex: TypeError): unit = {
      if (settings.debug.value) ex.printStackTrace();
      ex match {
	case CyclicReference(sym, info: TypeCompleter) =>
	  info.tree match {
	    case ValDef(_, _, tpt, _) if (tpt.tpe == null) =>
	      error(pos, "recursive " + sym + " needs type")
	    case DefDef(_, _, _, _, tpt, _) if (tpt.tpe == null) =>
	      error(pos, "recursive " + sym + " needs result type")
	    case _ =>
	      error(pos, ex.getMessage())
	  }
	case _ =>
	  error(pos, ex.getMessage())
      }
    }

    /** Check that tree is a stable expression.
     */
    def checkStable(tree: Tree): Tree =
      if (treeInfo.isPureExpr(tree) || tree.tpe.isError) tree;
      else errorTree(tree, "stable identifier required, but " + tree + " found.");

    /** Check that type of given tree does not contain local or private components
     */
    object checkNoEscaping extends TypeMap {
      private var owner: Symbol = _;
      private var scope: Scope = _;
      private var badSymbol: Symbol = _;

      /** Check that type `tree' does not refer to private components unless itself is wrapped
       *  in something private (`owner' tells where the type occurs). */
      def privates[T <: Tree](owner: Symbol, tree: T): T = check(owner, EmptyScope, tree);

      /**  Check that type `tree' does not refer to entities defined in scope `scope'. */
      def locals[T <: Tree](scope: Scope, tree: T): T = check(NoSymbol, scope, tree);

      def check[T <: Tree](owner: Symbol, scope: Scope, tree: T): T = {
        this.owner = owner;
        this.scope = scope;
        badSymbol = NoSymbol;
        apply(tree.tpe);
        if (badSymbol == NoSymbol) tree
        else {
          error(tree.pos,
                (if (badSymbol.hasFlag(PRIVATE)) "private " else "") + badSymbol +
                " escapes its defining scope as part of type " + tree.tpe);
          setError(tree)
        }
      }
      override def apply(t: Type): Type = {
        def checkNoEscape(sym: Symbol): unit = {
          if (sym.hasFlag(PRIVATE)) {
            var o = owner;
            while (o != NoSymbol && o != sym.owner && !o.isLocal && !o.hasFlag(PRIVATE))
              o = o.owner;
            if (o == sym.owner) badSymbol = sym
          } else if (sym.owner.isTerm) {
            val e = scope.lookupEntry(sym.name);
            if (e != null && e.sym == sym && e.owner == scope) badSymbol = e.sym
          }
        }
        if (badSymbol == NoSymbol)
          t match {
            case TypeRef(_, sym, _) => checkNoEscape(sym)
            case SingleType(_, sym) => checkNoEscape(sym)
            case _ =>
          }
        mapOver(t)
      }
    }

    def reenterValueParams(vparamss: List[List[ValDef]]): unit =
      for (val vparams <- vparamss; val vparam <- vparams) context.scope enter vparam.symbol;

    def reenterTypeParams(tparams: List[AbsTypeDef]): List[Symbol] =
      for (val tparam <- tparams) yield { context.scope enter tparam.symbol; tparam.symbol }

    def attrInfo(attr: Tree): AttrInfo = attr match {
      case Apply(Select(New(tpt), nme.CONSTRUCTOR), args) =>
        Pair(tpt.tpe, args map {
          case Literal(value) =>
            value
          case arg =>
            error(arg.pos, "attribute argument needs to be a constant; found: " + arg);
        })
    }

    private def literalType(value: Any): Type =
      if (value.isInstanceOf[unit]) UnitClass.tpe
      else if (value.isInstanceOf[boolean]) BooleanClass.tpe
      else if (value.isInstanceOf[char]) CharClass.tpe
      else if (value.isInstanceOf[int]) IntClass.tpe
      else if (value.isInstanceOf[long]) LongClass.tpe
      else if (value.isInstanceOf[float]) FloatClass.tpe
      else if (value.isInstanceOf[double]) DoubleClass.tpe
      else if (value.isInstanceOf[String]) StringClass.tpe
      else if (value == null) AllRefClass.tpe
      else throw new FatalError("unexpected literal value: " + value);

    /** Perform the following adaptations of expression, pattern or type `tree' wrt to
     *  given mode `mode' and given prototype `pt':
     *  (1) Resolve overloading, unless mode contains FUNmode
     *  (2) Apply parameterless functions
     *  (3) Apply polymorphic types to fresh instances of their type parameters and
     *      store these instances in context.undetparams,
     *      unless followed by explicit type application.
     *  (4) When in mode EXPRmode but not FUNmode, convert unapplied methods to functions
     *      However, if function is `match' or a constructor, issue an error.
     *  (5) Convert a class type that serves as a constructor in a pattern as follows:
     *  (5.1) If this type refers to a case class, set tree's type to the unique
     *        instance of its primary constructor that is a subtype of the expected type.
     *  (5.2) Otherwise, if this type is a subtype of scala.Seq[A], set trees' type
     *        to a method type from a repeated parameter sequence type A* to the expected type.
     *  (6) Convert all other types to TypeTree nodes.
     *  (7) When in TYPEmode nut not FUNmode, check that types are fully parameterized
     *  (8) When in both EXPRmode and FUNmode, add apply method calls to values of object type.
     *  (9) If there are undetermined type variables and not POLYmode, infer expression instance
     *  Then, if tree's type is not a subtype of expected type, try the following adaptations:
     *  (10) If the expected type is byte, short or char, and the expression
     *      is an integer fitting in the range of that type, convert it to that type.
     *  (11) Widen numeric literals to their expected type, if necessary
     *  (12) When in mode EXPRmode, convert E to { E; () } if expected type is Scala.unit.
     *  (13) When in mode EXPRmode, apply a view
     *  If all this fails, error
     */
    private def adapt(tree: Tree, mode: int, pt: Type): Tree = tree.tpe match {
      case OverloadedType(pre, alts) if ((mode & FUNmode) == 0) => // (1)
	inferExprAlternative(tree, pt);
	adapt(tree, mode, pt)
      case PolyType(List(), restpe) => // (2)
	transform(constfold(tree.setType(restpe)), mode, pt);
      case PolyType(tparams, restpe) if ((mode & TAPPmode) == 0) => // (3)
	val tparams1 = tparams map (.cloneSymbol);
        val tree1 = if (tree.isType) tree
                    else TypeApply(tree, tparams1 map (tparam =>
                      TypeTree() setPos tparam.pos setType tparam.tpe));
	context.undetparams = context.undetparams ::: tparams1;
	adapt(tree1 setType restpe.substSym(tparams, tparams1), mode, pt)
      case MethodType(_, _) if ((mode & (EXPRmode | FUNmode)) == EXPRmode &&
				isCompatible(tree.tpe, pt)) => // (4)
	val meth = treeInfo.methSymbol(tree);
	if (meth.isConstructor) errorTree(tree, "missing arguments for " + meth)
	else transform(etaExpand(tree, tree.tpe), mode, pt)
      case _ =>
	if (tree.isType) {
	  val clazz = tree.tpe.symbol;
	  if ((mode & PATTERNmode) != 0) { // (5)
	    if (clazz.hasFlag(CASE)) {   // (5.1)
	      val tparams = context.undetparams;
	      context.undetparams = List();
	      inferConstructorInstance(
		TypeTree() setPos tree.pos
                  setType tree.tpe.prefix.memberType(clazz.primaryConstructor),
		tparams, pt);
	      tree
	    } else if (clazz.isSubClass(SeqClass)) { // (5.2)
	      pt.baseType(clazz).baseType(SeqClass) match {
		case TypeRef(pre, seqClass, args) =>
		  tree.setType(MethodType(List(typeRef(pre, RepeatedParamClass, args)), pt))
		case NoType =>
		  errorTree(tree, "expected pattern type " + pt +
			    " does not conform to sequence " + clazz)
	      }
	    } else errorTree(tree,
                             clazz.toString() + " is neither a case class nor a sequence class")
	  } else if ((mode & FUNmode) != 0) {
	    tree
	  } else if (tree.symbol != null && !tree.symbol.typeParams.isEmpty) { // (7)
            errorTree(tree, "" + clazz + " takes type parameters");
          } else tree match { // (6)
            case TypeTree() => tree
            case _ => TypeTree() setPos tree.pos setType tree.tpe
          }
	} else if ((mode & (EXPRmode | FUNmode)) == (EXPRmode | FUNmode) &&
		   tree.tpe.member(nme.apply).filter(
		     m => m.tpe.paramSectionCount > 0) != NoSymbol) { // (8)
	  transform(Select(tree, nme.apply), mode, pt)
	} else if (!context.undetparams.isEmpty & (mode & POLYmode) == 0) { // (9)
	    val tparams = context.undetparams;
	    context.undetparams = List();
	    inferExprInstance(tree, tparams, pt);
	    tree
	} else if (tree.tpe <:< pt) {
	    tree
	} else {
	  val tree1 = constfold(tree, pt); // (10) (11)
	  if (tree1 != tree) transform(tree1, mode, pt);
	  else if ((mode & EXPRmode) != 0)
	    if (pt.symbol == UnitClass && tree1.tpe <:< AnyClass.tpe)  // (12)
	      transform(Block(List(tree), Literal(())), mode, pt)
	    else { // (13)
	      val vmeth = bestView(tree.tpe, pt);
	      if (vmeth != NoSymbol)
		transform(Apply(Ident(vmeth.name), List(tree)), mode, pt)
	      else typeErrorTree(tree, tree.tpe, pt)
	    }
	  else typeErrorTree(tree, tree.tpe, pt)
	}
    }

    def completeSuperType(supertpt: Tree, tparams: List[Symbol], vparamss: List[List[ValDef]], superargs: List[Tree]): Type = {
      new Typer(context).enterValueParams(context.owner, vparamss);
      context.undetparams = tparams;
      transformExpr(atPos(supertpt.pos)(Apply(Select(New(supertpt), nme.CONSTRUCTOR), superargs)))
        .tpe
    }

    def parentTypes(templ: Template): List[Tree] = {
      var supertpt = transform(templ.parents.head, TYPEmode | FUNmode, WildcardType);
      var mixins = templ.parents.tail map transformType;
      if (supertpt.symbol != null) {
        val tparams = supertpt.symbol.typeParams;
        if (!tparams.isEmpty) {
          val constr @ DefDef(_, _, _, vparamss, _, Apply(_, superargs)) =
	    treeInfo.firstConstructor(templ.body);
          supertpt = gen.TypeTree(
            new TypeChecker(context.makeNewScope(constr, context.owner.owner))
              .completeSuperType(
                supertpt,
                tparams,
                vparamss map (.map(.duplicate.asInstanceOf[ValDef])),
                superargs map (.duplicate))) setPos supertpt.pos;
        } else if (supertpt.symbol.isTrait) {
          supertpt = gen.TypeTree(supertpt.tpe.parents(0)) setPos supertpt.pos;
          mixins = templ.parents
        }
      }
      (supertpt :: mixins) mapConserve (tpt => checkNoEscaping.privates(context.owner, tpt))
    }

    /** Check that
     *  - all parents are class types,
     *  - first parent cluss is not a trait; following classes are traits,
     *  - final classes are not inherited,
     *  - sealed classes are only inherited by classes which are
     *    nested within definition of base class, or that occur within same
     *    statement sequence,
     *  - self-type of current class is a subtype of self-type of each parent class.
     *  - no two parents define same symbol.
     */
    def validateParentClasses(parents: List[Tree], selfType: Type): unit = {
      var c = context;
      do { c = c.outer } while (c.owner == context.owner);
      val defscope = c.scope;

      def validateParentClass(parent: Tree, isFirst: boolean): unit =
	if (!parent.tpe.isError) {
	  val psym = parent.tpe.symbol;
	  if (!psym.isClass)
	    error(parent.pos, "class type expected");
	  else if (!isFirst && !psym.isTrait)
	    error(parent.pos, "" + psym + " is not a trait; cannot be used as mixin");
	  else if (psym.hasFlag(FINAL))
	    error(parent.pos, "illegal inheritance from final class");
	  else if (psym.isSealed) {
	    // are we in same scope as base type definition?
	    val e = defscope.lookupEntry(psym.name);
	    if (!(e.sym == psym && e.owner == defscope)) {
	      // we are not within same statement sequence
	      var c = context;
	      while (c != NoContext && c.owner !=  psym) c = c.outer.enclClass;
	      if (c == NoContext) error(parent.pos, "illegal inheritance from sealed class")
	    }
	  }
	  if (!(selfType <:< parent.tpe.typeOfThis)) {
	    System.out.println(context.owner);//debug
	    System.out.println(context.owner.thisSym);//debug
	    error(parent.pos, "illegal inheritance;\n self-type " +
		  selfType + " does not conform to " + parent +
		  "'s selftype " + parent.tpe.typeOfThis);
	    if (settings.explaintypes.value) explainTypes(selfType, parent.tpe.typeOfThis);
	  }
	  if (parents exists (p => p != parent && p.tpe.symbol == psym && !psym.isError))
	    error(parent.pos, "" + psym + " is inherited twice")
	}

      if (!parents.isEmpty) {
        validateParentClass(parents.head, true);
        for (val p <- parents.tail) validateParentClass(p, false);
      }
    }

    def transformClassDef(cdef: ClassDef): Tree = {
      val clazz = cdef.symbol;
      reenterTypeParams(cdef.tparams);
      val tparams1 = cdef.tparams mapConserve transformAbsTypeDef;
      val tpt1 = checkNoEscaping.privates(clazz.thisSym, transformType(cdef.tpt));
      val impl1 = new TypeChecker(context.make(cdef.impl, clazz, clazz.info.decls))
        .transformTemplate(cdef.impl);
      copy.ClassDef(cdef, cdef.mods, cdef.name, tparams1, tpt1, impl1) setType NoType
    }

    def transformModuleDef(mdef: ModuleDef): Tree = {
      val clazz = mdef.symbol.moduleClass;
      val impl1 = new TypeChecker(context.make(mdef.impl, clazz, clazz.info.decls))
        .transformTemplate(mdef.impl);
      copy.ModuleDef(mdef, mdef.mods, mdef.name, impl1) setType NoType
    }

    def addGetterSetter(stat: Tree): List[Tree] = stat match {
      case vd @ ValDef(mods, _, _, _) if (mods & PRIVATE) == 0 =>
	def setter: DefDef = {
	  val sym = vd.symbol;
	  val setter = sym.owner.info.decls.lookup(nme.SETTER_NAME(sym.name)).suchThat(.hasFlag(ACCESSOR));
	  atPos(vd.pos)(
	    gen.DefDef(setter, vparamss => gen.Assign(gen.mkRef(vparamss.head.head), gen.mkRef(sym)))
	  ).setType(null) // to force type check
	}
	def getter: ValDef = {
	  val sym = vd.symbol;
	  val getter = sym.owner.info.decls.lookup(sym.name).suchThat(.hasFlag(ACCESSOR));
	  atPos(vd.pos)(
	    gen.ValDef(getter, gen.mkRef(sym))
	  ).setType(null) // to force type check
	}
	if ((mods & MUTABLE) != 0) List(stat, getter, setter) else List(stat, getter)
      case _ =>
	List(stat)
    }

    def transformTemplate(templ: Template): Template = {
      templ setSymbol context.owner.newLocalDummy(templ.pos);
      val parents1 = parentTypes(templ);
      validateParentClasses(parents1, context.owner.typeOfThis);
      val body1 = templ.body flatMap addGetterSetter;
      val body2 = transformStats(templ.body, templ.symbol);
      copy.Template(templ, parents1, body2) setType context.owner.tpe
    }

    def transformValDef(vdef: ValDef): ValDef = {
      val sym = vdef.symbol;
      var tpt1 = checkNoEscaping.privates(sym, transformType(vdef.tpt));
      val rhs1 =
	if (vdef.rhs.isEmpty) vdef.rhs
	else new TypeChecker(context.make(vdef, sym)).transform(vdef.rhs, EXPRmode, tpt1.tpe);
      copy.ValDef(vdef, vdef.mods, vdef.name, tpt1, rhs1) setType NoType
    }

    def transformDefDef(ddef: DefDef): DefDef = {
      val meth = ddef.symbol;
      reenterTypeParams(ddef.tparams);
      reenterValueParams(ddef.vparamss);
      val tparams1 = ddef.tparams mapConserve transformAbsTypeDef;
      val vparamss1 = ddef.vparamss mapConserve (.mapConserve(transformValDef));
      var tpt1 = checkNoEscaping.privates(meth, transformType(ddef.tpt));
      val rhs1 =
	if (ddef.name == nme.CONSTRUCTOR) {
	  if (!meth.hasFlag(SYNTHETIC) &&
	      !(meth.owner.isClass ||
		meth.owner.isModuleClass ||
		meth.owner.isAnonymousClass ||
		meth.owner.isRefinementClass))
	    error(ddef.pos, "constructor definition not allowed here " + meth.owner);//debug
	  context.enclClass.owner.setFlag(INCONSTRUCTOR);
	  val result = transform(ddef.rhs, EXPRmode | INCONSTRmode, UnitClass.tpe);
	  context.enclClass.owner.resetFlag(INCONSTRUCTOR);
	  result
	} else {
	  transform(ddef.rhs, EXPRmode, tpt1.tpe);
	}
      copy.DefDef(ddef, ddef.mods, ddef.name, tparams1, vparamss1, tpt1, rhs1) setType NoType
    }

    def transformAbsTypeDef(tdef: AbsTypeDef): AbsTypeDef = {
      val lo1 = checkNoEscaping.privates(tdef.symbol, transformType(tdef.lo));
      val hi1 = checkNoEscaping.privates(tdef.symbol, transformType(tdef.hi));
      copy.AbsTypeDef(tdef, tdef.mods, tdef.name, lo1, hi1) setType NoType
    }

    def transformAliasTypeDef(tdef: AliasTypeDef): AliasTypeDef = {
      reenterTypeParams(tdef.tparams);
      val tparams1 = tdef.tparams mapConserve transformAbsTypeDef;
      val rhs1 = checkNoEscaping.privates(tdef.symbol, transformType(tdef.rhs));
      copy.AliasTypeDef(tdef, tdef.mods, tdef.name, tparams1, rhs1) setType NoType
    }

    def transformLabelDef(ldef: LabelDef): LabelDef = {
      val lsym = namer.enterInScope(
        context.owner.newLabel(ldef.pos, ldef.name) setInfo MethodType(List(), UnitClass.tpe));
      val rhs1 = transform(ldef.rhs, EXPRmode, UnitClass.tpe);
      copy.LabelDef(ldef, ldef.name, ldef.params, rhs1) setType UnitClass.tpe
    }

    def transformBlock(block: Block, mode: int, pt: Type): Block = {
      namer.enterSyms(block.stats);
      val stats1 =
        if ((mode & INCONSTRmode) != 0) {
          val constrCall = transform(block.stats.head, mode, WildcardType);
          context.enclClass.owner.resetFlag(INCONSTRUCTOR);
          constrCall :: block.stats.tail mapConserve transformExpr
        } else {
          block.stats mapConserve transformExpr
        }
      val expr1 = transform(block.expr, mode & ~(FUNmode | QUALmode), pt);
      checkNoEscaping.locals(
        context.scope, copy.Block(block, stats1, expr1) setType expr1.tpe.deconst)
    }

    def transformCase(cdef: CaseDef, pattpe: Type, pt: Type): CaseDef = {
      val pat1: Tree = transform(cdef.pat, PATTERNmode, pattpe);
      val guard1: Tree = if (cdef.guard == EmptyTree) EmptyTree
	                 else transform(cdef.guard, EXPRmode, BooleanClass.tpe);
      val body1: Tree = transform(cdef.body, EXPRmode, pt);
      copy.CaseDef(cdef, pat1, guard1, body1) setType body1.tpe
    }

    def transformFunction(fun: Function, mode: int, pt: Type): Function = {
      val Triple(clazz, argpts, respt) = pt match {
        case TypeRef(_, sym, argtps)
        if (sym == FunctionClass(fun.vparams.length) ||
            sym == PartialFunctionClass && fun.vparams.length == 1 && fun.body.isInstanceOf[Match]) =>
          Triple(sym, argtps.init, argtps.last)
        case _ =>
          Triple(FunctionClass(fun.vparams.length), fun.vparams map (x => NoType), WildcardType)
      }
      val vparamSyms = List.map2(fun.vparams, argpts) { (vparam, argpt) =>
        vparam match {
          case ValDef(_, _, tpt, _) =>
            if (tpt.isEmpty)
              tpt.tpe =
                if (argpt == NoType) { error(vparam.pos, "missing parameter type"); ErrorType }
		else argpt
        }
        namer.enterSym(vparam)
      }
      val vparams1 = fun.vparams mapConserve transformValDef;
      val body1 = transform(fun.body, EXPRmode, respt);
      copy.Function(fun, vparams1, body1)
	setType typeRef(clazz.tpe.prefix, clazz, (vparamSyms map (.tpe)) ::: List(body1.tpe))
    }

    def transformRefinement(stats: List[Tree]): List[Tree] = {
      for (val stat <- stats) namer.enterSym(stat) setFlag OVERRIDE;
      transformStats(stats, NoSymbol);
    }

    def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] =
      stats mapConserve { stat =>
        if (context.owner.isRefinementClass && !treeInfo.isDeclaration(stat))
	  errorTree(stat, "only declarations allowed here");
        (if (stat.isDef) TypeChecker.this
         else new TypeChecker(context.make(stat, exprOwner))).transformExpr(stat)
      }

    private def transform1(tree: Tree, mode: int, pt: Type): Tree = {

      def funmode = mode & stickyModes | FUNmode | POLYmode;

      def transformCases(cases: List[CaseDef], pattp: Type): List[CaseDef] = {
        val tc1 = new TypeChecker(context.makeNewScope(tree, context.owner));
        cases mapConserve (cdef => tc1.transformCase(cdef, pattp, pt))
      }

      def transformTypeApply(fun: Tree, args: List[Tree]): Tree = fun.tpe match {
	case OverloadedType(pre, alts) =>
          inferPolyAlternative(fun, args.length);
          transformTypeApply(fun, args)
        case PolyType(tparams, restpe) if (tparams.length != 0) =>
          if (tparams.length == args.length) {
            val targs = args map (.tpe);
            checkBounds(tree.pos, tparams, targs, "");
	    System.out.println("type app " + tparams + " => " + targs + " = " + restpe.subst(tparams, targs));//debug
	    copy.TypeApply(tree, fun, args) setType restpe.subst(tparams, targs);
          } else {
            errorTree(tree, "wrong number of type parameters for " + treeSymTypeMsg(fun))
          }
	case ErrorType =>
	  setError(tree)
	case _ =>
          errorTree(tree, treeSymTypeMsg(fun) + " takes type parameters.");
        }

      def transformApply(fun: Tree, args: List[Tree]): Tree = fun.tpe match {
        case OverloadedType(pre, alts) =>
          val args1 = args mapConserve (arg =>
            transform(arg, mode & stickyModes, WildcardType));
          inferMethodAlternative(fun, context.undetparams, args1 map (.tpe.deconst), pt);
          transformApply(adapt(fun, funmode, WildcardType), args1);
        case MethodType(formals0, restpe) =>
          val formals = formalTypes(formals0, args.length);
          if (formals.length != args.length) {
            errorTree(tree, "wrong number of arguments for " + treeSymTypeMsg(fun))
          } else {
            val tparams = context.undetparams;
            context.undetparams = List();
            if (tparams.isEmpty) {
              val args1 = List.map2(args, formals) ((arg, formal) =>
                transform(arg, mode & stickyModes, formal));
              val tree1 = copy.Apply(tree, fun, args1).setType(restpe);
              val tree2 = constfold(tree1);
              if (tree1 == tree2) tree2 else transform(tree2, mode, pt)
	    } else {
	      assert((mode & PATTERNmode) == 0); // this case cannot arise for patterns
              val lenientTargs = protoTypeArgs(tparams, formals, restpe, pt);
              val strictTargs = List.map2(lenientTargs, tparams)((targ, tparam) =>
                if (targ == WildcardType) tparam.tpe else targ);
              def transformArg(tree: Tree, formal: Type): Tree = {
	        val lenientPt = formal.subst(tparams, lenientTargs);
	        val tree1 = transform(tree, mode & stickyModes | POLYmode, lenientPt);
	        val argtparams = context.undetparams;
	        context.undetparams = List();
	        if (!argtparams.isEmpty) {
	          val strictPt = formal.subst(tparams, strictTargs);
	          inferArgumentInstance(tree1, argtparams, strictPt, lenientPt);
	        }
	        tree1
              }
              val args1 = List.map2(args, formals)(transformArg);
              if (args1 exists (.tpe.isError)) setError(tree)
              else {
                val undetparams = inferMethodInstance(fun, tparams, args1, pt);
                val result = transformApply(fun, args1);
                context.undetparams = undetparams;
                result
              }
            }
          }
        case ErrorType =>
          setError(tree)
      }

      /** The qualifying class of a this or super with prefix `qual' */
      def qualifyingClass(qual: Name): Symbol = {
        if (qual == nme.EMPTY.toTypeName) {
          val clazz = context.enclClass.owner;
          if (!clazz.isPackageClass) clazz
          else {
            error(tree.pos, "" + tree + " can be used only in a class, object, or template");
            NoSymbol
          }
        } else {
          var c = context.enclClass;
          while (c != NoContext && c.owner.name != qual) c = c.outer.enclClass;
          if (c != NoContext) c.owner
          else {
            error(tree.pos, "" + qual + " is not an enclosing class");
	    NoSymbol
          }
        }
      }

      /** Attribute a selection where `tree' is `qual.name'.
       *  `qual' is already attributed.
       */
      def transformSelect(qual: Tree, name: Name): Tree = {
	val sym = qual.tpe.member(name);
	if (sym == NoSymbol && qual.isTerm) {
	  val vmeth = bestView(qual.tpe, name);
	  if (vmeth != NoSymbol)
	    return transform(Select(Apply(Ident(vmeth.name), List(qual)), name), mode, pt)
	}
        if (sym.info == NoType)
          errorTree(tree, decode(name) + " is not a member of " + qual.tpe.widen)
        else
	  stabilize(checkAccessible(tree, sym, qual.tpe, qual), qual.tpe)
      }

      /** Attribute an identifier consisting of a simple name or an outer reference.
       *  @param tree      The tree representing the identifier.
       *  @param name      The name of the identifier.
       *  Transformations: (1) Prefix class members with this.
       *                   (2) Change imported symbols to selections
       */
      def transformIdent(name: Name): Tree = {
	def ambiguousError(msg: String) =
	  error(tree.pos, "reference to " + name + " is ambiguous;\n" + msg);

	var defSym: Symbol = NoSymbol;   // the directly found symbol
	var defEntry: ScopeEntry = null; // the scope entry of defSym, if defined in a local scope
	var pre: Type = NoPrefix;        // the prefix type of defSym, if a class member

	var cx = context;
	while (defSym == NoSymbol && cx != NoContext) {
	  defEntry = cx.scope.lookupEntry(name);
	  pre = cx.enclClass.owner.thisType;
	  if (defEntry != null) defSym = defEntry.sym
	  else {
            cx = cx.enclClass;
	    defSym = pre.member(name);
	    if (defSym == NoSymbol) cx = cx.outer;
	  }
	}
	val symDepth = if (defEntry == null) cx.depth
		       else cx.depth - (cx.scope.nestingLevel - defEntry.owner.nestingLevel);
	var impSym: Symbol = NoSymbol;      // the imported symbol
	var imports = context.imports;      // impSym != NoSymbol => it is imported from imports.head
	while (impSym == NoSymbol && !imports.isEmpty && imports.head.depth > symDepth) {
	  impSym = imports.head.importedSymbol(name);
	  if (impSym == NoSymbol) imports = imports.tail;
	}

	// detect ambiguous definition/import,
	// update `defSym' to be the final resolved symbol,
	// update `pre' to be `sym's prefix type in case it is an imported member,
	// and compute value of:
	var qual: Tree = EmptyTree;   // the qualififier tree if transformed tree is a select
	if (defSym.tpe != NoType) {
	  if (impSym.tpe != NoType)
	    ambiguousError(
	      "it is both defined in " + defSym.owner +
	      " and imported subsequently by \n" + imports.head);
	  else if (defSym.owner.isClass && !defSym.owner.isPackageClass && !defSym.isTypeParameter)
	    qual = gen.This(pre.symbol) setPos tree.pos
          else
            pre = NoPrefix;
	} else {
	  if (impSym.tpe != NoType) {
	    var impSym1 = NoSymbol;
	    var imports1 = imports.tail;
	    def ambiguousImportError = ambiguousError(
	      "it is imported twice in the same scope by\n" + impSym +  "\nand " + impSym1);
	    while (!imports1.isEmpty && imports1.head.depth == imports.head.depth) {
	      var impSym1 = imports1.head.importedSymbol(name);
	      if (impSym1 != NoSymbol) {
		if (imports1.head.isExplicitImport(name)) {
		  if (imports.head.isExplicitImport(name)) ambiguousImportError;
		  impSym = impSym1;
		  imports = imports1;
		}
		if (imports.head.isExplicitImport(name)) impSym1 = NoSymbol;
	      }
	    }
	    if (impSym1 != NoSymbol) ambiguousImportError;
	    defSym = impSym;
	    qual = imports.head.qual;
	    pre = qual.tpe;
	  } else {
	    error(tree.pos, "not found: " + decode(name));
	    defSym = context.owner.newErrorSymbol(name);
	  }
	}
	val tree1 = if (qual == EmptyTree) tree else Select(qual, name) setPos tree.pos;
	stabilize(checkAccessible(tree1, defSym, pre, qual), pre);
      }

      /** Post-process an identifier or selection node, performing the following:
       *  (1) Turn trees of constant type into literals
       *  (2) Check that non-function pattern expressions are stable
       *  (3) Check that packages and static modules are not used as values
       *  (4) Turn tree type into stable type if possible and required by context. */
      def stabilize(tree: Tree, pre: Type): Tree = tree.tpe match {
        case ConstantType(base, value) => // (1)
          Literal(value) setPos tree.pos setType tree.tpe
        case PolyType(List(), restp @ ConstantType(base, value)) => // (1)
          Literal(value) setPos tree.pos setType restp
        case _ =>
          if (tree.symbol.hasFlag(OVERLOADED) && (mode & FUNmode) == 0)
            inferExprAlternative(tree, pt);
          if ((mode & (PATTERNmode | FUNmode)) == PATTERNmode && tree.isTerm) // (2)
            checkStable(tree)
          else if ((mode & (EXPRmode | QUALmode)) == EXPRmode && !tree.symbol.isValue) // (3)
            errorTree(tree, tree.symbol.toString() + " is not a value");
          else if (tree.symbol.isStable && pre.isStable &&
                   (pt.isStable  || (mode & QUALmode) != 0 || tree.symbol.isModule)) // (4)
            tree.setType(singleType(pre, tree.symbol))
          else
            tree
      }

      // begin transform1
      System.out.println("transforming " + tree);//debug
      val sym: Symbol = tree.symbol;
      if (sym != null) sym.initialize;
      if (settings.debug.value && tree.isDef) global.log("transforming definition of " + sym);
      tree match {
        case PackageDef(name, stats) =>
          val stats1 = new TypeChecker(context.make(tree, sym.moduleClass, sym.info.decls))
            .transformStats(stats, NoSymbol);
          copy.PackageDef(tree, name, stats1) setType NoType

        case cdef @ ClassDef(_, _, _, _, _) =>
          val result = new TypeChecker(context.makeNewScope(tree, sym)).transformClassDef(cdef);
	  System.out.println("entered: " + result.symbol);//debug
	  result


        case mdef @ ModuleDef(_, _, _) =>
          transformModuleDef(mdef)

        case vdef @ ValDef(_, _, _, _) =>
          transformValDef(vdef)

        case ddef @ DefDef(_, _, _, _, _, _) =>
          new TypeChecker(context.makeNewScope(tree, sym)).transformDefDef(ddef)

        case tdef @ AbsTypeDef(_, _, _, _) =>
          transformAbsTypeDef(tdef)

        case tdef @ AliasTypeDef(_, _, _, _) =>
          new TypeChecker(context.makeNewScope(tree, sym)).transformAliasTypeDef(tdef)

        case ldef @ LabelDef(_, List(), _) =>
          new TypeChecker(context.makeNewScope(tree, context.owner)).transformLabelDef(ldef)

        case Import(_, _) =>
          EmptyTree

        case Attributed(attr, defn) =>
          val attr1 = transform(attr, EXPRmode, AttributeClass.tpe);
          val defn1 = transform(defn, mode, pt);
          val existing = attributes.get(defn1.symbol) match {
            case None => List()
            case Some(attrs) => attrs
          }
          attributes(defn1.symbol) = attrInfo(attr1) :: existing;
          defn1

        case DocDef(comment, defn) =>
          transform(defn, mode, pt);

        case block @ Block(_, _) =>
          new TypeChecker(context.makeNewScope(tree, context.owner))
            .transformBlock(block, mode, pt)

        case Sequence(elems) =>
	  val elems1 = elems mapConserve (elem => transform(elem, mode, pt));
          copy.Sequence(tree, elems1) setType pt

        case Alternative(alts) =>
	  val alts1 = alts mapConserve (alt => transform(alt, mode, pt));
          copy.Alternative(tree, alts1) setType pt

        case Bind(name, body) =>
          val body1 = transform(body, mode, pt);
          val vble = context.owner.newValue(tree.pos, name).setInfo(
            if (treeInfo.isSequenceValued(body)) seqType(pt) else pt);
          namer.enterInScope(vble);
          copy.Bind(tree, name, body1) setSymbol vble setType pt

        case fun @ Function(_, _) =>
          new TypeChecker(context.makeNewScope(tree, context.owner))
            .transformFunction(fun, mode, pt)

        case Assign(lhs, rhs) =>
          def isGetter(sym: Symbol) = sym.info match {
            case PolyType(List(), _) => sym.owner.isClass && !sym.isStable
            case _ => false
          }
          val lhs1 = transformExpr(lhs);
          val varsym = lhs1.symbol;
          System.out.println("" + lhs1 + " " + " " + lhs1.getClass() + varsym);//debug
          if (varsym != null && isGetter(varsym)) {
            lhs1 match {
              case Select(qual, name) =>
                transform(Apply(Select(qual, nme.SETTER_NAME(name)), List(rhs)), mode, pt)
            }
          } else if (varsym != null && varsym.isVariable) {
            val rhs1 = transform(rhs, EXPRmode, lhs.tpe);
            copy.Assign(tree, lhs1, rhs1) setType UnitClass.tpe;
          } else {
            System.out.println("" + lhs1 + " " + " " + lhs1.getClass() + varsym);//debug
            if (!lhs.tpe.isError) error(tree.pos, "assignment to non-variable ");
            setError(tree)
          }

        case If(cond, thenp, elsep) =>
          val cond1 = transform(cond, EXPRmode, BooleanClass.tpe);
          if (elsep.isEmpty) {
            val thenp1 = transform(thenp, EXPRmode, UnitClass.tpe);
            copy.If(tree, cond1, thenp1, elsep) setType UnitClass.tpe
          } else {
            val thenp1 = transform(thenp, EXPRmode, pt);
            val elsep1 = transform(thenp, EXPRmode, pt);
            copy.If(tree, cond1, thenp1, elsep1) setType lub(List(thenp1.tpe, elsep1.tpe));
          }

        case Match(selector, cases) =>
          val selector1 = transformExpr(selector);
          val cases1 = transformCases(cases, selector1.tpe);
          copy.Match(tree, selector1, cases1) setType lub(cases1 map (.tpe))

        case Return(expr) =>
          val enclFun = context.owner.enclMethod;
          if (!enclFun.isMethod || enclFun.isConstructor)
            errorTree(tree, "return outside method definition")
          else if (context.owner.hasFlag(INITIALIZED))
            errorTree(tree, "method with return needs result type")
          else {
            val expr1: Tree = transform(expr, EXPRmode, enclFun.tpe.resultType);
            copy.Return(tree, expr1) setSymbol enclFun setType AllClass.tpe;
          }

        case Try(block, catches, finalizer) =>
          val block1 = transform(block, EXPRmode, pt);
          val catches1 = transformCases(catches, ThrowableClass.tpe);
          val finalizer1 = if (finalizer.isEmpty) finalizer
                           else transform(finalizer, EXPRmode, UnitClass.tpe);
          copy.Try(tree, block1, catches1, finalizer1)
            setType lub(block1.tpe :: (catches1 map (.tpe)))

        case Throw(expr) =>
          val expr1 = transform(expr, EXPRmode, ThrowableClass.tpe);
          copy.Throw(tree, expr1) setType AllClass.tpe

        case New(tpt: Tree) =>
          var tpt1 = transform(tpt, TYPEmode | FUNmode, WildcardType);
          if (tpt1.symbol != null && !tpt1.symbol.typeParams.isEmpty) {
            context.undetparams = tpt1.symbol.typeParams;
            tpt1 = TypeTree()
              setPos tpt1.pos
              setType appliedType(tpt1.tpe, context.undetparams map (.tpe));
          }
          copy.New(tree, tpt1).setType(tpt1.tpe)

        case Typed(expr, tpt @ Ident(nme.WILDCARD_STAR)) =>
          val expr1 = transform(expr, mode & stickyModes, seqType(pt));
          expr1.tpe.baseType(SeqClass) match {
            case TypeRef(_, _, List(elemtp)) =>
              copy.Typed(tree, expr1, tpt setType elemtp) setType elemtp
            case _ =>
              setError(tree)
          }
        case Typed(expr, tpt) =>
          val tpt1 = transformType(tpt);
          val expr1 = transform(expr, mode & stickyModes, tpt1.tpe);
          copy.Typed(tree, expr1, tpt1) setType tpt1.tpe

        case TypeApply(fun, args) =>
	  val args1 = args mapConserve transformType;
	  // do args first in order to maintain conext.undetparams on the function side.
          transformTypeApply(transform(fun, funmode | TAPPmode, WildcardType), args1)

        case Apply(fun, args) =>
          val funpt = if ((mode & PATTERNmode) != 0) pt else WildcardType;
          var fun1 = transform(fun, funmode, funpt);
          // if function is overloaded, filter all alternatives that match
	  // number of arguments and expected result type.
	  if (fun1.hasSymbol && fun1.symbol.hasFlag(OVERLOADED)) {
	    val argtypes = args map (arg => AllClass.tpe);
	    val pre = fun1.symbol.tpe.prefix;
            val sym = fun1.symbol filter (alt =>
	      isApplicable(context.undetparams, pre.memberType(alt), argtypes, pt));
            if (sym != NoSymbol)
              fun1 = adapt(fun1 setSymbol sym setType pre.memberType(sym), funmode, WildcardType)
          }
          transformApply(fun1, args)

        case Super(qual, mix) =>
          val clazz = qualifyingClass(qual);
          if (clazz == NoSymbol) setError(tree)
          else {
	    val owntype =
	      if (mix == nme.EMPTY.toTypeName) intersectionType(clazz.info.parents)
              else {
                val ps = clazz.info.parents dropWhile (p => p.symbol.name != mix);
                if (ps.isEmpty) {
                  System.out.println(clazz.info.parents map (.symbol.name));//debug
                  error(tree.pos, "" + mix + " does not name a base class of " + clazz);
                  ErrorType
                } else ps.head
              }
	    tree setSymbol clazz setType owntype
	  }

        case This(qual) =>
          val clazz = qualifyingClass(qual);
          if (clazz == NoSymbol) setError(tree)
          else {
	    val owntype = if (pt.isStable || (mode & QUALmode) != 0) clazz.thisType
			  else clazz.typeOfThis;
            tree setSymbol clazz setType owntype
	  }

        case Select(qual @ Super(_, _), nme.CONSTRUCTOR) =>
          val qual1 = transform(qual, EXPRmode | QUALmode | POLYmode, WildcardType);
          // the qualifier type of a supercall constructor is its first parent class
          qual1.tpe match {
            case RefinedType(parents, _) => qual1.tpe = parents.head;
            case _ =>
          }
          transformSelect(qual1, nme.CONSTRUCTOR);

        case Select(qual, name) =>
          var qual1 = transform(qual, EXPRmode | QUALmode | POLYmode, WildcardType);
          if (name.isTypeName) qual1 = checkStable(qual1);
          transformSelect(qual1, name);

        case Ident(name) =>
          transformIdent(name)

        case Literal(value) =>
          tree setType literalType(value)

        case SingletonTypeTree(ref) =>
          val ref1 = checkStable(transform(ref, EXPRmode | QUALmode, AnyRefClass.tpe));
          tree setType ref1.tpe.resultType;

        case SelectFromTypeTree(qual, selector) =>
          tree setType transformSelect(transformType(qual), selector).tpe

        case CompoundTypeTree(templ: Template) =>
          tree setType {
            val parents1 = templ.parents mapConserve transformType;
            if (parents1 exists (.tpe.isError)) ErrorType
            else {
              val decls = new Scope();
              val self = refinedType(parents1 map (.tpe), context.enclClass.owner, decls);
              new TypeChecker(context.make(tree, self.symbol, decls)).transformRefinement(templ.body);
              self
            }
          }

        case AppliedTypeTree(tpt, args) =>
          val tpt1 = transform(tpt, mode | FUNmode | TAPPmode, WildcardType);
          val tparams = tpt1.tpe.symbol.typeParams;
          val args1 = args mapConserve transformType;
          if (tpt1.tpe.isError)
            setError(tree)
          else if (tparams.length == args1.length)
            tree setType appliedType(tpt1.tpe, args1 map (.tpe))
          else if (tparams.length == 0)
            errorTree(tree, "" + tpt1.tpe + " does not take type parameters")
          else
            errorTree(tree, "wrong number of type arguments for " + tpt1.tpe)
      }
    }

    def transform(tree: Tree, mode: int, pt: Type): Tree =
      try {
        if (tree.tpe != null) tree
	else {
          val tree1 = transform1(tree, mode, pt);
          System.out.println("transformed " + tree1 + ":" + tree1.tpe);//debug
          adapt(tree1, mode, pt)
        }
      } catch {
        case ex: TypeError =>
	  reportTypeError(tree.pos, ex);
	  setError(tree)
      }

    def transformExpr(tree: Tree): Tree = transform(tree, EXPRmode, WildcardType);
    def transformQualExpr(tree: Tree): Tree = transform(tree, EXPRmode | QUALmode, WildcardType);
    def transformType(tree: Tree) = transform(tree, TYPEmode, WildcardType);
  }
}

