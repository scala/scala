/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

import nsc.util.ListBuffer;
import symtab.Flags._;
import scala.tools.util.Position;

/** Methods to create symbols and to enter them into scopes. */
abstract class Typers: Analyzer {
  import global._;
  import definitions._;
  import posAssigner.atPos;

  var appcnt = 0;
  var idcnt = 0;
  var selcnt = 0;
  var implcnt = 0;

  class TypeCheckPhase(prev: Phase) extends StdPhase(prev) {
    def name = "typer";
    val global: Typers.this.global.type = Typers.this.global;
    def apply(unit: CompilationUnit): unit =
      unit.body = newTyper(startContext.make(unit)).transformExpr(unit.body)
  }

  def newTyper(context: Context) = new Typer(context);

  class Typer(context0: Context) {
    import context0.unit;

    val infer = new Inferencer(context0) {
      override def isCoercible(tp: Type, pt: Type): boolean =
        context0.reportGeneralErrors && // this condition prevents chains of views
	inferView(Position.NOPOS, tp, pt, false) != EmptyTree
    }

    private def inferView(pos: int, from: Type, to: Type, reportAmbiguous: boolean): Tree = {
      if (settings.debug.value) System.out.println("infer view from " + from + " to " + to);//debug
      val res = inferImplicit(pos, functionType(List(from), to), true, reportAmbiguous);
      res
    }

    private def inferView(pos: int, from: Type, name: Name, reportAmbiguous: boolean): Tree = {
      val to = refinedType(List(WildcardType), NoSymbol);
      val psym = (if (name.isTypeName) to.symbol.newAbstractType(pos, name)
		  else to.symbol.newValue(pos, name)) setInfo WildcardType;
      to.decls.enter(psym);
      inferView(pos, from, to, reportAmbiguous)
    }

    import infer._;

    private var namerCache: Namer = null;
    def namer = {
      if (namerCache == null || namerCache.context != context) namerCache = new Namer(context);
      namerCache
    }

    var context = context0;

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

    private val stickyModes: int  = EXPRmode | PATTERNmode | TYPEmode;

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
      def privates[T <: Tree](owner: Symbol, tree: T): T = {
	check(owner, EmptyScope, tree);
      }

      /**  Check that type `tree' does not refer to entities defined in scope `scope'. */
      def locals[T <: Tree](scope: Scope, tree: T): T = check(NoSymbol, scope, tree);

      def check[T <: Tree](owner: Symbol, scope: Scope, tree: T): T = {
        this.owner = owner;
        this.scope = scope;
        badSymbol = NoSymbol;
	assert(tree.tpe != null, tree);//debug
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
      else if (value.isInstanceOf[byte]) ByteClass.tpe
      else if (value.isInstanceOf[short]) ShortClass.tpe
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
     *  (4) Do the following to unapplied methods used as values:
     *  (4.1) If the method has only implicit parameters pass implicit arguments
     *  (4.2) otherwise, convert to function by eta-expansion,
     *        except if the method is a constructor, in which case we issue an error.
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
//    def adapt(tree: Tree, mode: int, pt: Type): Tree = {
    private def adapt(tree: Tree, mode: int, pt: Type): Tree = tree.tpe match {
      case OverloadedType(pre, alts) if ((mode & FUNmode) == 0) => // (1)
	inferExprAlternative(tree, pt);
	adapt(tree, mode, pt)
      case PolyType(List(), restpe) => // (2)
	transform(constfold(tree.setType(restpe)), mode, pt);
      case TypeRef(_, sym, List(arg))
      if ((mode & EXPRmode) != 0 && sym == ByNameParamClass) => // (2)
	adapt(tree setType arg, mode, pt);
      case PolyType(tparams, restpe) if ((mode & TAPPmode) == 0) => // (3)
	if (tree.symbol != null) {
	  if (settings.debug.value) System.out.println("adapting " + tree + " " + tree.symbol.tpe + " " + tree.symbol.getClass() + " " + tree.symbol.hasFlag(CASE));//debug
	}
	val tparams1 = cloneSymbols(tparams);
        val tree1 = if (tree.isType) tree
                    else TypeApply(tree, tparams1 map (tparam =>
                      TypeTree() setPos tparam.pos setType tparam.tpe)) setPos tree.pos;
	context.undetparams = context.undetparams ::: tparams1;
	adapt(tree1 setType restpe.substSym(tparams, tparams1), mode, pt)
      case mt: ImplicitMethodType if ((mode & (EXPRmode | FUNmode)) == EXPRmode) => // (4.1)
	transform(applyImplicitArgs(tree), mode, pt)
      case mt: MethodType if ((mode & (EXPRmode | FUNmode)) == EXPRmode &&
	                      isCompatible(tree.tpe, pt)) => // (4.2)
	val meth = treeInfo.methSymbol(tree);
	if (meth.isConstructor) errorTree(tree, "missing arguments for " + meth)
	else transform(etaExpand(tree), mode, pt)
      case _ =>
	if (tree.isType) {
	  val clazz = tree.tpe.symbol;
	  if ((mode & PATTERNmode) != 0) { // (5)
	    if (tree.tpe.isInstanceOf[MethodType]) {
	      tree // everything done already
	    } else {
	      clazz.initialize;
	      if (clazz.hasFlag(CASE)) {   // (5.1)
		val tree1 = TypeTree() setPos tree.pos
		    setType
		      clazz.primaryConstructor.tpe.asSeenFrom(tree.tpe.prefix, clazz.owner);
		// tree.tpe.prefix.memberType(clazz.primaryConstructor); //!!!
		inferConstructorInstance(tree1, clazz.unsafeTypeParams, pt);
		tree1
	      } else if (clazz.isSubClass(SeqClass)) { // (5.2)
		pt.baseType(clazz).baseType(SeqClass) match {
		  case TypeRef(pre, seqClass, args) =>
		    tree.setType(MethodType(List(typeRef(pre, RepeatedParamClass, args)), pt))
		  case NoType =>
		    errorTree(tree, "expected pattern type " + pt +
			      " does not conform to sequence " + clazz)
		}
	      } else {
		System.out.println("bad: " + clazz + ":" + tree.tpe + " " + flagsToString(clazz.flags) + clazz.hasFlag(CASE));//debug
		errorTree(tree, clazz.toString() + " is neither a case class nor a sequence class")
	      }
	    }
	  } else if ((mode & FUNmode) != 0) {
	    tree
	  } else if (tree.symbol != null && !tree.symbol.unsafeTypeParams.isEmpty) { // (7)
            errorTree(tree, "" + clazz + " takes type parameters");
          } else tree match { // (6)
            case TypeTree() => tree
            case _ => TypeTree() setPos tree.pos setType tree.tpe
          }
	} else if ((mode & (EXPRmode | FUNmode)) == (EXPRmode | FUNmode) &&
                   ((mode & TAPPmode) == 0 || tree.tpe.typeParams.isEmpty) &&
		   tree.tpe.member(nme.apply).filter(m => m.tpe.paramSectionCount > 0)
		     != NoSymbol) { // (8)
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
	  else {
            if ((mode & (EXPRmode | FUNmode)) == EXPRmode) {
              assert(pt != null);
              assert(tree1.tpe != null, tree1);
	      if (pt.symbol == UnitClass && tree1.tpe <:< AnyClass.tpe)  // (12)
	        return transform(Block(List(tree), Literal(())), mode, pt)
	      else if (context.reportGeneralErrors) { // (13); the condition prevents chains of views
	        val coercion = inferView(tree.pos, tree.tpe, pt, true);
	        if (coercion != EmptyTree)
		  return transform(Apply(coercion, List(tree)) setPos tree.pos, mode, pt);
	      }
            }
	    typeErrorTree(tree, tree.tpe, pt)
          }
	}
    }
//      System.out.println("adapt " + tree + ":" + tree.tpe + ", mode = " + mode + ", pt = " + pt);
//      adapt(tree, mode, pt)
//    }

    private def completeSuperType(supertpt: Tree, tparams: List[Symbol], vparamss: List[List[ValDef]], superargs: List[Tree]): Type = {
      tparams foreach context.scope.enter;
      namer.enterValueParams(context.owner, vparamss);
      val newTree = New(supertpt) setType
	PolyType(tparams, appliedType(supertpt.tpe, tparams map (.tpe)));
      val tree = transformExpr(atPos(supertpt.pos)(Apply(Select(newTree, nme.CONSTRUCTOR), superargs)));
      if (settings.debug.value) System.out.println("superconstr " + tree + " co = " + context.owner);//debug
      tree.tpe
    }

    def parentTypes(templ: Template): List[Tree] = {
      var supertpt = transform(templ.parents.head, TYPEmode | FUNmode, WildcardType);
      var mixins = templ.parents.tail map transformType;
      // If first parent is trait, make it first mixin and add its superclass as first parent
      if (supertpt.tpe.symbol != null && supertpt.tpe.symbol.initialize.isTrait) {
	mixins = supertpt :: mixins;
        supertpt = gen.TypeTree(supertpt.tpe.parents(0)) setPos supertpt.pos;
      }
      if (supertpt.symbol != null) {
        val tparams = supertpt.symbol.typeParams;
        if (!tparams.isEmpty) {
          val constr @ DefDef(_, _, _, vparamss, _, Apply(_, superargs)) =
	    treeInfo.firstConstructor(templ.body);
	  val outercontext = context.outer.outer;
          supertpt = gen.TypeTree(
            newTyper(context.outer.outer.makeNewScope(constr, context.outer.outer.owner))
              .completeSuperType(
                supertpt,
                tparams,
                vparamss map (.map(.duplicate.asInstanceOf[ValDef])),
                superargs map (.duplicate))) setPos supertpt.pos;
        }
      }
      List.mapConserve(supertpt :: mixins)(tpt => checkNoEscaping.privates(context.owner, tpt))
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
	  val psym = parent.tpe.symbol.initialize;
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
      val tparams1 = List.mapConserve(cdef.tparams)(transformAbsTypeDef);
      val tpt1 = checkNoEscaping.privates(clazz.thisSym, transformType(cdef.tpt));
      val impl1 = newTyper(context.make(cdef.impl, clazz, new Scope()))
        .transformTemplate(cdef.impl);
      copy.ClassDef(cdef, cdef.mods, cdef.name, tparams1, tpt1, impl1) setType NoType
    }

    def transformModuleDef(mdef: ModuleDef): Tree = {
      val clazz = mdef.symbol.moduleClass;
      val impl1 = newTyper(context.make(mdef.impl, clazz, new Scope()))
        .transformTemplate(mdef.impl);
      copy.ModuleDef(mdef, mdef.mods, mdef.name, impl1) setType NoType
    }

    def addGetterSetter(stat: Tree): List[Tree] = stat match {
      case vd @ ValDef(mods, _, _, _) if (mods & PRIVATE) == 0 =>
	def setter: DefDef = {
	  val sym = vd.symbol;
	  val setter = sym.owner.info.decls.lookup(nme.SETTER_NAME(sym.name)).suchThat(.hasFlag(ACCESSOR));
          atPos(vd.pos)(gen.DefDef(
            setter, vparamss => gen.Assign(gen.mkRef(vparamss.head.head), gen.mkRef(sym))));
	}
	def getter: DefDef = {
	  val sym = vd.symbol;
	  val getter = sym.owner.info.decls.lookup(sym.name).suchThat(.hasFlag(ACCESSOR));
	  val result = atPos(vd.pos)(gen.DefDef(getter, vparamss => gen.mkRef(sym)));
          checkNoEscaping.privates(result.symbol, result.tpt);
          result
	}
	if ((mods & MUTABLE) != 0) List(stat, getter, setter) else List(stat, getter)
      case _ =>
	List(stat)
    }

    def transformTemplate(templ: Template): Template = {
      templ setSymbol context.owner.newLocalDummy(templ.pos);
      val parents1 = parentTypes(templ);
      val selfType =
        if (context.owner.isAnonymousClass)
          refinedType(context.owner.info.parents, context.owner.owner)
        else context.owner.typeOfThis;
      validateParentClasses(parents1, selfType);
      val body1 = templ.body flatMap addGetterSetter;
      val body2 = transformStats(body1, templ.symbol);
      copy.Template(templ, parents1, body2) setType context.owner.tpe
    }

    def transformValDef(vdef: ValDef): ValDef = {
      val sym = vdef.symbol;
      var tpt1 = checkNoEscaping.privates(sym, transformType(vdef.tpt));
      val rhs1 =
	if (vdef.rhs.isEmpty) vdef.rhs
	else newTyper(context.make(vdef, sym)).transform(vdef.rhs, EXPRmode, tpt1.tpe.deconst);
      copy.ValDef(vdef, vdef.mods, vdef.name, tpt1, rhs1) setType NoType
    }

    def transformDefDef(ddef: DefDef): DefDef = {
      val meth = ddef.symbol;
      reenterTypeParams(ddef.tparams);
      reenterValueParams(ddef.vparamss);
      val tparams1 = List.mapConserve(ddef.tparams)(transformAbsTypeDef);
      val vparamss1 = List.mapConserve(ddef.vparamss)(vparams1 =>
	List.mapConserve(vparams1)(transformValDef));
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
      val tparams1 = List.mapConserve(tdef.tparams)(transformAbsTypeDef);
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
          constrCall :: transformStats(block.stats.tail, context.owner);
        } else {
	  transformStats(block.stats, context.owner)
        }
      val expr1 = transform(block.expr, mode & ~(FUNmode | QUALmode), pt);
      val block1 = copy.Block(block, stats1, expr1) setType expr1.tpe.deconst;
      if (block1.tpe.symbol.isAnonymousClass)
	block1 setType refinedType(block1.tpe.parents, block1.tpe.symbol.owner);
      if (isFullyDefined(pt)) block1 else checkNoEscaping.locals(context.scope, block1)
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
        if (fun.vparams.length <= MaxFunctionArity && sym == FunctionClass(fun.vparams.length) ||
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
        namer.enterSym(vparam);
	vparam.symbol
      }
      val vparams1 = List.mapConserve(fun.vparams)(transformValDef);
      val body1 = transform(fun.body, EXPRmode, respt);
      copy.Function(fun, vparams1, body1)
	setType typeRef(clazz.tpe.prefix, clazz, (vparamSyms map (.tpe)) ::: List(body1.tpe))
    }

    def transformRefinement(stats: List[Tree]): List[Tree] = {
      namer.enterSyms(stats);
      for (val stat <- stats) stat.symbol setFlag OVERRIDE;
      transformStats(stats, NoSymbol);
    }

    def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] =
      List.mapConserve(stats) { stat =>
        if (context.owner.isRefinementClass && !treeInfo.isDeclaration(stat))
	  errorTree(stat, "only declarations allowed here");
	stat match {
	  case imp @ Import(_, _) =>
	    context = context.makeNewImport(imp);
	    EmptyTree
	  case _ =>
	    (if (!stat.isDef && exprOwner != context.owner)
	      newTyper(context.make(stat, exprOwner)) else this).transformExpr(stat)
	}
      }

    private def transform1(tree: Tree, mode: int, pt: Type): Tree = {

      def funmode = mode & stickyModes | FUNmode | POLYmode;

      def transformCases(cases: List[CaseDef], pattp: Type): List[CaseDef] = {
        List.mapConserve(cases)(cdef =>
	  newTyper(context.makeNewScope(tree, context.owner)).transformCase(cdef, pattp, pt))
      }

      def transformTypeApply(fun: Tree, args: List[Tree]): Tree = fun.tpe match {
	case OverloadedType(pre, alts) =>
          inferPolyAlternative(fun, args.length);
          transformTypeApply(fun, args)
        case PolyType(tparams, restpe) if (tparams.length != 0) =>
          if (tparams.length == args.length) {
            val targs = args map (.tpe);
            checkBounds(tree.pos, tparams, targs, "");
	    if (settings.debug.value) System.out.println("type app " + tparams + " => " + targs + " = " + restpe.subst(tparams, targs));//debug
	    copy.TypeApply(tree, fun, args) setType restpe.subst(tparams, targs);
          } else {
            errorTree(tree, "wrong number of type parameters for " + treeSymTypeMsg(fun))
          }
	case ErrorType =>
	  setError(tree)
	case _ =>
	  System.out.println(fun.toString() + " " + args);//debug
          errorTree(tree, treeSymTypeMsg(fun) + " does not take type parameters.");
        }

      def transformApply(fun: Tree, args: List[Tree]): Tree = fun.tpe match {
        case OverloadedType(pre, alts) =>
          val args1 = List.mapConserve(args)(arg =>
            transform(arg, mode & stickyModes, WildcardType));
          inferMethodAlternative(fun, context.undetparams, args1 map (.tpe.deconst), pt);
          transformApply(adapt(fun, funmode, WildcardType), args1);
        case MethodType(formals0, restpe) =>
          val formals = formalTypes(formals0, args.length);
          if (formals.length != args.length) {
	    System.out.println("" + formals.length + " " + args.length);
            errorTree(tree, "wrong number of arguments for " + treeSymTypeMsg(fun))
          } else {
            val tparams = context.undetparams;
            context.undetparams = List();
            if (tparams.isEmpty) {
              val args1 = List.map2(args, formals) ((arg, formal) =>
                transform(arg, mode & stickyModes, formal));
	      def ifPatternSkipFormals(tp: Type) = tp match {
		case MethodType(_, rtp) if ((mode & PATTERNmode) != 0) => rtp
		case _ => tp
	      }
              val tree1 = copy.Apply(tree, fun, args1).setType(ifPatternSkipFormals(restpe));
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
                if (settings.debug.value) System.out.println("infer method inst " + fun + ", tparams = " + tparams + ", args = " + args1.map(.tpe) + ", pt = " + pt + ", lobounds = " + tparams.map(.tpe.bounds.lo));//debug
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
	if (sym == NoSymbol && qual.isTerm && (qual.symbol == null || qual.symbol.isValue)) {
	  val coercion = inferView(qual.pos, qual.tpe, name, true);
	  if (coercion != EmptyTree)
	    return transform(
	      copy.Select(tree, Apply(coercion, List(qual)) setPos qual.pos, name), mode, pt)
	}
        if (sym.info == NoType) {
	  System.out.println(qual);
          System.out.println(qual.tpe.members);//debug
          System.out.println(qual.tpe.member(name));//debug
          errorTree(tree, decode(name) + " is not a member of " + qual.tpe.widen)
        } else {
	  val tree1 = tree match {
	    case Select(_, _) => copy.Select(tree, qual, name)
	    case SelectFromTypeTree(_, _) => copy.SelectFromTypeTree(tree, qual, name);
	  }
	  stabilize(checkAccessible(tree1, sym, qual.tpe, qual), qual.tpe)
	}
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
          pre = cx.enclClass.owner.thisType;
          defEntry = cx.scope.lookupEntry(name);
	  if (defEntry != null)
	    defSym = defEntry.sym
	  else {
            cx = cx.enclClass;
	    defSym = pre.member(name) filter (sym => context.isAccessible(sym, pre, false));
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

        // imported symbols take precedence over external package-owned symbols (hack?)
        if (defSym.tpe != NoType && impSym.tpe != NoType && defSym.isExternal && defSym.owner.isPackageClass)
          defSym = NoSymbol;

	if (defSym.tpe != NoType) {
	  if (impSym.tpe != NoType)
	    ambiguousError(
	      "it is both defined in " + defSym.owner +
	      " and imported subsequently by \n" + imports.head);
	  else if (defSym.owner.isClass && !defSym.owner.isPackageClass && !defSym.isTypeParameter)
	    qual = atPos(tree.pos)(gen.mkQualifier(pre));
          else
            pre = NoPrefix;
	} else {
	  if (impSym.tpe != NoType) {
	    var impSym1 = NoSymbol;
	    var imports1 = imports.tail;
	    def ambiguousImportError = ambiguousError(
	      "it is imported twice in the same scope by\n" + imports.head +  "\nand " + imports1.head);
	    while (!imports1.isEmpty && imports1.head.depth == imports.head.depth) {
	      var impSym1 = imports1.head.importedSymbol(name);
	      if (impSym1 != NoSymbol) {
		if (imports1.head.isExplicitImport(name)) {
		  if (imports.head.isExplicitImport(name)) ambiguousImportError;
		  impSym = impSym1;
		  imports = imports1;
		} else if (!imports.head.isExplicitImport(name)) ambiguousImportError
	      }
	      imports1 = imports1.tail;
	    }
	    defSym = impSym;
	    qual = imports.head.qual;
	    pre = qual.tpe;
	  } else {
            System.out.println(context);//debug
            System.out.println(context.imports);//debug
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
          if (tree.symbol.hasFlag(OVERLOADED) && (mode & FUNmode) == 0) {
            inferExprAlternative(tree, pt)
	  }
          if ((mode & (PATTERNmode | FUNmode)) == PATTERNmode && tree.isTerm) // (2)
            checkStable(tree)
          else if ((mode & (EXPRmode | QUALmode)) == EXPRmode && !tree.symbol.isValue) // (3)
            errorTree(tree, tree.symbol.toString() + " is not a value");
          else if (tree.symbol.isStable && pre.isStable && tree.tpe.symbol != ByNameParamClass &&
                   (pt.isStable  || (mode & QUALmode) != 0 || tree.symbol.isModule)) // (4)
            tree.setType(singleType(pre, tree.symbol))
          else
            tree
      }

      // begin transform1
      val sym: Symbol = tree.symbol;
      if (sym != null) sym.initialize;
      //if (settings.debug.value && tree.isDef) global.log("transforming definition of " + sym);//DEBUG
      tree match {
        case PackageDef(name, stats) =>
          val stats1 = newTyper(context.make(tree, sym.moduleClass, sym.info.decls))
            .transformStats(stats, NoSymbol);
          copy.PackageDef(tree, name, stats1) setType NoType

        case cdef @ ClassDef(_, _, _, _, _) =>
          newTyper(context.makeNewScope(tree, sym)).transformClassDef(cdef)

        case mdef @ ModuleDef(_, _, _) =>
          transformModuleDef(mdef)

        case vdef @ ValDef(_, _, _, _) =>
          transformValDef(vdef)

        case ddef @ DefDef(_, _, _, _, _, _) =>
          newTyper(context.makeNewScope(tree, sym)).transformDefDef(ddef)

        case tdef @ AbsTypeDef(_, _, _, _) =>
          transformAbsTypeDef(tdef)

        case tdef @ AliasTypeDef(_, _, _, _) =>
          newTyper(context.makeNewScope(tree, sym)).transformAliasTypeDef(tdef)

        case ldef @ LabelDef(_, List(), _) =>
          newTyper(context.makeNewScope(tree, context.owner)).transformLabelDef(ldef)

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
          newTyper(context.makeNewScope(tree, context.owner))
            .transformBlock(block, mode, pt)

        case Sequence(elems) =>
	  val elems1 = List.mapConserve(elems)(elem => transform(elem, mode, pt));
          copy.Sequence(tree, elems1) setType pt

        case Alternative(alts) =>
	  val alts1 = List.mapConserve(alts)(alt => transform(alt, mode, pt));
          copy.Alternative(tree, alts1) setType pt

        case Bind(name, body) =>
          val body1 = transform(body, mode, pt);
          val vble = context.owner.newValue(tree.pos, name).setInfo(
            if (treeInfo.isSequenceValued(body)) seqType(pt) else body1.tpe);
	  //todo: check whether we can always use body1.tpe
          namer.enterInScope(vble);
          copy.Bind(tree, name, body1) setSymbol vble setType pt

        case fun @ Function(_, _) =>
          newTyper(context.makeNewScope(tree, context.owner))
            .transformFunction(fun, mode, pt)

        case Assign(lhs, rhs) =>
          def isGetter(sym: Symbol) = sym.info match {
            case PolyType(List(), _) => sym.owner.isClass && !sym.isStable
            case _ => false
          }
          val lhs1 = transformExpr(lhs);
          val varsym = lhs1.symbol;
          if (varsym != null && isGetter(varsym)) {
            lhs1 match {
              case Select(qual, name) =>
                transform(
		  Apply(
		    Select(qual, nme.SETTER_NAME(name)) setPos lhs.pos,
		    List(rhs)), mode, pt) setPos tree.pos
            }
          } else if (varsym != null && varsym.isVariable) {
            val rhs1 = transform(rhs, EXPRmode, lhs1.tpe);
            copy.Assign(tree, lhs1, rhs1) setType UnitClass.tpe;
          } else {
            System.out.println("" + lhs1 + " " + varsym + " " + flagsToString(varsym.flags));//debug
            if (!lhs1.tpe.isError) error(tree.pos, "assignment to non-variable ");
            setError(tree)
          }

        case If(cond, thenp, elsep) =>
          val cond1 = transform(cond, EXPRmode, BooleanClass.tpe);
          if (elsep.isEmpty) {
            val thenp1 = transform(thenp, EXPRmode, UnitClass.tpe);
            copy.If(tree, cond1, thenp1, elsep) setType UnitClass.tpe
          } else {
            val thenp1 = transform(thenp, EXPRmode, pt);
            val elsep1 = transform(elsep, EXPRmode, pt);
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
          else if (!context.owner.hasFlag(INITIALIZED))
            errorTree(tree, "method " + context.owner + " has return statement; needs result type")
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
	    context.undetparams = cloneSymbols(tpt1.symbol.unsafeTypeParams);
            tpt1 = TypeTree()
              setPos tpt1.pos
              setType appliedType(tpt1.tpe, context.undetparams map (.tpe));
          }
          copy.New(tree, tpt1).setType(tpt1.tpe)

        case Typed(expr, tpt @ Ident(name)) if (name == nme.WILDCARD_STAR.toTypeName) =>
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
	  val args1 = List.mapConserve(args)(transformType);
	  // do args first in order to maintain conext.undetparams on the function side.
          transformTypeApply(transform(fun, funmode | TAPPmode, WildcardType), args1)

        case Apply(fun, args) =>
          val funpt = if ((mode & PATTERNmode) != 0) pt else WildcardType;
          var fun1 = transform(fun, funmode, funpt);
          // if function is overloaded, filter all alternatives that match
	  // number of arguments and expected result type.
	  if (settings.debug.value) System.out.println("trans app " + fun1 + ":" + fun1.symbol + ":" + fun1.tpe + " " + args);//debug
	  if (fun1.hasSymbol && fun1.symbol.hasFlag(OVERLOADED)) {
	    val argtypes = args map (arg => AllClass.tpe);
	    val pre = fun1.symbol.tpe.prefix;
            val sym = fun1.symbol filter (alt =>
	      isApplicable(context.undetparams, pre.memberType(alt), argtypes, pt));
            if (sym != NoSymbol)
              fun1 = adapt(fun1 setSymbol sym setType pre.memberType(sym), funmode, WildcardType)
          }
	  appcnt = appcnt + 1;
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
	  selcnt = selcnt + 1;
	  assert (name != nme.CONSTRUCTOR || !qual.isInstanceOf[Super], tree);//debug
          var qual1 = transform(qual, EXPRmode | QUALmode | POLYmode, WildcardType);
          if (name.isTypeName) qual1 = checkStable(qual1);
          transformSelect(qual1, name);

        case Ident(name) =>
	  idcnt = idcnt + 1;
	  if (name == nme.WILDCARD && (mode & (PATTERNmode | FUNmode)) == PATTERNmode)
	    tree setType pt
	  else
	    transformIdent(name);

        case Literal(value) =>
          tree setType ConstantType(literalType(value), value)

        case SingletonTypeTree(ref) =>
          val ref1 = checkStable(transform(ref, EXPRmode | QUALmode, AnyRefClass.tpe));
          tree setType ref1.tpe.resultType;

        case SelectFromTypeTree(qual, selector) =>
          tree setType transformSelect(transformType(qual), selector).tpe

        case CompoundTypeTree(templ: Template) =>
          tree setType {
            val parents1 = List.mapConserve(templ.parents)(transformType);
            if (parents1 exists (.tpe.isError)) ErrorType
            else {
              val decls = new Scope();
              val self = refinedType(parents1 map (.tpe), context.enclClass.owner, decls);
              newTyper(context.make(tree, self.symbol, new Scope())).transformRefinement(templ.body);
              self
            }
          }

        case AppliedTypeTree(tpt, args) =>
          val tpt1 = transform(tpt, mode | FUNmode | TAPPmode, WildcardType);
          val tparams = tpt1.tpe.symbol.typeParams;
          val args1 = List.mapConserve(args)(transformType);
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
        if (settings.debug.value) assert(pt != null, tree);//debug
	if (settings.debug.value) System.out.println("transforming " + tree);//debug
        val tree1 = if (tree.tpe != null) tree else transform1(tree, mode, pt);
        if (tree1.isEmpty) tree1 else adapt(tree1, mode, pt)
      } catch {
        case ex: TypeError =>
	  reportTypeError(tree.pos, ex);
	  setError(tree)
	case ex: Throwable =>
	  if (true || settings.debug.value)//!!!
	    System.out.println("exception when tranforming " + tree + ", pt = " + pt);
	  throw(ex)
      }

    def transformExpr(tree: Tree): Tree = transform(tree, EXPRmode, WildcardType);
    def transformQualExpr(tree: Tree): Tree = transform(tree, EXPRmode | QUALmode, WildcardType);
    def transformType(tree: Tree) = transform(tree, TYPEmode, WildcardType);

    /* -- Views --------------------------------------------------------------- */

    private def transformImplicit(pos: int, info: ImplicitInfo, pt: Type): Tree =
      if (isCompatible(info.tpe, pt)) {
	implcnt = implcnt + 1;
	var tree: Tree = EmptyTree;
	try {
	  tree = transform1(Ident(info.name) setPos pos, EXPRmode, pt);
	  if (settings.debug.value) System.out.println("transformed implicit " + tree + ":" + tree.tpe + ", pt = " + pt);//debug
	  if (settings.debug.value) assert(isCompatible(tree.tpe, pt), "bad impl " + info.tpe + " " + tree.tpe + " " + pt);//debug
	  val tree1 = adapt(tree, EXPRmode, pt);
	  if (settings.debug.value) System.out.println("adapted implicit " + tree.symbol + ":" + info.sym);//debug
	  if (info.sym == tree.symbol) tree1 else EmptyTree
	} catch {
	  case ex: TypeError =>
	    if (settings.debug.value)
	      System.out.println(tree.toString() + " is not a valid implicit value because:\n" + ex.getMessage());
	  EmptyTree
	}
      } else EmptyTree;

    private def inferImplicit(pos: int, pt: Type, isView: boolean, reportAmbiguous: boolean): Tree = {
      def isBetter(sym1: Symbol, tpe1: Type, sym2: Symbol, tpe2: Type): boolean =
	(sym1.owner != sym2.owner) && (sym1.owner isSubClass sym2.owner) && (tpe1 matches tpe2);
      val tc = newTyper(context.makeImplicit(reportAmbiguous));
      var iss = context.implicitss;
      var tree: Tree = EmptyTree;
      while (tree == EmptyTree && !iss.isEmpty) {
	var is = iss.head;
	if (settings.debug.value) System.out.println("testing " + is.head.sym + ":" + is.head.tpe);//debug
	iss = iss.tail;
	while (!is.isEmpty) {
	  tree = tc.transformImplicit(pos, is.head, pt);
	  val is0 = is;
	  is = is.tail;
	  if (tree != EmptyTree) {
	    while (!is.isEmpty) {
	      val tree1 = tc.transformImplicit(pos, is.head, pt);
	      if (tree1 != EmptyTree) {
		if (isBetter(is.head.sym, tree1.tpe, is0.head.sym, tree.tpe))
		  tree = tree1
		else if (!isBetter(is0.head.sym, tree.tpe, is.head.sym, tree1.tpe))
		  error(
		    pos,
		    "ambiguous implicit value:\n" +
		    " both " + is0.head.sym + is0.head.sym.locationString + " of type " + tree.tpe +
		    "\n and" + is.head.sym + is.head.sym.locationString + " of type " + tree1.tpe +
		    (if (isView)
		      "\n are possible conversion functions from " +
		      pt.typeArgs(0) + " to " + pt.typeArgs(1)
		     else
		       "\n match expected type " + pt));
	      }
	      is = is.tail
	    }
	  }
	}
      }
      tree
    }

    def applyImplicitArgs(tree: Tree): Tree = tree.tpe match {
      case MethodType(formals, _) =>
        def implicitArg(pt: Type) = {
          val arg = inferImplicit(tree.pos, pt, false, true);
          if (arg != EmptyTree) arg
          else errorTree(tree, "no implicit argument matching parameter type " + pt + " was found.")
        }
      Apply(tree, formals map implicitArg) setPos tree.pos
    }
  }
}

